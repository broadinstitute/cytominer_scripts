#!/usr/bin/env Rscript

'aggregate

Usage:
  aggregate.R -s <sqlite_file> -o <file> [-t <sc_type>]

Options:
  -h --help                                       Show this screen.
  -s <sqlite_file> --sqlite_file=<sqlite_file>    Location of the sql_lite file.
  -o <file> --output=<file>                       per-well aggregated data.
  -t <sc_type> --sc_type=<sc_type>                which sc_type to focus on [default: none]' -> doc

suppressWarnings(suppressMessages(library(docopt)))

suppressWarnings(suppressMessages(library(dplyr)))

suppressWarnings(suppressMessages(library(magrittr)))

suppressWarnings(suppressMessages(library(stringr)))

opts <- docopt(doc)

sql_file <- opts[["sqlite_file"]]

output_file <- opts[["output"]]

sc_type <- opts[["sc_type"]]

db <- src_sqlite(path = sql_file)

image <- tbl(src = db, "image") %>%
  select(TableNumber, ImageNumber, Metadata_Plate, Metadata_Well)

# If sc_type is specified, group by different columns
if (sc_type == "none") {
  strata_cols <- c("Metadata_Plate", "Metadata_Well")
} else {
  strata_cols <- c("TableNumber", "ImageNumber", "ObjectNumber", "Metadata_Plate",
                   "Metadata_Well")
}

aggregate_objects <- function(compartment,
                              strata_cols = c("Metadata_Plate", "Metadata_Well"),
                              sc_type = "none") {
  object <- tbl(src = db, compartment)

  object %<>% inner_join(image, by = c("TableNumber", "ImageNumber"))

  if (sc_type == "isolated") {
    object <- object %>% dplyr::filter(Cells_Neighbors_NumberOfNeighbors_Adjacent == 0)
  } else if (sc_type == "colony") {
    object <- object %>% dplyr::filter(Cells_Neighbors_NumberOfNeighbors_Adjacent >= 4)
  }

  # compartment tag converts nuclei to ^Nuclei_
  compartment_tag <-
    str_c("^", str_sub(compartment, 1, 1) %>% str_to_upper(), str_sub(compartment, 2), "_")

  variables <- colnames(object) %>% stringr::str_subset(compartment_tag)
  futile.logger::flog.info(str_c("Started aggregating ", compartment))

  cytominer::aggregate(
    population = object,
    variables = variables,
    strata = strata_cols,
    operation = "mean"
  ) %>% collect()

}

aggregated <-
  aggregate_objects(compartment = "cells",
                    strata_cols = strata_cols,
                    sc_type = sc_type) %>%
  inner_join(
    aggregate_objects(compartment = "cytoplasm",
                      strata_cols = strata_cols,
                      sc_type = sc_type),
            by = strata_cols
          ) %>%
  inner_join(
    aggregate_objects(compartment = "nuclei",
                      strata_cols = strata_cols,
                      sc_type = sc_type),
             by = strata_cols
           )

if (sc_type != "none") {
  futile.logger::flog.info(
    paste0("Now collapsing single cell by well for ", output_file)
  )

  aggregated <- aggregated %>%
    dplyr::group_by_(.dots = c("Metadata_Plate", "Metadata_Well")) %>%
    dplyr::summarize_at(.funs = 'mean', .vars = variables) %>%
    dplyr::ungroup()
}

futile.logger::flog.info(paste0("Writing aggregated to ", output_file))

aggregated %>% readr::write_csv(output_file)
