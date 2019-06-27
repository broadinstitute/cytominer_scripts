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

aggregate_objects <- function(compartment, sc_type = "none") {
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

  object <- object %>%
    dplyr::as_tibble() %>%
    dplyr::group_by_(.dots = c("TableNumber", "ImageNumber",
                               "ObjectNumber", "Metadata_Plate",
                               "Metadata_Well")) %>%
    dplyr::summarise_at(.funs = 'mean', .vars = variables) %>%
    dplyr::ungroup()

  return(object)
}

aggregate_cols <- c("TableNumber", "ImageNumber", "ObjectNumber",
                    "Metadata_Plate", "Metadata_Well")
sc_objects <-
  aggregate_objects("cells",
                    sc_type = sc_type) %>%
  inner_join(aggregate_objects("cytoplasm"),
             by = aggregate_cols) %>%
  inner_join(aggregate_objects("nuclei"),
             by = aggregate_cols)

futile.logger::flog.info(paste0("Now collapsing by well for ", output_file))

variables <- colnames(sc_objects) %>% stringr::str_subset("^Cells|^Nuclei|^Cytoplasm")

sc_objects <- sc_objects %>%
  dplyr::group_by_(.dots = c("Metadata_Plate", "Metadata_Well")) %>%
  dplyr::summarize_at(.funs = 'mean', .vars = variables) %>%
  dplyr::ungroup()

futile.logger::flog.info(paste0("Writing aggregated to ", output_file))

sc_objects %>% readr::write_csv(output_file)
