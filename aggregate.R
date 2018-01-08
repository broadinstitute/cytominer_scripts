#!/usr/bin/env Rscript

'aggregate

Usage:
  aggregate.R <sqlite_file> -o <file>

Options:
  -h --help                     Show this screen.
  -o <file> --output=<file>     per-well aggregated data.' -> doc

suppressWarnings(suppressMessages(library(docopt)))

suppressWarnings(suppressMessages(library(dplyr)))

suppressWarnings(suppressMessages(library(magrittr)))

suppressWarnings(suppressMessages(library(stringr)))

opts <- docopt(doc)

db <- src_sqlite(path = opts[["sqlite_file"]])

image <- tbl(src = db, "image") %>%
  select(TableNumber, ImageNumber, Metadata_Plate, Metadata_Well)

aggregate_objects <- function(compartment) {
  object <- tbl(src = db, compartment)

  object %<>% inner_join(image, by = c("TableNumber", "ImageNumber"))

  # compartment tag converts nuclei to ^Nuclei_
  compartment_tag <-
    str_c("^", str_sub(compartment, 1, 1) %>% str_to_upper(), str_sub(compartment, 2), "_")

  variables <- colnames(object) %>% stringr::str_subset(compartment_tag)

  futile.logger::flog.info(str_c("Started aggregating ", compartment))

  cytominer::aggregate(
    population = object,
    variables = variables,
    strata = c("Metadata_Plate", "Metadata_Well"),
    operation = "mean"
  ) %>% collect()

}

aggregated <-
  aggregate_objects("cells") %>%
  inner_join(aggregate_objects("cytoplasm"),
             by = c("Metadata_Plate", "Metadata_Well")) %>%
  inner_join(aggregate_objects("nuclei"),
             by = c("Metadata_Plate", "Metadata_Well"))

futile.logger::flog.info(paste0("Writing aggregated to ", opts[["output"]]))

aggregated %>% readr::write_csv(opts[["output"]])
