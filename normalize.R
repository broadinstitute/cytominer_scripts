#!/usr/bin/env Rscript

'normalize

Usage: 
  normalize.R -b <id> -p <id> -s <query> [-r <op>]

Options:
  -b <id> --batch_id=<id>       Batch ID.
  -p <id> --plate_id=<id>       Plate ID.
  -r <op> --operation=<op>      Normalization operation [default: robustize].
  -s <query> --subset=<query>   Query to specify the sample for estimating normalization parameters.' -> doc

suppressWarnings(suppressMessages(library(docopt)))

suppressWarnings(suppressMessages(library(dplyr)))

suppressWarnings(suppressMessages(library(magrittr)))

opts <- docopt(doc)

batch_id <- opts[["batch_id"]]

plate_id <- opts[["plate_id"]]

operation <- opts[["operation"]]

subset <- opts[["subset"]] #"Metadata_broad_sample_type == '''control'''"

backend_dir <- paste("../..", "backend", batch_id, plate_id, sep = "/")

profiles <- suppressMessages(readr::read_csv(paste(backend_dir, paste0(plate_id, "_augmented.csv"), sep = "/")))

load_single_cells <- function(path, metadata) {
    db <- src_sqlite(path = path)

    metadata <- dplyr::copy_to(db, metadata)

    image <- dplyr::tbl(src = db, "image") %>% 
      select(TableNumber, ImageNumber, Image_Metadata_Plate, Image_Metadata_Well) %>%
      rename(Metadata_Plate = Image_Metadata_Plate,
             Metadata_Well = Image_Metadata_Well) %>%
      inner_join(metadata, by = c("Metadata_Plate", "Metadata_Well"))

    object <-
      tbl(src = db, "cells") %>%
      inner_join(dplyr::tbl(src = db, "cytoplasm"),
                 by = c("TableNumber", "ImageNumber", "ObjectNumber")) %>%
      inner_join(dplyr::tbl(src = db, "nuclei"),
                 by = c("TableNumber", "ImageNumber", "ObjectNumber"))

    object %<>% inner_join(image, by = c("TableNumber", "ImageNumber"))

    object
}

metadata <- 
    profiles %>% 
    select(matches("Metadata_")) %>%
    distinct()

single_cells <- 
    load_single_cells(path = paste(backend_dir, paste0(plate_id, ".sqlite"), sep = "/"),
                      metadata = metadata)

sample <- 
    single_cells %>% 
    filter_(subset) %>% 
    collect()

variables <-
  colnames(profiles) %>%
  stringr::str_subset("^Nuclei_|^Cells_|^Cytoplasm_")

normalized <-
  cytominer::normalize(
    population = profiles,
    variables = variables,
    strata =  c("Metadata_Plate"),
    sample = sample,
    operation = operation
  )

normalized %>% readr::write_csv(paste(backend_dir, paste0(plate_id, "_normalized.csv"), sep = "/"))

