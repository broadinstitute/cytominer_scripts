#!/usr/bin/env Rscript

'usage: my_prog.R -b <BATCH_ID> -p <BATCH_ID>

options:
 -b <BATCH_ID>, --batch_id=<BATCH_ID> Batch ID
 -p <BATCH_ID>, --plate_id=<PLATE_ID> Plate ID' -> doc

suppressWarnings(suppressMessages(library(docopt)))
# retrieve the command-line arguments
opts <- docopt(doc)

suppressWarnings(suppressMessages(library(dplyr)))
suppressWarnings(suppressMessages(library(magrittr)))


batch_id <- opts["batch_id"]

plate_id <- opts["plate_id"]

metadata_dir <- paste("../..", "metadata", batch_id, sep = "/")

backend_dir <- paste("../..", "backend", batch_id, plate_id, sep = "/")

profiles <- suppressMessages(readr::read_csv(paste(backend_dir, paste0(plate_id, ".csv"), sep = "/")))

# read profiles

cnames <- colnames(profiles)

cnames %<>% stringr::str_replace_all("^Image_Metadata", "Metadata")

names(profiles) <- cnames

# read and join metadata map

metadata_map <- suppressMessages(readr::read_csv(paste(metadata_dir, "barcode_platemap.csv", sep = "/")))

testthat::expect_true("Assay_Plate_Barcode" %in% colnames(metadata_map))

cnames <- colnames(metadata_map)

cnames %<>% stringr::str_replace_all("^", "Metadata_")

names(metadata_map) <- cnames

profiles %<>% mutate(Metadata_Assay_Plate_Barcode = Metadata_Plate)

profiles %<>% inner_join(metadata_map, by = c("Metadata_Assay_Plate_Barcode"))

# read and join platemap

platemap_name <- profiles %>% select(Metadata_Plate_Map_Name) %>% distinct() %>% extract2("Metadata_Plate_Map_Name")

testthat::expect_equal(length(platemap_name), 1)

platemap <- suppressMessages(readr::read_tsv(paste(metadata_dir, "platemap", paste0(platemap_name, ".txt"), sep = "/")))

testthat::expect_true("well_position" %in% colnames(platemap))

cnames <- colnames(platemap)

cnames %<>% stringr::str_replace_all("^", "Metadata_")

names(platemap) <- cnames

profiles %<>% mutate(Metadata_well_position = Metadata_Well)

profiles %<>% inner_join(platemap, by = c("Metadata_well_position"))

profiles_with_metadata <- paste(backend_dir, paste0(plate_id, "_augmented.csv"), sep = "/")

metadata_cols <- stringr::str_subset(names(profiles), "^Metadata_")

feature_cols <- stringr::str_subset(names(profiles), "^Cells_|^Cytoplasm_|^Nuclei_")

all_cols <- c(metadata_cols, feature_cols)

profiles[all_cols] %>% readr::write_csv(profiles_with_metadata)
