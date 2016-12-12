#!/usr/bin/env Rscript

'usage: join_metadata.R -b <id> -p <id>

options:
 -b <id>, --batch_id=<id> Batch ID
 -p <id>, --plate_id=<id> Plate ID' -> doc

suppressWarnings(suppressMessages(library(docopt)))

suppressWarnings(suppressMessages(library(dplyr)))

suppressWarnings(suppressMessages(library(magrittr)))

opts <- docopt(doc)

batch_id <- opts["batch_id"]

plate_id <- opts["plate_id"]

metadata_dir <- paste("../..", "metadata", batch_id, sep = "/")

backend_dir <- paste("../..", "backend", batch_id, plate_id, sep = "/")

# read profiles and rename column names 

profiles <- suppressMessages(readr::read_csv(paste(backend_dir, paste0(plate_id, ".csv"), sep = "/")))

profiles %<>% setNames(names(profiles) %>% stringr::str_replace_all("^Image_Metadata", "Metadata"))

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

# save 

profiles_with_metadata <- paste(backend_dir, paste0(plate_id, "_augmented.csv"), sep = "/")

metadata_cols <- stringr::str_subset(names(profiles), "^Metadata_")

feature_cols <- stringr::str_subset(names(profiles), "^Cells_|^Cytoplasm_|^Nuclei_")

all_cols <- c(metadata_cols, feature_cols)

profiles[all_cols] %>% readr::write_csv(profiles_with_metadata)
