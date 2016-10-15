suppressWarnings(suppressMessages(library(dplyr)))
suppressWarnings(suppressMessages(library(magrittr)))

args = commandArgs(trailingOnly=TRUE)

batch_id <- args[1]

plate_id <- args[2]

metadata_dir <- paste("../..", "metadata", batch_id, sep = "/")

backend_dir <- paste("../..", "backend", batch_id, plate_id, sep = "/")

profiles <- readr::read_csv(paste(backend_dir, paste0(plate_id, ".csv"), sep = "/"))

# read profiles
cnames <- colnames(profiles)

cnames %<>% stringr::str_replace_all("^Image_Metadata", "Metadata")

names(profiles) <- cnames

# read and join metadata map

metadata_map <- readr::read_csv(paste(metadata_dir, "barcode_platemap.csv", sep = "/"))

testthat::expect_true("Assay_Plate_Barcode" %in% colnames(metadata_map))

cnames <- colnames(metadata_map)

cnames %<>% stringr::str_replace_all("^", "Metadata_")

names(metadata_map) <- cnames

profiles %<>% mutate(Metadata_Assay_Plate_Barcode = Metadata_Plate)

profiles %<>% inner_join(metadata_map, by = c("Metadata_Assay_Plate_Barcode"))

# read and join platemap

cpd_map_name <- profiles %>% select(Metadata_Compound_Plate_Map_Name) %>% distinct() %>% extract2("Metadata_Compound_Plate_Map_Name")

testthat::expect_equal(length(cpd_map_name), 1)

platemap <- readr::read_tsv(paste(metadata_dir, "platemap_cpd", paste0(cpd_map_name, ".txt"), sep = "/"))

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
