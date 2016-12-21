#!/usr/bin/env Rscript

'collapse

Usage:
collapse.R -b <id> -m <id> -o <file> [-f <str>]

Options:
-h --help                         Show this screen.
-b <id> --batch_id=<id>           Batch ID.
-m <id> --plate_map_name=<id>     Plate map name.
-o <file> --output=<file>         Output CSV file.
-f <str> --suffix=<str>           Suffix to append to barcode to select a profile file [default: _normalized_variable_selected.csv]' -> doc

suppressWarnings(suppressMessages(library(docopt)))

suppressWarnings(suppressMessages(library(dplyr)))

suppressWarnings(suppressMessages(library(magrittr)))

opts <- docopt(doc)

batch_id <- opts[["batch_id"]]

output <- opts[["output"]]

plate_map_name <- opts[["plate_map_name"]]

suffix <- opts[["suffix"]]

backend_dir <- paste("../..", "backend", batch_id, sep = "/")

metadata_dir <- paste("../..", "metadata", batch_id, sep = "/")

barcode_platemap <- suppressMessages(readr::read_csv(paste0(metadata_dir, "/barcode_platemap.csv")))

filelist <- barcode_platemap %>%
  filter(Plate_Map_Name == plate_map_name) %>%
  mutate(filename = normalizePath(
    paste0(
      backend_dir,
      "/",
      Assay_Plate_Barcode,
      "/",
      Assay_Plate_Barcode,
      suffix
    )
  ))

profiles <- lapply(filelist$filename, 
    function(filename) {
        if (file.exists(filename)) {
            suppressMessages(readr::read_csv(filename))

        } else {
            tibble::data_frame()

        }
    })

variables <-
  colnames(profiles) %>%
  stringr::str_subset("^Nuclei_|^Cells_|^Cytoplasm_")

aggregated <-
  cytominer::aggregate(
    population = profiles,
    variables = variables,
    strata = c("Metadata_Plate", "Metadata_Well"),
    operation = "mean"
  )

aggregated %>% readr::write_csv(output)
