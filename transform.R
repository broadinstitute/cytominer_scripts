#!/usr/bin/env Rscript

'transform

Usage:
  transform.R -b <id> -p <id> -r <str>
  transform.R <input> -o <file> -r <str>

Options:
  -h --help                        Show this screen.
  -b <id> --batch_id=<id>          Batch ID.
  -p <id> --plate_id=<id>          Plate ID.
  -o <file> --output=<file>        Output file.
  -r <str> --operation=<str>       Transformation operation.' -> doc

suppressWarnings(suppressMessages(library(docopt)))

suppressWarnings(suppressMessages(library(dplyr)))

suppressWarnings(suppressMessages(library(magrittr)))

opts <- docopt(doc)

batch_id <- opts[["batch_id"]]

input <- opts[["input"]]

output <- opts[["output"]]

plate_id <- opts[["plate_id"]]

operation <- opts[["operation"]]

# read features

if(!is.null(batch_id)) {
  # use default file names

  backend_dir <- paste("../..", "backend", batch_id, plate_id, sep = "/")

  profiles <- paste(backend_dir, paste0(plate_id, "_normalized_variable_selected.csv"), sep = "/")

  profiles_transformed <- paste(backend_dir, paste0(plate_id, "_normalized_variable_selected_transformed.csv"), sep = "/")

} else {
  profiles <- input

  profiles_transformed <- output

}

df <- suppressMessages(readr::read_csv(profiles))

variables <-
  df %>%
  select(matches("^Nuclei_|^Cells_|^Cytoplasm_"))

variable_names <- names(variables)

metadata <-
  df %>%
  select(matches("^Metadata_"))

# transform profiles

if(operation == "normalize_quantiles") {
  variables %<>%
    as.matrix() %>%
    t() %>%
    preprocessCore::normalize.quantiles() %>%
    t() %>%
    as_data_frame()

  names(variables) <- variable_names

  df <- bind_cols(metadata, variables)

}

df %>%
  readr::write_csv(profiles_transformed)
