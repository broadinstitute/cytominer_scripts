#!/usr/bin/env Rscript

'csv2gct

Usage:
  csv2gct.R <csv_file> -o <file> [-a] [-c <str>] [-p <str>]

Options:
  -h --help                         Show this screen.
  -a --create_row_annotations       Create row annotations.
  -c <str> --channels=<str>         Comma-separated list of image channels.
  -p <str> --feature_regex=<str>    Regular expression to extract features from column names. [default: "^Nuclei_|^Cells_|^Cytoplasm"]
  -o <file> --output=<file>         GCT file.' -> doc

suppressWarnings(suppressMessages(library(docopt)))

suppressWarnings(suppressMessages(library(dplyr)))

suppressWarnings(suppressMessages(library(magrittr)))

opts <- docopt(doc)

csv_file <- opts[["csv_file"]]

channels <- opts[["channels"]]

create_row_annotations <- opts[["create_row_annotations"]]

feature_regex <- opts[["feature_regex"]]

output <- opts[["output"]]

source("write_gct.R")

suppressMessages(readr::read_csv(csv_file)) %>%
  write_gct(path = output,
            channels = channels,
            create_row_annotations = create_row_annotations,
            feature_regex = feature_regex)
