#!/usr/bin/env Rscript

'csv2gct

Usage: 
  csv2gct.R <csv_file> -o <file>

Options:
  -h --help                     Show this screen.
  -o <file> --output=<file>     GCT file.' -> doc

suppressWarnings(suppressMessages(library(docopt)))

suppressWarnings(suppressMessages(library(dplyr)))

suppressWarnings(suppressMessages(library(magrittr)))

opts <- docopt(doc)

csv_file <- opts[["csv_file"]]

output <- opts[["output"]]

str(opts)
