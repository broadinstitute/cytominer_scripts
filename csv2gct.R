#!/usr/bin/env Rscript

'csv2gct

Usage: 
  csv2gct.R <csv_file> -o <file> [-c <str>]

Options:
  -h --help                     Show this screen.
  -c <str> --channels=<str>     Comma-separated list of image channels.
  -o <file> --output=<file>     GCT file.' -> doc

suppressWarnings(suppressMessages(library(docopt)))

suppressWarnings(suppressMessages(library(dplyr)))

suppressWarnings(suppressMessages(library(magrittr)))

opts <- docopt(doc)

csv_file <- opts[["csv_file"]]

channels <- opts[["channels"]]

output <- opts[["output"]]

source("write_gct.R")

suppressMessages(readr::read_csv(csv_file)) %>%  
  write_gct(path = output,
            channels = channels)
