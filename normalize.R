#!/usr/bin/env Rscript

'normalize

Usage: 
  normalize.R -b <id> -p <id> [-r <op>]

Options:
  -b <id> --batch_id=<id>       Batch ID.
  -p <id> --plate_id=<id>       Plate ID.
  -r <op> --operation=<op>      Normalization operation [default: robustize].' -> doc

suppressWarnings(suppressMessages(library(docopt)))

suppressWarnings(suppressMessages(library(dplyr)))

suppressWarnings(suppressMessages(library(magrittr)))

opts <- docopt(doc)

batch_id <- opts[["batch_id"]]

plate_id <- opts[["plate_id"]]

operation <- opts[["operation"]]

str(opts)

