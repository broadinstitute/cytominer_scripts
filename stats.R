#!/usr/bin/env Rscript

'stats

Usage:
  stats.R <sqlite_file> -o <file>

Options:
  -h --help                     Show this screen.
  -o <file> --output=<file>     output file.' -> doc

suppressWarnings(suppressMessages(library(docopt)))

suppressWarnings(suppressMessages(library(dplyr)))

suppressWarnings(suppressMessages(library(magrittr)))

suppressWarnings(suppressMessages(library(stringr)))

opts <- docopt(doc)

db <- src_sqlite(path = opts[["sqlite_file"]])

stats <- tbl(src = db, "image") %>%
  select(Metadata_Plate, Metadata_Well, Count_Cells) %>%
  group_by(Metadata_Plate, Metadata_Well) %>%
  summarize(Count_Cells = sum(Count_Cells)) %>%
  collect()

futile.logger::flog.info(paste0("Writing stats to ", opts[["output"]]))

stats %>% readr::write_csv(opts[["output"]])
