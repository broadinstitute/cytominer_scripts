#!/usr/bin/env Rscript

'collate

Usage:
  collate.R -b <id> -p <id> [-c <file>] [-l <str>] [-m] [-n <str>] [-t <dir>] [-x]

Options:
  -b <id> --batch_id=<id>             Batch ID.
  -c <file> --config=<file>           Config file specifying how to collate.
  -p <id> --plate_id=<id>             Plate ID.
  -l <str> --column_as_plate=<str>    Column name in CSV file that should be mapped to Image_Metadata_Plate.
  -m --munge                          Split object CSV into separate CSVs per compartment.
  -n <str> --pipeline_name=<str>      Name of pipeline that produced the output CSVs [default: analysis].
  -t <dir> --tmpdir=<dir>             Temporary directory [default: /tmp].
  -x --overwrite_backend_cache        Overwrite backend cache file if it exists.
  -h --help                           Show this screen.' -> doc

suppressWarnings(suppressMessages(library(docopt)))

suppressWarnings(suppressMessages(library(dplyr)))

suppressWarnings(suppressMessages(library(magrittr)))

suppressWarnings(suppressMessages(library(stringr)))

opts <- docopt(doc)

batch_id <- opts[["batch_id"]]

column_as_plate <- opts[["column_as_plate"]]

config <- opts[["config"]]

munge <- opts[["munge"]]

overwrite_backend_cache <- opts[["overwrite_backend_cache"]]

pipeline_name <- opts[["pipeline_name"]]

plate_id <- opts[["plate_id"]]

tmpdir <- opts[["tmpdir"]]

# str(opts)

input_dir <- file.path("../..", "analysis", batch_id, plate_id, pipeline_name)

stopifnot(dir.exists(input_dir))

backend_dir <- file.path("../..", "backend", batch_id, plate_id)

cache_backend_dir <- file.path(tmpdir, "backend", batch_id, plate_id)

if (!dir.exists(backend_dir)) { dir.create(backend_dir, recursive = TRUE) }

if (!dir.exists(cache_backend_dir)) { dir.create(cache_backend_dir, recursive = TRUE) }

backend_dir %<>% normalizePath()

cache_backend_dir %<>% normalizePath()

cache_backend_file <- file.path(cache_backend_dir, paste0(plate_id, ".sqlite"))

cache_aggregated_file <- file.path(cache_backend_dir, paste0(plate_id, ".csv"))

backend_file <- file.path(backend_dir, paste0(plate_id, ".sqlite"))

aggregated_file <- file.path(backend_dir, paste0(plate_id, ".csv"))

if (file.exists(backend_file) & file.exists(aggregated_file)) {
  # nothing to do
  futile.logger::flog.info(paste(backend_file, "and", aggregated_file, "already exist."))

  quit()

}

if (!file.exists(cache_backend_file) | overwrite_backend_cache) {
  ingest_cmd <- paste("ingest", input_dir, "-o", paste0("sqlite:///", cache_backend_file),
      "-c", config, ifelse(munge, "--munge", "--no-munge"))

  if (file.exists(cache_backend_file)) {
    file.remove(cache_backend_file)
  }

  # ingest

  futile.logger::flog.info("Ingesting...")

  system(ingest_cmd)

  stopifnot(file.exists(cache_backend_file))

  # add a column `Image_Metadata_Plate` if specified

  if(!is.null(column_as_plate)) {
    system(paste("sqlite", "ALTER TABLE Image ADD COLUMN Image_Metadata_Plate TEXT;"))

    system(paste("UPDATE image SET Image_Metadata_Plate =", column_as_plate, ";"))

  }

  # create index

  index_cmd <- paste("sqlite3", cache_backend_file, "< indices.sql")

  futile.logger::flog.info("Indexing...")

  system(index_cmd)

}

# create aggregated (even if it already exists)

aggregate_cmd <- paste("./aggregate.R", cache_backend_file, "-o", cache_aggregated_file)

futile.logger::flog.info("Aggregating...")

system(aggregate_cmd)

stopifnot(file.exists(cache_aggregated_file))

move_and_check <- function(src, dst) {
  file.copy(src, dst)

  stopifnot(tools::md5sum(src) == tools::md5sum(dst))

  file.remove(src)

  invisible()

}

futile.logger::flog.info("Moving...")

move_and_check(cache_backend_file, backend_file)

move_and_check(cache_aggregated_file, aggregated_file)

