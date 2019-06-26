#!/usr/bin/env Rscript

'collate

Usage:
  collate.R -b <id> -p <id> [-c <file>] [-d] [-l <str>] [-m] [-n <str>] [-r <str>]  [-t <dir>] [-x]

Options:
  -b <id> --batch_id=<id>             Batch ID.
  -c <file> --config=<file>           Config file specifying how to collate.
  -d --download                       Download CSV files locally before ingesting.
  -p <id> --plate_id=<id>             Plate ID.
  -l <str> --column_as_plate=<str>    Column name in CSV file that should be mapped to Metadata_Plate.
  -m --munge                          Split object CSV into separate CSVs per compartment.
  -n <str> --pipeline_name=<str>      Name of pipeline that produced the output CSVs [default: analysis].
  -r <str> --remote_base_dir=<str>    URL of remote location of base directory.
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
download <- opts[["download"]]
munge <- opts[["munge"]]
overwrite_backend_cache <- opts[["overwrite_backend_cache"]]
pipeline_name <- opts[["pipeline_name"]]
plate_id <- opts[["plate_id"]]
remote_base_dir <- opts[["remote_base_dir"]]
tmpdir <- opts[["tmpdir"]]

stopifnot(!download || !is.null(remote_base_dir))

base_dir = "../.."

input_dir <- file.path(base_dir, "analysis", batch_id, plate_id, pipeline_name)

if (download) {
  if(!dir.exists(input_dir)) {
    dir.create(input_dir, recursive = TRUE)
  }

  remote_input_dir <- file.path(remote_base_dir, "analysis", batch_id, plate_id, pipeline_name)

  sync_str <- paste('aws s3 sync --exclude "*" --include "*.csv"', remote_input_dir, input_dir, sep = " ")

  futile.logger::flog.info("Downloading CSVs...")

  stopifnot(system(sync_str) == 0)

} else {
  stopifnot(dir.exists(input_dir))

}

backend_dir <- file.path(base_dir, "backend", batch_id, plate_id)

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
  ingest_cmd <- paste("cytominer-database",
                      "ingest",
		      input_dir,
                      paste0("sqlite:///", cache_backend_file),
                      "-c",
                      config,
                      ifelse(munge, "--munge", "--no-munge"))

  if (file.exists(cache_backend_file)) {
    file.remove(cache_backend_file)
  }

  # ingest

  futile.logger::flog.info("Ingesting...")
  system(ingest_cmd)
  stopifnot(file.exists(cache_backend_file))

  # add a column `Metadata_Plate` if specified
  # TODO: Generalize this so that new_name:old_name pairs can be specified for any column

  if(!is.null(column_as_plate)) {
    system(paste("sqlite3", cache_backend_file, "'ALTER TABLE Image ADD COLUMN Metadata_Plate TEXT;'"))
    system(paste("sqlite3", cache_backend_file, "'UPDATE image SET Metadata_Plate =", column_as_plate, ";'"))

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

if (download) {
  remote_backend_dir <- file.path(remote_base_dir, "backend", batch_id, plate_id)
  remote_backend_file <- file.path(remote_backend_dir, paste0(plate_id, ".sqlite"))
  remote_aggregated_file <- file.path(remote_backend_dir, paste0(plate_id, ".csv"))

  sync_str <- paste("aws s3 cp", cache_backend_file, remote_backend_file, sep = " ")
  futile.logger::flog.info("Uploading backend_file ...")
  stopifnot(system(sync_str) == 0)
  sync_str <- paste("aws s3 cp", cache_aggregated_file, remote_aggregated_file, sep = " ")

  futile.logger::flog.info("Uploading aggregated_file ...")
  stopifnot(system(sync_str) == 0)

  futile.logger::flog.info("Deleting cache_backend_file ...")
  file.remove(cache_backend_file)

  futile.logger::flog.info("Deleting cache_aggregated_file ...")
  file.remove(cache_aggregated_file)

  futile.logger::flog.info("Deleting input_dir ...")
  unlink(input_dir, recursive = TRUE)

} else {
  move_and_check(cache_backend_file, backend_file)
  move_and_check(cache_aggregated_file, aggregated_file)
}
