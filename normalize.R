#!/usr/bin/env Rscript

'normalize

Usage:
  normalize.R -b <id> -p <id> -s <query> [-c] [-r <op>] [-t <dir>]

Options:
  -h --help                     Show this screen.
  -b <id> --batch_id=<id>       Batch ID.
  -p <id> --plate_id=<id>       Plate ID.
  -c --sample_single_cell       Use single cell data for sampling, instead of per-well profiles.
  -r <op> --operation=<op>      Normalization operation [default: robustize].
  -s <query> --subset=<query>   Query to specify the sample for estimating normalization parameters.
  -t <dir> --tmpdir=<dir>       Temporary directory [default: /tmp]' -> doc

suppressWarnings(suppressMessages(library(docopt)))

suppressWarnings(suppressMessages(library(dplyr)))

suppressWarnings(suppressMessages(library(magrittr)))

suppressWarnings(suppressMessages(library(stringr)))

opts <- docopt(doc)

batch_id <- opts[["batch_id"]]

sample_single_cell <- opts[["sample_single_cell"]]

plate_id <- opts[["plate_id"]]

operation <- opts[["operation"]]

subset <- opts[["subset"]] # e.g. "Metadata_broad_sample_type == '''control'''"

backend_dir <- paste("../..", "backend", batch_id, plate_id, sep = "/")

# load profiles
profiles <- suppressMessages(readr::read_csv(paste(backend_dir, paste0(plate_id, "_augmented.csv"), sep = "/")))

# prepare to load objects by loading image table
if(sample_single_cell) {
  path <- paste(backend_dir, paste0(plate_id, ".sqlite"), sep = "/")

  # TODO: if file doesn't exist at path then copy it from S3 to a tmpdir
  if (!file.exists(path)) {
    stop(paste0(path, " does not exist"))
  }

  db <- src_sqlite(path = path)

  # get metadata and copy to db
  metadata <-
    profiles %>%
    select(matches("Metadata_")) %>%
    distinct()

  metadata <- copy_to(db, metadata)

  image <- tbl(src = db, "image") %>%
    select(TableNumber, ImageNumber, Image_Metadata_Plate, Image_Metadata_Well) %>%
    rename(Metadata_Plate = Image_Metadata_Plate) %>%
    rename(Metadata_Well = Image_Metadata_Well) %>%
    inner_join(metadata, by = c("Metadata_Plate", "Metadata_Well"))

}


# compartment tag converts nuclei to ^Nuclei_
compartment_tag <- function(compartment) {
  str_c("^", str_sub(compartment, 1, 1) %>% str_to_upper(), str_sub(compartment, 2), "_")

}

load_objects <- function(compartment) {
  tbl(src = db, compartment) %>%
    inner_join(image, by = c("TableNumber", "ImageNumber"))

}

load_profiles <- function(compartment) {
  profiles %>%
    select(matches(str_c("Metadata_", "|", compartment_tag(compartment))))

}

normalize_profiles <- function(compartment) {
  if (sample_single_cell) {
    sample <- load_objects(compartment = compartment)

  } else {
    sample <- load_profiles(compartment = compartment)

  }

  sample %<>%
    filter_(subset) %>%
    collect(n=Inf) %>%
    mutate_at(variables, as.double)

  variables <- colnames(sample) %>% str_subset(compartment_tag(compartment))

  normalized <-
    cytominer::normalize(
      population = load_profiles(compartment = compartment),
      variables = variables,
      strata =  c("Metadata_Plate"),
      sample = sample,
      operation = operation
    )

}

metadata <-
  colnames(profiles) %>% str_subset("^Metadata_")

normalized <-
  normalize_profiles("cells") %>%
  inner_join(normalize_profiles("cytoplasm"),
             by = metadata) %>%
  inner_join(normalize_profiles("nuclei"),
             by = metadata)

normalized %>% readr::write_csv(paste(backend_dir, paste0(plate_id, "_normalized.csv"), sep = "/"))

