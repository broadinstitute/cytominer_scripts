#!/usr/bin/env Rscript

'compare_plates

Usage:
compare_plates.R -b <id> -m <id> -o <dir> [-f <str>]

Options:
-h --help                         Show this screen.
-b <id> --batch_id=<id>           Batch ID.
-m <id> --plate_map_name=<id>     Plate map name.
-o <dir> --output=<dir>           Output directory.
-f <str> --suffix=<str>           Suffix to append to barcode to select a profile file [default: _normalized_variable_selected.gct]' -> doc

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

barcode_platemap <-
  suppressMessages(readr::read_csv(paste0(metadata_dir, "/barcode_platemap.csv")))

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


dir.create(paste0(output, "/", batch_id, "/", "compare_plates"), showWarnings = FALSE, recursive = TRUE)

group_file <-
  paste0(output, "/", batch_id, "/", "compare_plates", "/", plate_map_name, ".grp")

filelist %>% select(filename) %>% readr::write_tsv(group_file, col_names = FALSE)

cmd <-
  sprintf(
    "matlab -nodesktop -nosplash -nojit -nodisplay -r \"cd /cmap/tools/jenkins/job_repos/espresso/cup/dunkin/compare_plates/; compare_plates(\'%s\', \'%s\', \'%s\', '--probe_display_field', 'cp_feature_name', '--sample_field', 'pert_well'); quit\"",
    normalizePath(group_file),
    normalizePath(paste0(output, "/", batch_id, "/", "compare_plates")),
    plate_map_name
  )

system(cmd)
