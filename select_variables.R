#!/usr/bin/env Rscript

'usage: select_variables.R -b <id> -p <id> -r <list>

options:
 -b <id>, --batch_id=<id> Batch ID
 -p <id>, --plate_id=<id> Plate ID
 -r <list> --filters=<list> comma-separated list of filters' -> doc

suppressWarnings(suppressMessages(library(docopt)))

suppressWarnings(suppressMessages(library(dplyr)))

suppressWarnings(suppressMessages(library(magrittr)))

opts <- docopt(doc)

batch_id <- opts[["batch_id"]]

plate_id <- opts[["plate_id"]]

filters <- opts[["filters"]]

filters <- stringr::str_split(filters, ",")[[1]]

# read features 

variables_selected <- 
    lapply(filters,
        function (filt) {
            suppressMessages(readr::read_tsv(paste0("../../parameters/", batch_id, "/variable_selection/", filt, ".txt" )))
        }) %>% 
    Reduce(function(df1, df2) dplyr::inner_join(df1, df2, by ="variable"), .) %>%
    magrittr::extract2("variable")

backend_dir <- paste("../..", "backend", batch_id, plate_id, sep = "/")

profiles <- paste(backend_dir, paste0(plate_id, "_augmented.csv"), sep = "/")

profiles_variable_selected <- paste(backend_dir, paste0(plate_id, "_augmented_variable_selected.csv"), sep = "/")

df <- suppressMessages(readr::read_csv(profiles))

variables <-
  colnames(df) %>%
  stringr::str_subset("^Nuclei_|^Cells_|^Cytoplasm_")

metadata <-
  colnames(df) %>%
  stringr::str_subset("^Metadata_")

variables <- intersect(variables, variables_selected)

testthat::expect_gt(length(variables), 0)

df %<>% 
    select_(.dots = c(metadata, variables))

df %>%
    readr::write_csv(profiles_variable_selected)
