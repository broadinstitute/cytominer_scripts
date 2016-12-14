#!/usr/bin/env Rscript

'usage: create_variable_selections.R -i <file> -b <id> -r <list> [-s <query>]

options:
 -i <file>, --input=<file> test data on which to perform variable selection operations
 -b <id>, --batch_id=<id> Batch ID
 -r <list> --operations=<list> comma-separated list of operations
 -s <query> --subset=<query> query to create the training data by subsetting' -> doc

suppressWarnings(suppressMessages(library(docopt)))

suppressWarnings(suppressMessages(library(dplyr)))

suppressWarnings(suppressMessages(library(magrittr)))

opts <- docopt(doc)

batch_id <- opts[["batch_id"]]

input <- opts[["input"]]

operations <- opts[["operations"]]

subset <- opts[["subset"]] #"Metadata_broad_sample_type == '''control'''"

operations <- stringr::str_split(operations, ",")[[1]]

for (operation in operations) {
    variable_selections_file <- paste0("../../parameters/", batch_id, "/variable_selection/", operation, ".txt" )

    if (tools::file_ext(input) == "rds") {
        df <- readRDS(input)

    } else if (tools::file_ext(output) == "csv"){
        df <- readr::read_csv(input)

    } else {
        stop(paste0("Unsupported file extension: ", tools::file_ext(input)))

    }

    variables <-
      colnames(df) %>%
      stringr::str_subset("^Nuclei_|^Cells_|^Cytoplasm_")

    futile.logger::flog.info("Dropping variables that have NA in any row...")

    df %<>%
      cytominer::select(variables = variables,
                        operation = "drop_na_columns",
                        cutoff = 0.0)
    variables <-
      colnames(df) %>%
      stringr::str_subset("^Nuclei_|^Cells_|^Cytoplasm_")

    if(!is.null(subset)) {
        futile.logger::flog.info(sprintf("Subsetting using %s", subset))

        sample <- df %>% filter_(subset)

    } else {
        sample <- df 

    }

    futile.logger::flog.info(sprintf("Performing %s...", operation))

    if (operation %in% c("variance_threshold", "correlation_threshold")) {
        df <-
          cytominer::select(
            population = df,
            variables = variables,
            sample = sample,
            operation = operation
          ) %>%
          collect()

    } else if (operation == "replicate_correlation")

    variables <-
      colnames(df) %>%
      stringr::str_subset("^Nuclei_|^Cells_|^Cytoplasm_")

    futile.logger::flog.info(sprintf("Writing variable selections to %s", variable_selections_file))

    data_frame(variable = variables) %>% readr::write_csv(variable_selections_file)
}