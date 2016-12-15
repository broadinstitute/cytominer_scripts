#!/usr/bin/env Rscript

'preselect

Usage:
  preselect.R -b <id> -i <file> -r <list> [-n <n>] [-s <query>] 

options:
  -b <id> --batch_id=<id>          Batch ID
  -i <file> --input=<file>         Test data on which to perform variable selection operations
  -n <n> --replicates=<n>          Number of replicates selected per plate map in <file>
  -r <list> --operations=<list>    Comma-separated list of operations
  -s <query> --subset=<query>      Query to create the training data by subsetting' -> doc

suppressWarnings(suppressMessages(library(docopt)))

suppressWarnings(suppressMessages(library(dplyr)))

suppressWarnings(suppressMessages(library(magrittr)))

opts <- docopt(doc)

batch_id <- opts[["batch_id"]]

input <- opts[["input"]]

operations <- opts[["operations"]]

replicates <- opts[["replicates"]]

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

    } else if (operation == "replicate_correlation") {
        # This is handled differently because there is no direct way yet to do filtering in cytominer
        # TODO: rewrite this after cytominer has an appropriate filtering function for this
        testthat::expect_false(is.null(replicates), info="replicates should be specified when performing replicate_correlation")
        
        feature_replicate_correlations <-
          df %>%
          cytominer::replicate_correlation(
            sample = .,
            variables = variables,
            strata = c("Metadata_Plate_Map_Name", "Metadata_Well"),
            split_by = "Metadata_Plate_Map_Name",
            replicates = replicates
            )

        feature_replicate_correlations %>% 
          readr::write_csv(paste0("../../parameters/", batch_id, "/variable_selection/", operation, ".csv" ))

        variables <- 
          feature_replicate_correlations %>% 
          na.omit() %>%
          filter(median > 0.6) %>% # intentionally hard-coded to avoid confusion
          magrittr::extract2("variable")

        metadata <-
          colnames(df) %>%
          stringr::str_subset("^Metadata_")

        df %<>% 
          select_(.dots = c(metadata, variables))          
    }

    variables <-
      colnames(df) %>%
      stringr::str_subset("^Nuclei_|^Cells_|^Cytoplasm_")

    futile.logger::flog.info(sprintf("Writing variable selections to %s", variable_selections_file))

    data_frame(variable = variables) %>% readr::write_csv(variable_selections_file)
}