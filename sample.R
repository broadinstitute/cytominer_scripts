#!/usr/bin/env Rscript

'sample

Usage:
  sample -b <id> -f <pattern> [-n <n>] -o <file>

Options:
  -h --help                             Show this screen.
  -b <id> --batch_id=<id>               Batch ID.
  -f <pattern> --pattern=<pattern>      Regular expression - only csv\'s.
  -n <n> --replicates=<n>               Number of replicates to select per plate map.
  -o <file> --output=<file>             Output file - either csv or rds.' -> doc

suppressWarnings(suppressMessages(library(docopt)))

suppressWarnings(suppressMessages(library(dplyr)))

suppressWarnings(suppressMessages(library(magrittr)))

suppressWarnings(suppressMessages(library(readr)))

opts <- docopt(doc)

batch_id <- opts[["batch_id"]]

pattern <- opts[["pattern"]]

replicates <- opts[["replicates"]]

output <- opts[["output"]]

backend_dir <- paste("../..", "backend", batch_id, sep = "/")

metadata_dir <- paste("../..", "metadata", batch_id, sep = "/")

file_list <- list.files(backend_dir, 
    pattern = pattern, 
    recursive = T, full.names = T) 

if (!is.null(replicates)) {

    # get the list of plates that retrieved using the pattern
    plate_list_retrieved <-
        lapply(file_list, function(file) head(tail(stringr::str_split(file, "/")[[1]], 2), 1)) %>%
        unlist() %>%
        data_frame(Assay_Plate_Barcode = .)

    replicates <- as.integer(replicates)

    # create a plate_list based on number of replicates to be selected
    plate_list <- 
        suppressMessages(readr::read_csv(paste(metadata_dir, "barcode_platemap.csv", sep = "/"),
                                        col_types = cols(Assay_Plate_Barcode = col_character(), 
                                                         Plate_Map_Name = col_character()))) %>%
        select(Assay_Plate_Barcode, Plate_Map_Name) %>%
        inner_join(plate_list_retrieved, by = "Assay_Plate_Barcode") %>%
        group_by(Plate_Map_Name) %>% 
        arrange(Assay_Plate_Barcode) %>% 
        mutate(replicate_id = row_number(Assay_Plate_Barcode)) %>% 
        filter(replicate_id %in% seq(replicates)) %>% 
        ungroup() %>% 
        select(Assay_Plate_Barcode) %>%
        magrittr::extract2("Assay_Plate_Barcode")

    # filter file_list based on plate_list
    file_list <- 
        lapply(file_list, 
            function(file) {
                if(length(unlist(lapply(plate_list, function(plate) grep(plate, file))))) {
                    file
                } 
            }
            ) %>%
            unlist()
}

futile.logger::flog.info(sprintf("Reading %d files...:\n%s", length(file_list), paste(file_list, collapse="\n")))

df <- file_list %>% 
  lapply(function(x) suppressMessages(readr::read_csv(x))) %>%
  bind_rows() 

futile.logger::flog.info(sprintf("Output contains %d rows.", nrow(df)))

if (tools::file_ext(output) == "rds") {
    saveRDS(df, output)

} else if (tools::file_ext(output) == "csv"){
    readr::write_csv(df, output)

} else if (tools::file_ext(output) == "feather"){
    feather::write_feather(df, output)

} else {
    stop(paste0("Unsupported file extension: ", tools::file_ext(output)))

}


