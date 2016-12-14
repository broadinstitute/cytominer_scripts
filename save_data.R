#!/usr/bin/env Rscript

'usage: save_data -b <id> -f <pattern> -o -<file>

options:
-b <id>, --batch_id=<id> Batch ID
-f <pattern>, --pattern=<pattern> regular expression (only csv\'s)
-o <file>, --output=<file> output file (either csv or rds)' -> doc

suppressWarnings(suppressMessages(library(docopt)))

suppressWarnings(suppressMessages(library(dplyr)))

suppressWarnings(suppressMessages(library(magrittr)))

opts <- docopt(doc)

batch_id <- opts[["batch_id"]]

pattern <- opts[["pattern"]]

output <- opts[["output"]]

backend_dir <- paste("../..", "backend", batch_id, sep = "/")

file_list <- list.files(backend_dir, 
    pattern = pattern, 
    recursive = T, full.names = T) 

futile.logger::flog.info(sprintf("Reading %d files...", length(file_list)))

df <- file_list %>% 
  lapply(function(x) suppressMessages(readr::read_csv(x))) %>%
  bind_rows() 

futile.logger::flog.info(sprintf("Output contains %d rows.", nrow(df)))

if (tools::file_ext(output) == "rds") {
    saveRDS(df, output)

} else if (tools::file_ext(output) == "csv"){
    readr::write_csv(df, output)

} else {
    stop(paste0("Unsupported file extension: ", tools::file_ext(output)))

}
