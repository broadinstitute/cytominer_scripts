#!/usr/bin/env Rscript

'audit

Usage: 
  audit.R -b <id> -m <id> -o <file> [-a <int>] [-l <file>] [-s <query>] [-p <var>] [-f <str>] [-r <op>] [-t <dir>]

Options:
  -h --help                          Show this screen.
  -b <id> --batch_id=<id>            Batch ID.
  -m <id> --plate_map_name=<id>      Plate map name.
  -o <file> --output=<file>          Output CSV file (audit summarized across all groups).
  -l <file> --output_detailed=<file> Output CSV file (audit per group).
  -s <query> --subset=<query>        Query to specify the sample for doing the audit.
  -f <str> --suffix=<str>            Suffix to append to barcode to select a profile file [default: _normalized_variable_selected.csv].
  -p <var> --group_by=<var>          Group by column [default: Metadata_Well].
  -r <op> --operation=<op>           Audit operation [default: replicate_quality].
  -t <dir> --tmpdir=<dir>            Temporary directory [default: /tmp].
  -a <int> --accuracy=<int>          Accuracy of null threshold estimation (higher = more robust)  [default: 10].  ' -> doc

suppressWarnings(suppressMessages(library(docopt)))

suppressWarnings(suppressMessages(library(dplyr)))

suppressWarnings(suppressMessages(library(magrittr)))

opts <- docopt(doc)

batch_id <- opts[["batch_id"]]

plate_map_name <- opts[["plate_map_name"]]

operation <- opts[["operation"]]

output <- opts[["output"]]

output_detailed <- opts[["output_detailed"]]

group_by <- stringr::str_split(opts[["group_by"]], ",")[[1]]

subset <- opts[["subset"]] 

suffix <- opts[["suffix"]]

accuracy <- opts[["accuracy"]]

backend_dir <- paste("../..", "backend", batch_id, sep = "/")

metadata_dir <- paste("../..", "metadata", batch_id, sep = "/")

barcode_platemap <- suppressMessages(readr::read_csv(paste0(metadata_dir, "/barcode_platemap.csv")))

filelist <- barcode_platemap %>%
  filter(Plate_Map_Name == plate_map_name) %>% 
  mutate(filename = normalizePath(paste0(backend_dir, "/", Assay_Plate_Barcode, "/", Assay_Plate_Barcode, suffix)))


df <- lapply(filelist$filename, 
    function(filename) {
        if (file.exists(filename)) {
            suppressMessages(readr::read_csv(filename))

        } else {
            tibble::data_frame()

        }
    }) %>% 
  bind_rows()

if (!is.null(subset)) {
  df %<>% filter_(subset)

  futile.logger::flog.info(sprintf("%d profiles retained after filtering.", nrow(df)))

} else {
  futile.logger::flog.info("No filter")

}

variables <-
  colnames(df) %>%
  stringr::str_subset("^Nuclei_|^Cells_|^Cytoplasm_")

metadata <-
  colnames(df) %>%
  stringr::str_subset("^Metadata_")

# group_by is an input variable
stopifnot(group_by %in% metadata)

median_pairwise_correlation <- function(df, variables, group_by) {
  df %>% 
    dplyr::group_by_(.dots = group_by) %>% 
    do(tibble::data_frame(correlation = median(as.dist(cor(t(as.matrix(.[variables])))))))
}

estimate_null_threshold <- function(df, variables, group_by, accuracy) {
  us <- c()
  u <-  c()
  for (i in 1:accuracy) {
    u <- df %>% 
      tidyr::unite_("group_by", group_by) %>%
      mutate(group_by = sample(group_by)) %>%
      median_pairwise_correlation(variables, "group_by") 
    us <- rbind(us,u)
  }
  
  null_threshold <- us %>%
    magrittr::extract2("correlation") %>%
    quantile(0.95, na.rm = TRUE) 
  
  return(null_threshold)
}

set.seed(24)

correlations <- df %>% 
  median_pairwise_correlation(variables, group_by) 

null_threshold <- estimate_null_threshold(df, variables, group_by, accuracy) 


result <- 
  tibble::data_frame(
    plate_map_name = plate_map_name,
    null_threshold = null_threshold,
    fraction_strong = (sum(correlations$correlation > null_threshold) / nrow(correlations)))

knitr::kable(result)

result %>% readr::write_csv(output)

if (!is.null(output_detailed)) {

  knitr::kable(correlations)

  correlations %>% readr::write_csv(output_detailed)

}
#summary(correlations)
