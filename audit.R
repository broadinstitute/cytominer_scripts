#!/usr/bin/env Rscript

'audit

Usage: 
  audit.R -b <id> -m <id> [-s <str>] [-r <op>] [-t <dir>]

Options:
  -h --help                         Show this screen.
  -b <id> --batch_id=<id>           Batch ID.
  -m <id> --plate_map_name=<id>     Plate map name.
  -s <str> --suffix=<str>           Suffix to append to barcode to select a profile file [default: _normalized_variable_selected.csv]
  -r <op> --operation=<op>          Audit operation [default: replicate_quality].
  -t <dir> --tmpdir=<dir>           Temporary directory [default: /tmp]' -> doc

suppressWarnings(suppressMessages(library(docopt)))

suppressWarnings(suppressMessages(library(dplyr)))

suppressWarnings(suppressMessages(library(magrittr)))

opts <- docopt(doc)

str(opts)

batch_id <- opts[["batch_id"]]

plate_map_name <- opts[["plate_map_name"]]

suffix <- opts[["suffix"]]

operation <- opts[["operation"]]

backend_dir <- paste("../..", "backend", batch_id, sep = "/")

metadata_dir <- paste("../..", "metadata", batch_id, sep = "/")

barcode_platemap <- suppressMessages(readr::read_csv(paste0(metadata_dir, "/barcode_platemap.csv")))

filelist <- barcode_platemap %>%
  filter(Plate_Map_Name == plate_map_name) %>% 
  mutate(filename = normalizePath(paste0(backend_dir, "/", Assay_Plate_Barcode, "/", Assay_Plate_Barcode, suffix)))


df <- lapply(filelist$filename, 
    function(filename) {
        if (file.exists(filename)) {
            readr::read_csv(filename)
        } else {
            tibble::data_frame()
        }
    }) %>% 
  bind_rows()


print(dim(df))

print(knitr::kable(filelist))

# PLATES=`csvjoin -c Plate_Map_Name ../../metadata/${BATCH_ID}/barcode_platemap.csv <(printf "Plate_Map_Name\n${SET_ID}\n")|csvcut -c Assay_Plate_Barcode|tail -n +2|tr '\n' ' '`


# df <- lapply(per_well_csv$flist %>% normalizePath(), readr::read_csv) %>% bind_rows()



# # rm(list = ls())
# # library(dplyr)
# # library(stringr)
# # library(reshape2)
# # library(htmlTable)
# # source("rep.corr.func.R")

# backend_dir <- paste("../..", "backend", batch_id, sep = "/")

# file_list <- list.files(backend_dir, 
#     pattern = pattern, 
#     recursive = T, full.names = T) 


# x <- readRDS("../results/master/2016-12-13_da3e6bfb/2016_04_01_a549_48hr_batch1_normalized.rds")

# x <- cbind(x, data.frame(Metadata_Treatment = paste(x$Metadata_pert_id, x$Metadata_mg_per_ml, sep = "@")))

# set.seed(24)

# feats <- colnames(x)

# feats <- feats[which(!str_detect(feats, "Metadata"))]

# metadata <- colnames(x)

# metadata <- metadata[which(str_detect(metadata, "Metadata"))]

# thr <- non.rep.cor(list(data = x, feat_col = feats, factor_col = metadata), "Metadata_Treatment", feats)

# u <- rep.cor(list(data = x, feat_col = feats, factor_col = metadata), "Metadata_Treatment", feats)

# strong.trt <- u$Metadata_Treatment[which(u$cr > thr)]

# sprintf("Hit ratio (compound-concentrations) : %f%%", round(length(strong.trt)/NROW(u) * 100))

# strong.cmpd <- lapply(strong.trt, function(x) str_split(x, "@")[[1]][1]) %>% unlist %>% unique

# all.cmpd <- lapply(u$Metadata_Treatment, function(x) str_split(x, "@")[[1]][1]) %>% unlist %>% unique

# sprintf("Hit ratio (compounds) : %f%%", round(length(strong.cmpd)/length(all.cmpd) * 100))

# x <- x %>% dplyr::filter(Metadata_Treatment %in% strong.trt)




