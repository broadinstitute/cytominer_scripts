library(magrittr)

args = commandArgs(trailingOnly=TRUE)

db <- dplyr::src_sqlite(path = args[1])

image <- dplyr::tbl(src = db, "image") %>% 
  dplyr::select(TableNumber, ImageNumber, Image_Metadata_Plate, Image_Metadata_Well)

object <-
  dplyr::tbl(src = db, "cells") %>%
  dplyr::inner_join(dplyr::tbl(src = db, "cytoplasm"),
                    by = c("TableNumber", "ImageNumber", "ObjectNumber")) %>%
  dplyr::inner_join(dplyr::tbl(src = db, "nuclei"),
                    by = c("TableNumber", "ImageNumber", "ObjectNumber"))

object %<>% dplyr::inner_join(image, by = c("TableNumber", "ImageNumber"))

feature_cols <-
  colnames(object) %>%
  stringr::str_subset("^Nuclei_|^Cells_|^Cytoplasm_")

aggregated <-
  cytominer::aggregate(
    population = object,
    variables = feature_cols,
    strata = c("Image_Metadata_Plate", "Image_Metadata_Well"),
    operation = "median"
  )

futile.logger::flog.info("Started checking aggregated")

futile.logger::flog.info("Finished checking aggregated")

futile.logger::flog.info("Started collecting aggregated")

aggregated %<>% dplyr::collect()

futile.logger::flog.info("Finished collecting aggregated")

futile.logger::flog.info(paste0("Writing aggregated to ", args[2]))

aggregated %>% readr::write_csv(args[2])
