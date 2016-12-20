#' Write CellProfiler data to gct file.
#'  
#'  @param x ...
#'  @param path ...
#'  
#'    
#' @return The input \code{x}, invisibly.
#' 
write_gct <- function(x, path) {
  stopifnot(is.data.frame(x))
  path <- normalizePath(path, mustWork = FALSE)
  
  if(file.exists(path)) {
    file.remove(path)
  }
  
  # id is hash of metadata columns
  x %<>% 
    tidyr::unite("id", matches("Metadata_"), remove = F) %>% 
    dplyr::rowwise() %>% 
    dplyr::mutate(id = digest::digest(id)) %>% 
    dplyr::ungroup() 
  
  # change has to an sequential id because some sig tools fail if not
  x %<>% 
    dplyr::mutate(id = paste0("SAMPLE_", row_number(id)))
    
  # write.gctx does not handle Date 
  x %<>% dplyr::mutate_if(is.numeric.Date, as.character)
  
  feature_cols <-
    colnames(x) %>%
    stringr::str_subset("^Nuclei_|^Cells_|^Cytoplasm_")
  
  measurements <- 
    x %>% 
    dplyr::select_(.dots = feature_cols) %>% 
    data.matrix() %>% 
    t()
  
  column_annotations <- 
    x %>% 
    dplyr::select(matches("^id$|^Metadata_"))
  
  row_annotations <- 
    tibble::data_frame(cp_feature_name = row.names(measurements)) %>%
    tidyr::separate(col = "cp_feature_name", 
                    into = c("compartment", "feature_group", "feature_name"), 
                    sep = "_", extra = "merge", remove = FALSE
    )
  
  column_annotations_df <- 
    column_annotations %>% 
    t() %>% 
    as.data.frame() %>% 
    tibble::rownames_to_column() %>% 
    dplyr::mutate(rowname = stringr::str_replace(rowname, "Metadata_", ""))
  
  filler <- row_annotations %>% slice(0)
  filler[1,] <- colnames(filler)
  filler[2:nrow(column_annotations_df),] <- NA
  
  column_annotations_df <- 
    dplyr::bind_cols(column_annotations_df[1],
                     filler,
                     column_annotations_df[2:ncol(column_annotations_df)]
    )
  
  measurements_df <- 
    measurements %>% 
    as.data.frame() %>% 
    tibble::rownames_to_column()
  
  measurements_df <- 
    dplyr::bind_cols(measurements_df[1], 
                     row_annotations, 
                     measurements_df[2:ncol(measurements_df)]
    )
  
  f <- file(path, "w")
  
  writeLines("#1.3", con = f)
  
  writeLines(sprintf("%d\t%d\t%d\t%d", 
                     nrow(measurements), 
                     ncol(measurements), 
                     ncol(row_annotations), 
                     ncol(column_annotations) - 1), 
             con = f)
  
  close(f)
  
  readr::write_tsv(x = column_annotations_df, path = path, append = TRUE, col_names = FALSE)
  
  readr::write_tsv(x = measurements_df, path = path, append = TRUE)
  
  invisible(x)
}
