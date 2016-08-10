library(dplyr)
library(ggplot2)
library(magrittr)

CYTOMINR_DIR <- "../../software/cytominr_med/"
devtools::load_all(CYTOMINR_DIR)

#batch_id <- args[1] #"2016_04_01_a549_48hr_batch1"
batch_id <- "2016_04_01_a549_48hr_batch1"

metadata_dir <- paste("../..", "metadata", batch_id, sep = "/")

backend_dir <- paste("../..", "backend", batch_id, sep = "/")

per_well_csv <- 
  tibble::data_frame(flist = 
                       list.files(backend_dir, 
                                  pattern = "*augmented.csv", 
                                  recursive = T, full.names = T)
  )

per_well_csv

per_well_csv %<>% 
  rowwise() %>%
  mutate(Assay_Plate_Barcode = 
           stringr::str_split(basename(flist), "_")[[1]][1]
         ) %>%
  ungroup()

per_well_csv %<>% inner_join(
  readr::read_csv(paste(metadata_dir, "barcode_platemap.csv", sep = "/")) %>%
    select(Compound_Plate_Map_Name, Assay_Plate_Barcode)
)

per_well_csv %>% count(Compound_Plate_Map_Name)

df <- lapply(per_well_csv$flist %>% normalizePath(), readr::read_csv) %>% bind_rows()

feature_cols <-
  colnames(df) %>%
  stringr::str_subset("^Nuclei_|^Cells_|^Cytoplasm_")

metadata_cols <-
  colnames(df) %>%
  stringr::str_subset("^Metadata_")

P.prf <- list(metadata = df[metadata_cols],
           featdata = df[feature_cols])

metadata_cols %<>% stringr::str_replace("Metadata_", "")

names(P.prf$metadata) <- metadata_cols


P.prf$metadata$xid <- sapply(tidyr::unite_(P.prf$metadata,
                                     "xid",
                                     from = names(P.prf$metadata))$xid,
                       digest::digest, USE.NAMES = F)
P.prf$featdata$xid <- P.prf$metadata$xid


P.prf$metadata %<>%
  mutate(WellRow = factor(toupper(substr(Well, 1, 1)), 
                          levels = rev(LETTERS[seq(from = 1, to = 16)]),
                          ordered = T)
  ) %>%
  mutate(WellCol = factor(as.integer(substr(Well, 2, 3)), 
                          levels = 1:24, 
                          ordered = T)) 

class(P.prf) <- "profile.data"


# p <-
#   ggplot(full(P.prf), 
#          aes(WellCol, WellRow, fill = Nuclei_Intensity_IntegratedIntensity_DNA)) + 
#   geom_tile() + 
#   facet_wrap(Compound_Plate_Map_Name ~ Plate) + 
#   coord_equal()
# p

scale.robust <- function(x) {
  mad.x <- mad(x, na.rm = T)
  if (mad.x == 0) {
    return(x*NA)
  } else {
    xn <- (x - median(x, na.rm = T))  / (mad.x)
    return(xn)
  }
}

feat_cols <- names(feat(P.prf))
meta_cols <- names(meta(P.prf))

data.1 <- 
  full(P.prf, keep_xid = T) %>% 
  group_by(Plate) %>% 
  mutate_each_(funs(scale.robust), vars = feat_cols) %>% 
  ungroup()
data.1 <- as.data.frame(data.1)
P.prf$metadata <- data.1[c(meta_cols, "xid")]
P.prf$featdata <- data.1[c(feat_cols, "xid")]


good_feats <- 
  plyr::colwise(sd)(feat(P.prf)) %>% 
  as.data.frame() %>% 
  tidyr::gather(feat, value) %>% 
  filter(between(value, 0.6, 1.6))

P.prf$featdata <- 
  bind_cols(
    P.prf$featdata %>% select(xid),
    feat(P.prf) %>% select(one_of(good_feats$feat))
  )

local({
  remove_feats <- caret::findCorrelation(cor(feat(P.prf)), cutoff = 0.9)
  remove_feats <- names(feat(P.prf))[remove_feats]
  keep_feats <- c(setdiff(names(feat(P.prf)), remove_feats), 'xid')
  P.prf$featdata %<>% select_(.dots = keep_feats) 
  P.prf$featdata <<- P.prf$featdata
})


aggregate_profile <- function(P, meta_l) {
  
  feat_l <- names(feat(P))
  
  D <- 
    full(P) %>%
    group_by_(.dots = meta_l) %>%
    summarise_each_(funs(median), vars = feat_l) %>%
    ungroup()
  
  P.agg <- P
  P.agg$featdata <- D[feat_l]
  P.agg$metadata <- D[meta_l]
  
  P.agg$metadata$xid <- sapply(tidyr::unite_(P.agg$metadata,
                                             "xid",
                                             from = names(P.agg$metadata))$xid,
                               digest::digest, USE.NAMES = F)
  P.agg$featdata$xid <- P.agg$metadata$xid
  P.agg
}

#Save data as gctx - define function

library(rhdf5)
library(GENE.E)
save_profile_as_gctx <- function(P, fname) {
  P$metadata %<>% select(Plate, Well, Compound_Plate_Map_Name, broad_sample, mg_per_ml)

  if(file.exists(fname)) {
    file.remove(fname)
  }
  GENE.E:::write.gctx(
    mdat = P %>% feat() %>%
      as.matrix() %>%
      t(),
    column.annotations = P %>% meta(),
    file = fname
  )
}


df.to.gct <- function(fname, D, factor_cols, feat_cols) {
  
  n.dim = length(feat_cols)
  n.samples = NROW(D)
  #factor_cols_exclude <- c("BatchName", "InfectionCondition", "UserStem")
  #factor_cols <- setdiff(factor_cols, factor_cols_exclude)
  n_offset <- length(factor_cols) - 1
  
  D <- D[,c(factor_cols, feat_cols)]
  if (("Name" %in% names(D)) & ("x_mutation_status" %in% names(D))) {
    D <- D %>% dplyr::rename(XName = Name) 
    D <- D %>% dplyr::rename(Name = x_mutation_status)
    D <- D %>% data.frame()
  }
  
  op <- t(D)
  tmp.fname <- tempfile()
  write.table(x = op, file = tmp.fname, quote = F, col.names = F, sep = "\t")
  f <- file(fname, "w")
  writeLines("#1.3", con = f)
  writeLines(sprintf("%d\t%d\t%d\t%d", n.dim, n.samples, 1, n_offset), con = f)
  writeLines(readLines(tmp.fname), con = f)
  close(f)
  unlink(tmp.fname)
}

df.to.gct( "P_prf.gct", full(P.prf, keep_xid = T), names(feat(P.prf)), c(names(meta(P.prf)), "xid") )

save_profile_as_gctx(P.prf, "P_prf.gctx")

P.prf$metadata$broad_sample[is.na(P.prf$metadata$broad_sample)] <- "DMSO"
P.prf$metadata %<>% mutate(type = ifelse(broad_sample == "DMSO", "ctrl", "trt"))
P.prf$metadata %<>% mutate(mg_per_ml = ifelse(broad_sample == "DMSO", 0, mg_per_ml))

#Save data as gctx
full(P.prf) %>%
  readr::write_csv("P_prf.csv")


# Calculate pairwise correlation
data_id <- digest::digest(P.prf)
P.prf$metadata %<>% mutate(data_id = data_id)

compute_sim <- function(P, tag) {
  
  futile.logger::flog.info("Computing similarity matrix...")
  sim_name <- "pearson"
  sim_l <- cytominr::compute_similarity_within_group(P, 
                                                     grp = c("data_id"), 
                                                     method = sim_name
  )
  futile.logger::flog.info("Similarity matrix is %dx%d...", 
                           dim(smat(sim_l[[1]]))[1], 
                           dim(smat(sim_l[[1]]))[2])
  
  futile.logger::flog.debug("Using only the first similarity matrix")
  testthat::expect_equal(length(sim_l), 1)
  sim_1 <- sim_l[[1]]
  saveRDS(sim_1, sprintf("sim_1_%s.rds", tag))
  rm(list = c("sim_l"))
  
}

P.prf %>% 
  compute_sim( "uncollapsed")

sim_1 <- readRDS("sim_1_uncollapsed.rds")


#Define some functions

fq <- function(t_x, t_y, v_y) {
  function(v_x) {
    d <- data_frame(v_x, v_y)
    names(d) <- c(t_x, t_y)
    d
  }
}

qh <- function(v_x) {
  function(f, eq) {
    cytominr::query(sim_1,
                    equality_join_cols = eq,
                    query_frame = f(v_x),
                    return_all_cols = T,
                    include_sim_name = T) 
  }
}


#Get replicate similarity

data_id <- sim_1$row_meta$data_id[1]

# similarity to replicates
rep_sim <- 
  (function(v_x) qh(v_x)(f = fq(t_x = "data_id.x", t_y = "data_id.y", v_y = data_id), 
                         eq = c("Compound_Plate_Map_Name", "Well", "broad_sample", "mg_per_ml"))) (v_x = data_id)
rep_sim %<>%
  filter(xid.x > xid.y)

# similarity to wells from same plate
non_rep_sim <-
  (function(v_x) qh(v_x)(f = fq(t_x = "data_id.x", t_y = "data_id.y", v_y = data_id),
                         eq = c("Compound_Plate_Map_Name"))) (v_x = data_id)
non_rep_sim %<>%
  filter(xid.x > xid.y) %>%
  filter(broad_sample.x != broad_sample.y) %>%
  filter(Plate.x == Plate.y) 



all_sim <- bind_rows(rep_sim, non_rep_sim)

all_sim %<>% 
  mutate(is_replicate = 
           (Compound_Plate_Map_Name.x == Compound_Plate_Map_Name.y) &
           (Well.x == Well.y)
  )
           

f_signif <- function(sim) {
  thresh <- 
    sim %>% 
    filter(broad_sample.x != broad_sample.y) %>%
    magrittr::extract2("sim_val") %>%
    quantile(., c(.95)) %>%
    magrittr::extract2(1)
  
  message(thresh)
  sim %>%
    mutate(is_signif = sim_val > thresh)
  
}
signif <- 
  all_sim %>%
  split(.$Compound_Plate_Map_Name.x) %>%
  lapply(f_signif) %>%
  bind_rows()

signif_rep_sim <- 
  signif %>% 
  filter(is_replicate == TRUE) %>%
  mutate(is_signif = ifelse(is_signif, "signif_yes", "signif_no")) %>%
  group_by(Compound_Plate_Map_Name.x, type.x, is_signif) %>% 
  tally() %>% 
  tidyr::spread(is_signif, n) %>%
  mutate(signif_ratio = signif_yes / (signif_yes + signif_no)) %>%
  rename(Compound_Plate_Map_Name = Compound_Plate_Map_Name.x, type = type.x)

signif_rep_sim %>%
  knitr::kable(digits = 2)

signif_rep_sim %>%
  readr::write_csv("signif_rep_sim.csv")

p <- 
  ggplot(signif_rep_sim, aes(type, signif_ratio)) + 
  geom_bar(stat="identity") + 
  geom_label(aes(label = sprintf("%d/%d=%.2f", 
                                 signif_yes, signif_yes + signif_no, 
                                 signif_ratio))) +
  ylim(0, 1) + 
  facet_wrap(~ Compound_Plate_Map_Name) + 
  ggtitle("Fraction of replicates significantly more\ncorrelated than non-replicates")
p
ggsave("signif_repsim_null.pdf", p, width = 8, height = 6)




