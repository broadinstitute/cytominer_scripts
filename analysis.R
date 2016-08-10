rm(list = ls())
library(dplyr)
library(magrittr)
library(reshape2)
set.seed(42)

CYTOMINR_DIR <- "../../software/cytominr_old/"
devtools::load_all(CYTOMINR_DIR)

base.dir <- "../../backend/2016_04_01_a549_48hr_batch1"
norm.center <- "mean"
norm.scale <- "sd"
z.score.threshold <- 20      # any feature with std beyond this threshold would be removed 
pca.cut.off <- 1.0
num.null.samples <- 1000
findCorr.cut.off <- 0.90
apply.med.polish <- F

## plate directories to include in the analysis
plates.dir <- c("SQ00015116", "SQ00015117", "SQ00015118")

## reading the profiles in the plates + meta data and put them together 
Px.list <- c()
for (plate.dir in plates.dir) {
  Pxi <- readr::read_csv(sprintf("%s/%s/%s_augmented.csv", base.dir, plate.dir, plate.dir))  
  Px.list <- c(Px.list, list(Pxi))
}
Px <- do.call(rbind, Px.list)
Px$Metadata_broad_sample <- lapply(Px$Metadata_broad_sample, function(x) ifelse(is.na(x), "DMSO", x)) %>% unlist
Px.cols <- colnames(Px)
indx.meta.cols <- which(stringr::str_detect(Px.cols, "Metadata"))
indx.feat.cols <- which(!stringr::str_detect(Px.cols, "Metadata"))
Pf <- list(data = Px, feat_cols = Px.cols[indx.feat.cols], meta_cols = Px.cols[indx.meta.cols])

## normalize the data plate-wise based on DMSO and remove any NA or NaN column produced as a result
Pf.normalized.list <- c()
for (plate in unique(Pf$data$Metadata_Plate)) {
  Pf.plate <- Pf$data %>% dplyr::filter(Metadata_Plate == plate) 
  dmso.sample.data <- Pf.plate %>% dplyr::filter(Metadata_broad_sample == "DMSO") 
  dmso.sample.data <- dmso.sample.data[,Pf$feat_cols]
  nrm.center.param <- apply(dmso.sample.data, 2, norm.center)
  nrm.scale.param <- apply(dmso.sample.data, 2, norm.scale)
  Px.plate.normalized <- scale(Pf.plate[,Pf$feat_cols], center = nrm.center.param, scale = nrm.scale.param)
  Pf.plate.normalized <- cbind(Px.plate.normalized, Pf.plate[,Pf$meta_cols])
  Pf.normalized.list <- c(Pf.normalized.list, list(Pf.plate.normalized))
}
Pf.data.normalized <- do.call(rbind, Pf.normalized.list)
Pf.normalized <- Pf
Pf.normalized$data <- Pf.data.normalized
is.bad.column <- (apply(Pf.normalized$data[,Pf.normalized$feat_cols], 2, function(x) return(any(is.na(x) | is.nan(x)))))
Pf.normalized$feat_cols <- Pf.normalized$feat_cols[!is.bad.column]
Pf.normalized$data <- Pf.normalized$data[, c(Pf.normalized$feat_cols, Pf.normalized$meta_cols)]
feats.to.retain <- which(apply(Pf.normalized$data[,Pf.normalized$feat_cols], 2, sd) <= z.score.threshold) %>% names
Pf.normalized$feat_cols <- feats.to.retain
Pf.normalized$data <- Pf.normalized$data[, c(Pf.normalized$feat_cols, Pf.normalized$meta_cols)]

## feature selection by findCorrelation
feat.cor <- Pf.normalized$data[,Pf.normalized$feat_cols] %>% cor
feats.to.remove <- caret::findCorrelation(x = feat.cor, cutoff = findCorr.cut.off, names = T, exact = F)
Pf.normalized$feat_cols <- setdiff(Pf.normalized$feat_cols, feats.to.remove)
Pf.normalized$data <- Pf.normalized$data[, c(Pf.normalized$feat_cols, Pf.normalized$meta_cols)]

## apply PCA
if (pca.cut.off != 1) {
  Pf.normalized <- c(Pf.normalized, list(factor_cols = Pf.normalized$meta_cols))
  class(Pf.normalized) <- "profile.data"
  nzv_cols <- cytominr::prune.feats.nzv(Pf.normalized, get.nzv=T)
  Pf.pr <- cytominr::prune.feats.nzv.apply(Pf.normalized, nzv_cols)
  model <- cytominr::pca(Pf.pr, pca.cut.off, scale=FALSE, get.proj=T)
  Pf.normalized.pca <- cytominr::pca.apply(Pf.pr, model)
  
  #   pca.obj <- prcomp(Pf.normalized$data[,Pf.normalized$feat_cols], scale. = F)
  #   accum <- 0
  #   for (i in 1:length(pca.obj$sdev)) {
  #     accum <- accum + (pca.obj$sdev[i]^2)/sum((pca.obj$sdev^2))
  #     if (accum > pca.cut.off) {
  #       break
  #     }
  #   }
  #   
  #   Px.normalized.pca <- ((Pf.normalized$data[,Pf.normalized$feat_cols] - outer(rep(1, NROW(Pf.normalized$data)), pca.obj$center, "*")) %>% as.matrix()) %*% pca.obj$rotation[,1:(i-1)]
  #   Pf.normalized.pca <- list(data = cbind(Px.normalized.pca, Pf.normalized$data[, Pf.normalized$meta_cols]), feat_cols = colnames(Px.normalized.pca), meta_cols = Pf.normalized$meta_cols)
} else {
  Pf.normalized.pca <- Pf.normalized
}

## Checking replicate correlation
class(Pf.normalized.pca) <- "profile.data" 
Pf.normalized.pca$data$Metadata_mg_per_ml[which(Pf.normalized.pca$data$Metadata_broad_sample == "DMSO")] <- Pf.normalized.pca$data$Metadata_Well[which(Pf.normalized.pca$data$Metadata_broad_sample == "DMSO")]
Pf.normalized.pca.rep.cor <- cytominr::qualitymeas(Pf.normalized.pca,
                                         metacols = c("Metadata_Well", "Metadata_broad_sample", "Metadata_mg_per_ml"),
                                         cmpfunc = "pearson",
                                         summarize.quantiles = TRUE)

## find the null
wells <- unique(Pf.normalized.pca.rep.cor$data$Metadata_Well)
plates <- unique(Pf.normalized.pca$data$Metadata_Plate)
cmpds <- unique(paste(Pf.normalized.pca$data$Metadata_broad_sample, Pf.normalized.pca$data$Metadata_mg_per_ml))
null.samples <- c()

for (i in 1:num.null.samples) {
  cmpds.sampled <- sample(cmpds, length(plates))
  wells.sampled <- Pf.normalized.pca$data$Metadata_Well[which(paste(Pf.normalized.pca$data$Metadata_broad_sample, Pf.normalized.pca$data$Metadata_mg_per_ml) %in% cmpds.sampled)] %>% unique()
  #wells.sampled <- sample(wells, length(plates))
  
  j <- 1
  batch <- c()
  for (plate in plates) {
    well <- wells.sampled[j]
    j <- j + 1
    batch <- rbind(batch, Pf.normalized.pca$data %>% dplyr::filter(Metadata_Well == well & Metadata_Plate == plate) %>% dplyr::select(one_of(c(Pf.normalized.pca$feat_cols))))
  }
  
  null.samples <- c(null.samples, batch %>% t %>% cor %>% as.dist() %>% as.vector() %>% median)  
}
thr <- quantile(null.samples, 0.95) %>% as.matrix() %>% as.vector()
thr.alt <- quantile(Pf.normalized.pca.rep.cor$data$sim_pearson_q50[which(Pf.normalized.pca.rep.cor$data$Metadata_broad_sample == "DMSO")], 0.95)
Pf.normalized.pca.rep.cor$data <- Pf.normalized.pca.rep.cor$data %>% dplyr::filter(Metadata_broad_sample != "DMSO")

print(sprintf("Threshold for non-rep corr null : %f", thr))
print(sprintf("Threshold for DMSO rep corr null : %f", thr.alt))

print(sprintf("Hit ratio based on replicate corr (null = non-rep corr) : %f", length(which(Pf.normalized.pca.rep.cor$data$sim_pearson_q50 > thr))/NROW(Pf.normalized.pca.rep.cor$data)))

print(sprintf("Hit ratio based on replicate corr (null = DMSO rep corr) : %f", length(which(Pf.normalized.pca.rep.cor$data$sim_pearson_q50 > thr.alt))/NROW(Pf.normalized.pca.rep.cor$data)))

rep.table <- cbind(Pf.normalized.pca.rep.cor$data, data.frame(q50.thr = rep(thr, NROW(Pf.normalized.pca.rep.cor$data)))) %>% dplyr::mutate(has.strong.phenotype = (sim_pearson_q50 > q50.thr))

##########

# DMSO.indx <- which(Pf.normalized.pca$data$Metadata_broad_sample == "DMSO")
# cmpds <- unique(paste(Pf.normalized.pca$data$Metadata_broad_sample[DMSO.indx], Pf.normalized.pca$data$Metadata_mg_per_ml[DMSO.indx]))
# null.samples <- c()
# 
# for (i in 1:num.null.samples) {
#   cmpds.sampled <- sample(cmpds, length(plates))
#   wells.sampled <- Pf.normalized.pca$data$Metadata_Well[which(paste(Pf.normalized.pca$data$Metadata_broad_sample, Pf.normalized.pca$data$Metadata_mg_per_ml) %in% cmpds.sampled)] %>% unique()
#   
#   j <- 1
#   batch <- c()
#   for (plate in plates) {
#     well <- wells.sampled[j]
#     j <- j + 1
#     batch <- rbind(batch, Pf.normalized.pca$data %>% dplyr::filter(Metadata_Well == well & Metadata_Plate == plate) %>% dplyr::select(one_of(c(Pf.normalized.pca$feat_cols))))
#   }
#   
#   null.samples <- c(null.samples, batch %>% t %>% cor %>% as.dist() %>% as.vector() %>% median)  
# }
# q95.DMSO <- quantile(null.samples, 0.95) %>% as.matrix() %>% as.vector()
# 
# print(sprintf("non replicate correlation of DMSO (95th percentile) : %f", q95.DMSO))
# class(Pf.normalized.pca) <- "profile.data" 
# Pf.normalized.pca.rep.cor <- qualitymeas(Pf.normalized.pca,
#                                          metacols = c("Metadata_Well", "Metadata_broad_sample", "Metadata_mg_per_ml"),
#                                          cmpfunc = "pearson",
#                                          summarize.quantiles = TRUE)
# 
# print(sprintf("Hit ratio of DMSO based on the above threshold %f", length(which(Pf.normalized.pca.rep.cor$data$sim_pearson_q50 > q95.DMSO & Pf.normalized.pca.rep.cor$data$Metadata_broad_sample == "DMSO"))/length(which(Pf.normalized.pca.rep.cor$data$Metadata_broad_sample == "DMSO"))))

write.csv(rep.table, "rep.table.csv")

