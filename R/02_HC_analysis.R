# ________________________________________________________
# Analysis HC with approrpiate distance 
# for fuzzy coded traits 
# TODO: Analyse per Order
# ________________________________________________________

# TODO: Try using chord distance with blocks 
# from prep.fuzzy.var()
dist_mat <- daisy(x = data[,-grep("order|family", names(data))])

# ________________________________________________________
#### Optimal number of groups ####
# Using the gap statstic
# ________________________________________________________

set.seed(1234)
gap <- clusGap(x = as.matrix(dist_mat), 
               FUN = mycluster_hc, 
               K.max = 18, 
               B = 500)
# plot(gap)
# determines location of maximum 
optimal_nog <- maxSE(gap$Tab[, "gap"], 
                     gap$Tab[, "SE.sim"],
                     method="Tibs2001SEmax")

# metric:
# gap <- clusGap(x = as.matrix(gowdis_metric), FUN = mycluster_hc, K.max = 10, B = 500)
# # determines location of maximum 
# maxSE(gap$Tab[, "gap"], gap$Tab[, "SE.sim"], method="Tibs2001SEmax")

# ________________________________________________________
#### Hierarchical clustering & Visualization ####
# TODO: test the influence of ward.D, ward.D2 and
# other, see also Everitt and
# https://cran.r-project.org/web/packages/dendextend/vignettes/Cluster_Analysis.html#animals---attributes-of-animals
# ________________________________________________________
hc_taxa <- hclust(dist_mat, method = "ward.D")

# get labels of dendrogram
# merge the according order information to the labels
dend_label <- hc_taxa %>% as.dendrogram() %>% labels()
dend_label <-
  merge(data.frame(label = dend_label),
        data[, c("order", "family")],
        by.x = "label",
        by.y = "family")

# Dendrogram with order as labels
# TODO: Specify region in automated case
png(
  file = file.path(
    data_out,
    "Graphs",
    paste0("Dendrogram_", sub("\\.rds", "", dataset), ".png")
  ),
  width = 1100,
  height = 1300,
  res = 100
)
hc_taxa %>% as.dendrogram() %>%
  color_branches(k = optimal_nog) %>%
  hang.dendrogram(hang_height = 0.005) %>%
  set("labels_cex", 0.75) %>%
  dendextend::ladderize() %>%
  set("labels", dend_label$order) %>%
  plot(horiz = TRUE,
       main = paste("Dendrogram taxa (orders)", sub("\\.rds", "", dataset)))
dev.off()

# Dendrogram with families as labels
png(
  file = file.path(
    data_out,
    "Graphs",
    paste0("Dendrogram_family_", sub("\\.rds", "", dataset), ".png")
  ),
  width = 1100,
  height = 1300,
  res = 100
)
hc_taxa %>% as.dendrogram() %>%
  color_branches(k = optimal_nog) %>%
  hang.dendrogram(hang_height = 0.005) %>%
  set("labels_cex", 0.75) %>%
  dendextend::ladderize() %>%
  plot(horiz = TRUE,
       main = paste("Dendrogram taxa (families)", sub("\\.rds", "", dataset)))
dev.off()

# ________________________________________________________
#### Save clustered groups for further analysis ####
# TODO: Edit during automation
# ________________________________________________________

# get groups
dend_taxa <- as.dendrogram(hc_taxa)

# grouping of taxa
data_cluster[[dataset]] <-
  data.table(
    taxa = names(cutree(dend_taxa, k = optimal_nog)),
    groups = cutree(dend_taxa, k = optimal_nog)
  )

# merge back order information
data_cluster[[dataset]] <-
  merge(data_cluster[[dataset]], data[, c("order", "family")],
        by.x = "taxa", by.y = "family")

# ________________________________________________________________
#### Display traits as dendrogram & trait profiles as heatmap ####
# TODO: this is something to after the analysis
# ________________________________________________________________

# clustering of traits -> transpose trait data
# trait_transpose <- as.data.frame(t(data[, -grep("order|family", names(data))])) 
# # change columns to ordered 
# cols <- names(trait_transpose)
# trait_transpose[, cols] <- lapply(trait_transpose[, cols], as.ordered)
# 
# # transform "binary" columns to numeric 
# col_two_levels <- trait_transpose %>%
#   lapply(levels) %>%
#   lapply(., function(y)
#     length(y)[length(y) == 2]) %>%
#   unlist %>%
#   names
# if(length(col_two_levels) == 1) {
#   trait_transpose[[col_two_levels]] <- sapply(trait_transpose[[col_two_levels]], as.numeric)
# }
# if (length(col_two_levels) > 1) {
#   trait_transpose[, col_two_levels] <- lapply(trait_transpose[, col_two_levels], as.numeric)
# }
# 
# # dist mat
# gower_dist_trait <- gowdis(trait_transpose,
#                            ord = "podani")
# # hc object
# hc_trait <- hclust(gower_dist_trait, method = "ward.D2")
# 
# # plot & safe
# png(
#   file = file.path(data_out, "Graphs", paste0(
#     "Dendrogram_traits_podani_", name_dataset,".png"
#   )),
#   width = 1100,
#   height = 1300,
#   res = 100
# )
# hc_trait %>% as.dendrogram() %>%
#   #  color_branches(k = optimal_nog) %>%
#   hang.dendrogram(hang_height = 0.01) %>%
#   set("labels_cex", 0.75) %>%
#   dendextend::ladderize() %>%
#   plot(horiz = TRUE, main = paste("Dendrogram traits", name_dataset))
# dev.off()

#### heatmap
# does not work properly yet
# some_col_func <- function(n) {
#   colorspace::diverge_hcl(n,
#     h = c(246, 40),
#     c = 96, l = c(65, 90)
#   )
# }
# # par(mar = c(3,3,3,3))
# # library(gplots)
# cols <- names(data[,-1])
# data[, cols] <- lapply(data[,cols], as.numeric)
# 
# gplots::heatmap.2(as.matrix(data[,-1]), 
#                   main = "Macroinvertebrate traits",
#                   srtCol = NULL,
#                   Rowv = dend_taxa_metric,
#                   Colv = dend_trait,
#                   trace="row", hline = NA, tracecol = "darkgrey",         
#                   margins =c(6,3),      
#                   key.xlab = "no / yes",
#                   denscol = "grey",
#                   density.info = "density",
#                   col = some_col_func
# )