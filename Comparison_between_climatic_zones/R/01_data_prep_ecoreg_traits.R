# libraries
library(foreign)
library(data.table)
library(dplyr)

# path to .shp/.dbf files
# needs to be modified by user
data_in <- file.path(getwd(),
                     "Comparison_between_climatic_zones", 
                     "Data")

#### Load data ####

# ecoregions classified to Köppen classifikation 
ecoreg_kg <- read.dbf(file.path(data_in, "Ecoregions.dbf"))

# convert to data.table
setDT(ecoreg_kg)

# trait data
# loaded as .rds (already in data.table format)
# traits_EU <- readRDS(file.path(data_in, "Trait_freshecol_2020_pp_harmonized_ecoregions.rds"))
# loaded as .csv
traits_EU <- read.csv(file.path(data_in, "Trait_freshecol_2020_pp_harmonized_ecoregions.csv"))

# lookup table: 
# lookup <- data.table(ecoregion = c("Tundra", "Taiga"), 
#                    key_col = c("ER21", "ER23"))
lookup <- data.frame(ecoregion = c("Tundra", "Taiga"), 
                   key_col = c("ER21", "ER23"))

# merge with KG classification
# data.table way:
# lookup[ecoreg_kg, `:=`(KoppenClas = i.KoppenClas,
#                        SecKoppenClas = i.SecKC),
#        on = c(ecoregion = "NAME")]
# base way
lookup <- base::merge(
  x = lookup,
  y = ecoreg_kg[, c("KoppenClas", "SecKC", "NAME")],
  by.x = "ecoregion",
  by.y = "NAME",
  all.x = TRUE
)

# get ecoregions columns
er_cols <- grep("ER[0-9]{1,}", names(traits_EU), value = TRUE)

# melt is a data.table function but also works with data.frames 
# in case traits_EU is a data.frame (ignore warning for now)
traits_EU_lf <- melt(traits_EU, measure.vars = er_cols, variable = "key_col")

# subset only to species that have a classification in ecoregions
# data.table way:
# traits_EU_lf_sb <- traits_EU_lf[!is.na(value), ]
# base way:
traits_EU_lf_sb <- traits_EU_lf[!is.na(traits_EU_lf$value), ]

# merge with lookup
# data.table way:
# traits_EU_lf_sb[test, `:=`(ecoregion = i.ecoregion,
#                            KoppenClas = i.KoppenClas,
#                            SecKoppenClas = i.SecKoppenClas),
#                 on = "key_col"]
# base:
traits_EU_lf_sb <- base::merge(x = traits_EU_lf_sb,
                               y = lookup,
                               by = "key_col",
                               all.x = TRUE)

# TODO: transform back to wide format
# base:
# something like this could be the solution, but we need to include all variables
# could also create a subset with just the ecoregions and ID_AQEM and then merge back
# to the trait data
test <- traits_EU_lf_sb[!is.na(traits_EU_lf_sb$ecoregion), ]
reshape2::dcast(test, ID_AQEM + KoppenClas + SecKC ~ key_col)

#Necessary packages tidyverse, foreign

# Loading data
traits_full <- read.csv("traits_koppen_wide.csv")


# Subset 1:  ID_AQEM  KoppenClas  SecKC  Ecoregions
eco_subs <- select(traits_full, ID_AQEM, ecoregion, KoppenClas, SecKC)

# Subset 2: Taxonom. Info + Merkmale + Ecoregions + ID_AQEM
traits_sub <- select(traits_full, -grep("ER", names(traits_full)), -key_col, -KoppenClas,-SecKC, -grep("prim",names(traits_full)))

# Koppen Geiger Classification Data
classifications <- read.table(file="koppen_geiger/Klimaklassifikationen_Koppen_Geiger.txt", sep=",", header=TRUE)

# Loading ecoregions with confidence data
conf <- read.dbf("ecoregions/Ecoregions.dbf", as.is=FALSE)

# Adding confidence data to Subset 1
eco_sub <- base::merge(eco_subs, conf[,c(5,11)], by.x="ecoregion", by.y="NAME")

# Correcting error in Koppen Geiger Classification naming 
eco_sub$KoppenClas[eco_sub$KoppenClas=="Bsk"] <- "BSk"

# Adding classification descriptions to ecoregions
eco_sub2 <- mutate(eco_sub, Classification_Description1 = case_when(eco_sub$KoppenClas == "Dfb" ~ "Cold_nodryseason_warmsummer",
                                                                    eco_sub$KoppenClas == "ET"~"Polar_tundra",
                                                                    eco_sub$KoppenClas == "Dfc"~"Cold_nodryseason_coldsummer",
                                                                    eco_sub$KoppenClas == "Cfb"~"Temperate_nodryseason_warmsummer",
                                                                    eco_sub$KoppenClas == "BSk"~"Arid_steppe_cold",
                                                                    eco_sub$KoppenClas == "Csa"~"Temperate_drysummer_hotsummer"))



# Subsetting acording to climate classification
eco_Dfb <- subset(eco_sub2, eco_sub2$KoppenClas == "Dfb")
eco_ET <- subset(eco_sub2, eco_sub2$KoppenClas == "ET")
eco_Dfc <- subset(eco_sub2, eco_sub2$KoppenClas == "Dfc")
eco_Cfb <- subset(eco_sub2, eco_sub2$KoppenClas == "Cfb")
eco_BSk <- subset(eco_sub2, eco_sub2$KoppenClas == "BSk")
eco_Csa <- subset(eco_sub2, eco_sub2$KoppenClas == "Csa")
