# Loading data
genus_occ <- read.csv("Conus/Genus_Occurrences.csv")

# Identifying the different CRS
unique(genus_occ$Horizontal_datum)

# Seperating the different CRS
wgs <- genus_occ[genus_occ$Horizontal_datum == "WGS84",]
nad83 <- genus_occ[genus_occ$Horizontal_datum == "NAD83",]
nad27 <- genus_occ[genus_occ$Horizontal_datum == "NAD27",]

# Transforming data into a spatial object
coords.wgs <- SpatialPoints(wgs[,c("Longitude","Latitude")])
genus.wgs.spatial <- SpatialPointsDataFrame(coords.wgs,wgs)
proj4string(genus.wgs.spatial) <- CRS("+proj=longlat +elips=WGS84")

coords.nad83 <- SpatialPoints(nad83[,c("Longitude","Latitude")])
genus.nad83.spatial <- SpatialPointsDataFrame(coords.nad83,nad83)
proj4string(genus.nad83.spatial) <- CRS("+proj=longlat +elips=NAD83")

coords.nad27 <- SpatialPoints(nad27[,c("Longitude","Latitude")])
genus.nad27.spatial <- SpatialPointsDataFrame(coords.nad27,nad27)
proj4string(genus.nad27.spatial) <- CRS("+proj=longlat +elips=NAD27")

# Reprojecting to WGS84
nad83.to.wgs <- spTransform(genus.nad83.spatial, CRS("+proj=longlat +elips=WGS84"))
nad27.to.wgs <- spTransform(genus.nad27.spatial, CRS("+proj=longlat +ellps=WGS84"))

