library(tidyr)
library(sf)
library(dplyr)
library(purrr)
fd_ethiopia <- read.table("../data/tables/Ethiopia_data_collection_2024.csv", sep=",", dec=".", header=T)
fd_ethiopia_noNA <- fd_ethiopia[complete.cases(fd_ethiopia$Corner1_Lat),]

#### NEW TRY 17. Apr 2025 ####

# coords to polygons:
polygons <- vector("list", nrow(fd_ethiopia_noNA))

for (i in 1:nrow(fd_ethiopia_noNA)) {
  row <- fd_ethiopia_noNA[i, ]
  coords <- matrix(c(
    row$Corner1_Lon, row$Corner1_Lat,
    row$Corner2_Lon, row$Corner2_Lat,
    row$Corner3_Lon, row$Corner3_Lat,
    row$Corner4_Lon, row$Corner4_Lat,
    row$Corner1_Lon, row$Corner1_Lat
  ), ncol = 2, byrow = TRUE)
  
  polygons[[i]] <- st_polygon(list(coords))
}

sf_polygons <- st_sf(geometry = st_sfc(polygons), crs = 4326)
plot(sf_polygons)

fd <- cbind(fd_ethiopia_noNA, sf_polygons)
rm(fd_ethiopia, fd_ethiopia_noNA, polygons, sf_polygons, row, i,coords)

# check land cover type values:
all_polys <- st_read("../data/vector/ethiopia_fielddata.gpkg")
unique(all_polys$Landcover.type)
# ooh god, DATA CLEANING:

# "Sorghum", "Sorghum ", "Surghum" -> "Sorghum"
all_polys$Landcover.type[all_polys$Landcover.type == "Sorghum " | all_polys$Landcover.type == "Surghum"] <- "Sorghum"
unique(all_polys$Landcover.type)

# "Sorghum PPT", "Sorghum  PPT", "Sorgum PPT" -> "Sorghum PPT"
all_polys$Landcover.type[all_polys$Landcover.type == "Sorghum  PPT" | all_polys$Landcover.type == "Sorgum PPT"] <- "Sorghum PPT"
unique(all_polys$Landcover.type)

# "Mung bean", "Meng bean", "Mung beans", "Munge bean" -> "Mung bean"
all_polys$Landcover.type[all_polys$Landcover.type == "Meng bean" | all_polys$Landcover.type == "Munge bean" | all_polys$Landcover.type == "Mung beans" | all_polys$Landcover.type == "Mung beans "] <- "Mung bean"
unique(all_polys$Landcover.type)

# "Maize", "Maize " -> "Maize"
all_polys$Landcover.type <- gsub("\\b(Maize |Maize)\\b", replacement = "Maize", all_polys$Landcover.type)
unique(all_polys$Landcover.type)

# "Trees", "Tree" -> "Tree"
all_polys$Landcover.type <- gsub("\\b(Trees|Tree)\\b", replacement = "Tree", all_polys$Landcover.type)
unique(all_polys$Landcover.type)

# "Fruit farm", "Fruit farm (...)", "Fruit  farm" -> "Fruit farm"
all_polys$Landcover.type[all_polys$Landcover.type == "Fruit  farm" | all_polys$Landcover.type == "Fruit farm( mango, papaya & banana)"] <- "Sorghum PPT"
unique(all_polys$Landcover.type)

# "MaizePPT" -> "Maize PPT"
all_polys$Landcover.type[all_polys$Landcover.type == "MaizePPT"] <- "Maize PPT"
unique(all_polys$Landcover.type)

# "Push-pull " -> "Push-pull"
all_polys$Landcover.type[all_polys$Landcover.type == "Push-pull "] <- "Push-pull"
unique(all_polys$Landcover.type)

# "Home" -> "Built-up" to conform to other classifications
all_polys$Landcover.type[all_polys$Landcover.type == "Home"] <- "Built-up"
unique(all_polys$Landcover.type)

# "River" -> "Water" (probably better naming practice)
all_polys$Landcover.type[all_polys$Landcover.type == "River"] <- "Water"
unique(all_polys$Landcover.type)

# "Sorghum intrcropping with munge bean" -> "Sorghum x Mung bean"
all_polys$Landcover.type[all_polys$Landcover.type == "Sorghum intrcropping with munge bean"] <- "Sorghum x Mung bean"
unique(all_polys$Landcover.type)

# write to ethiopia_fielddata.gpkg, original table remains untouched
#st_write(all_polys, dsn = "../data/vector/ethiopia_fielddata_clean.gpkg", layer = "polygons")

