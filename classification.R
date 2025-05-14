library(terra)
library(sf)
library(caret)
library(ggplot2)

#rlist <- list.files("../data/raster/Robit_images/050169858020_01_P001_MUL/",pattern = ".TIF", full.names = T)
#rsrc <- terra::sprc(rlist)
#mos_rob <- terra::mosaic(rsrc)

#rlist <- list.files("../data/raster/Kemise_images/050169858010_01_P001_MUL/",pattern = ".TIF", full.names = T)
#rsrc <- terra::sprc(rlist)
#mos_kem <- terra::mosaic(rsrc)
#rm(rlist, rsrc)

# write mosaics to file once because mosaicking takes long:

#terra::writeRaster(mos_rob, "../data/raster/Robit_images/050169858020_01_P001_MUL/Robit_P001_MUL_mosaic.tif")
#terra::writeRaster(mos_kem, "../data/raster/Kemise_images/050169858010_01_P001_MUL/Kemise_P001_MUL_mosaic.tif")

mos_rob <- terra::rast("../data/raster/Robit_images/050169858020_01_P001_MUL/Robit_P001_MUL_mosaic.tif")
mos_kem <- terra::rast("../data/raster/Kemise_images/050169858010_01_P001_MUL/Kemise_P001_MUL_mosaic.tif")

# training data (read all polys):

# clip

polys <- st_read("../data/vector/ethiopia_fielddata_clean_validgeoms.gpkg")
polys <- st_transform(polys, 32637)

# remove unnecessary columns:

polys <- polys[,c(1,26,28)]

# separate into Kemise/Robit:

kemiseAOI <- st_transform(st_read("../data/raster/Kemise_images/GIS_FILES/050169858010_01_PRODUCT_SHAPE.shp"), 32637)
robitAOI <- st_transform(st_read("../data/raster/Robit_images/GIS_FILES/050169858020_01_PRODUCT_SHAPE.shp"), 32637)
kemise <- polys[kemiseAOI,]
robit <- polys[robitAOI,]
kemise$ID <- 1:nrow(kemise)
robit$ID <- 1:nrow(robit)

# remove unnecessary data:

rm(kemiseAOI, robitAOI, polys)

extracted_kem <- terra::extract(mos_kem, kemise)
extracted_rob <- terra::extract(mos_rob, robit)

# combine extracted with training data:

merged_kem <- merge(extracted_kem, kemise[, c("Landcover.type", "ID")], by ="ID", all.x = T)
merged_rob <- merge(extracted_rob, robit[, c("Landcover.type", "ID")], by="ID", all.x=T)

# rename columns because the generated ones don't match (needed for rbind)

cnames <- c("ID", "B1_coastal_blue", "B2_blue", "B3_green", "B4_yellow", "B5_red", "B6_red_edge", "B7_nir1", "B8_nir2", "Class", "geom")
colnames(merged_kem) <- cnames
colnames(merged_rob) <- cnames

combined <- rbind(merged_kem, merged_rob)

# remove geom column because it's a list and doesn't work in train():
combined$geom <- NULL
# remove rows with NA:
combined_noNA <- combined[is.na(combined) != TRUE,]
# combined_NAs <- combined[is.na(combined == TRUE),] #> 416 rows invalid
# somehow still doesn't work
combined_noNA <- na.omit(combined_noNA)
combined_noNA <- subset(combined_noNA, select = -ID) # remove ID column

# training data distribution again, more precise:
p <- ggplot(combined_noNA) +
  geom_bar(aes(y = Class), stat = "count") +
  xlab("Number of samples (pixels)") +
  ylab("Land Use class") +
  geom_text(aes(label = after_stat(count)),stack="count", position="stack", hjust = -0.2)
p

ggsave("../images/model_training_data_distribution.png",plot = p, width = 3000, height = 1000, units = "px")

model <- caret::train(Class ~ ., data = combined_noNA, method = "rf",
                      trControl = trainControl(method = "cv", number = 5))

# rename raster bands to match:
bnames <- c("B1_coastal_blue", "B2_blue", "B3_green", "B4_yellow", "B5_red", "B6_red_edge", "B7_nir1", "B8_nir2")
names(mos_kem) <- bnames

# predict:
class_kem <- predict(mos_kem, model, na.rm=T) # na.rm=T probably beacuase of skewed geometry introducing NAs
