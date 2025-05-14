library(sf)
library(ggplot2)

all_polys <- st_read("../data/vector/ethiopia_fielddata_clean.gpkg")

# avg corner coordinate precision:
mean(c(all_polys$Corner1_Accuracy, all_polys$Corner2_Accuracy, all_polys$Corner3_Accuracy, all_polys$Corner4_Accuracy, all_polys$Center.coordinate_precision), na.rm = T)

# avg ground truth position accuracy:
mean(x = all_polys$Ground.truth.point_precision, na.rm = T)

# check if all geometries are valid:
valid <- st_is_valid(all_polys$geom, reason = T)
unique(valid)
valid[valid != "Valid Geometry"] # how many non-valid geometries?
valid[valid == "Edge 1 crosses edge 3"] # how many self-intersects?
valid[valid == "Loop 0: Edge 3 is degenerate (duplicate vertex)"] # how many duplicate vertices?

# make invalid geoms valid:
polys_valid <- st_make_valid(all_polys)
unique(st_is_valid(polys_valid$geom, reason = T))
valid <- st_is_valid(polys_valid$geom, reason = T)
valid[valid != "Valid Geometry"] # how many non-valid geometries?
# -> self intersecting polygons can't be fixed with st_make_valid()
# turn off s2:
sf_use_s2(FALSE)
polys_valid <- st_make_valid(polys_valid) # -> works!
unique(st_is_valid(polys_valid$geom, reason = T))
# write to file once:
#st_write(polys_valid, dsn = "../data/vector/ethiopia_fielddata_clean_validgeoms.gpkg", layer = "polygons")

# calculate area of polygons:
#polys_valid$area <- sf::st_area(polys_valid)
#summary(polys_valid$area)
#boxplot(polys_valid$area)
# -> doesn't help because of overlap

# area per land cover type:
#poly_area <- function(df, id_col, classname){
#  c <- df[df[[id_col]] == classname,]
#  combined <- sf::st_union(c)
#  combined$area <- st_area(combined)
#  return(combined$area)
#}
#sum(polys_valid$area[polys_valid$Landcover.type == "Sorghum"])
# -> 594544.4 [m^2] shit
#poly_area(polys_valid, "Landcover.type", "Sorghum")
# -> 558901.9 [m^2]
# -> have to use st_union() instead of st_combine() then it works 

polys_valid <- st_read("../data/vector/ethiopia_fielddata_clean_validgeoms.gpkg")
cls <- unique(polys_valid$Landcover.type)
a <- c()
for (i in 1:length(cls)){ #works!
  current <- cls[i]
  c <- polys_valid[polys_valid[["Landcover.type"]] == cls[i],] # subset df to one class
  uned <- st_union(c) # union geometries for no overlap
  uned$area <- st_area(uned)
  a <- c(a, uned$area)
  writeLines(paste0(current, " area: ", sum(uned$area)))
}
df <- data.frame(a, cls)
df$a <- round(df$a, 2)
p <- ggplot2::ggplot(data = df, aes(x = cls, y = a)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label=a),position="stack", hjust = -0.2) +
  ylab("Area in m²") +
  xlab("Class") +
  labs(title = "Area distribution of Ethiopia training data") +
  expand_limits(y = max(df$a) * 1.12) +
  coord_flip()
#ggsave("../images/area_training_distribution.png", plot = p, width = 2500, height = 1080, units = "px")

# area in pixels: 1 px == 2.25 m² -> divide area by 2.25
df$a <- round(df$a / 2.25, digits = 0)
p <- ggplot2::ggplot(data = df, aes(x = cls, y = a)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label=a),position="stack", hjust = -0.2) +
  ylab("Area in px") +
  xlab("Class") +
  labs(title = "Area distribution of Ethiopia training data") +
  expand_limits(y = max(df$a) * 1.12) +
  coord_flip()
p
#ggsave("../images/area_training_dist_px.png", p, ,width=2500, heigh=1080, units = "px")
