################### Assign mapunits to LRU ############################

#Nathan Roe  
#2021/11/03 

# Install packages
install.packages(c("sf", "rgeos", "methods", "dplyr", "stringr"))

# Load packages
library(sf); library(rgeos); library(methods); library(dplyr); library(stringr)

## Step 1 - Create a shapefile of map units (farm locations) based on an identifier of the map units (aka farm locations). 
# You may be provided with a list of map units (farm locations) that need to be associated with an LRU (state). 

#Read in the list of map units of interest:
mapunit_list_location <- "C:/Users/Nathan.Roe/Documents/PES/Script_for_distribution/mapunit_list.csv" # replace with your file location
mapunit_list <- read.csv(mapunit_list_location)  

# Upload a shapefile of map units:
mapunit_shapefile_location <- "S:/NRCS/190 ECS Ecological Sciences/190-24 Ecological Sites/PES/Review Process 2022/MLRA18/SSURGO_FY22/mlra18_6abcde.shp" # replace with your file location
mapunit_shapefile <- read_sf(mapunit_shapefile_location)

# Filter the mapunit_shapefile so that it only includes mapunits defined in map_unit_list, our mapunits of interest
mapunit_shapefile_reduced <- mapunit_shapefile %>% filter(MUKEY %in% mapunit_list$mukey)

## Step 2 - Introduce the LRU boundary (aka State of California boundary)
LRU_boundary <- read_sf("C:/Users/Nathan.Roe/Documents/PES/GIS/18XA.shp")

# Ensure coordinate systems match between mapunit_shapefile_reduced and LRU_boundary
st_crs(mapunit_shapefile_reduced) == st_crs(LRU_boundary) # This is FALSE, so I need to transform
LRU_boundary_transformed <- st_transform(LRU_boundary, crs = st_crs(mapunit_shapefile_reduced))

## Step 3 - Create centroids and assemble into data frame
# First we create the centroids
centroids <- st_centroid(mapunit_shapefile_reduced)

# Now, we need to get them into a usable form that we can take averages of.
geometry_df <- as.data.frame(centroids$geometry) # Put x, y coordinates into data frame
geometry_df[,1] <- as.character(geometry_df[,1]) # Change class from list to character
colnames(geometry_df) <- "original" # Change column name from 'geometry' to 'original'
geometry_df$long <- geometry_df$original %>% str_extract(pattern = "(?<=\\().*(?=\\,)") # Create new column that is just the longitude
geometry_df$lat <- geometry_df$original %>% str_extract(pattern = "(?<=[:blank:]).*(?=\\))") # Create new column that is just the latitude
geometry_df_mukey <- cbind(geometry_df, mapunit_shapefile_reduced$MUKEY) %>% rename(MUKEY = `mapunit_shapefile_reduced$MUKEY`) # Add mapunit keys to the geometry_df

## Step 4 - Calculate average lat/long
# Make columns numeric
options(digits = 15) # Increase the number of significant digits in calculations

geometry_df_mukey$long <- as.numeric(geometry_df_mukey$long) # Make longitude numeric instead of character
geometry_df_mukey$lat <- as.numeric(geometry_df_mukey$lat) # Make latitude numeric instead of character

# Group polygons by mapunit and calculate average location. This means that when we calculate average lat/long, 
# it does so within groupings. If we did not do this, it would give us a single average location. Instead,
# we want an average location for each mapunit type.
geometry_df_mukey_grouped <- geometry_df_mukey %>% dplyr::group_by(MUKEY) %>% summarise(long_avg = mean(long), lat_avg = mean(lat))

## Step 5 - Determine which points are in LRU
# Convert from st to sf
geom_sf <- st_as_sf(geometry_df_mukey_grouped, coords = c("long_avg", "lat_avg"), crs = st_crs(mapunit_shapefile_reduced))

# Determine which points are in the LRU
geom_sf_clipped <- st_intersection(geom_sf, LRU_boundary_transformed) # This is the final answer

## Step 6 - QC process
# The final answer is create in geom_sf_clipped. These steps allow you to identify which mapunits were considered in the 
# LRU and which mapunits are considered outside of the LRU. As mentioned in the introduction, averaging can be influenced 
# by outliers, so you could visually assess these determinations to confirm that they make sense. This code will provide 
# you with code that you can copy and past into ArcMap to select mapunit considered within and outside. 

# Determine mapunits removed (outside LRU):
MU_removed <- geom_sf[!geom_sf$MUKEY %in% geom_sf_clipped$MUKEY, "MUKEY"]
MU_removed$mukey_mod <- paste0("'", MU_removed$MUKEY, "'")
MU_removed$append <- paste('MUKEY =', MU_removed$mukey_mod)
foo <- paste0(MU_removed$append, " OR ")
bar <- paste0(foo, collapse = "") # You just need to copy and paste this into ArcMap. Right click layer > Open attribute table. From top left drop down > Select by attribute. Then, paste this code in AND delete the last OR. 

# Determine mapunits retained (within LRU):
MU_remain <- geom_sf_clipped
MU_remain$mukey_mod <- paste0("'", MU_remain$MUKEY, "'")
MU_remain$append <- paste('MUKEY =', MU_remain$mukey_mod)
loo <- paste0(MU_remain$append, " OR ")
far <- paste0(loo, collapse = "") #You just need to copy and paste this into ArcMap. Right click layer > Open attribute table. From top left drop down > Select by attribute. Then, paste this code in AND delete the last OR. 

## Step 7 - Export
write.csv(geom_sf_clipped, 
          "C:/Users/Nathan.Roe/Documents/PES/Script_for_distribution/mapunits_in_LRU.csv", row.names = FALSE)
st_write(geom_sf_clipped, dsn = "C:/Users/Nathan.Roe/Documents/PES/Script_for_distribution/mapunits_in_LRU.shp", append = FALSE)