#Loading packages ----
options("rgdal_show_exportToProj4_warnings"="none")

library(sf)
library(raster)
library(mapview)

# Import raster elevation data Ordnance Survey projection
pan50m <- raster("gis_data/pan50m.tif")
plot(pan50m)

# You will notice that the default colours are the wrong way round, so we can
# use the terrain.colors option to set low elevation to green, high to brown, 
# with 30 colour categories
plot(pan50m, col=terrain.colors(30))


ll_crs <- CRS("+init=epsg:4326")  # 4326 is the code for latitude longitude
pan50m_ll <- projectRaster(pan50m, crs=ll_crs)
mapview(pan50m_ll)

hs = hillShade(slope = terrain(pan50m, "slope"), aspect = terrain(pan50m, "aspect"))
plot(hs, col = gray(0:100 / 100), legend = FALSE)
# overlay with DEM
plot(pan50m, col = terrain.colors(25), alpha = 0.5, add = TRUE) #if we wanna be fancy and create 

#Creation of contours from raster DTM ----

pan_contours <- rasterToContour(pan50m) %>% st_as_sf()
plot(pan50m)
plot(pan_contours, add=TRUE)

#Add DTM-derived information to site data ----

#Add windfarm data to site 
wind_turbines <- st_read("gis_data/wind_turbines.shp")

print(wind_turbines)

plot(pan50m)
plot(wind_turbines["WF_Name"], add=TRUE)

st_transform(wind_turbines, 4326)

mapview(st_transform(wind_turbines, 4326)) #to look at them in the interactive map

# Calculate slope and Calculate aspect

dem_slope  <- terrain(pan50m, unit="degrees") # defaults to slope
dem_aspect <- terrain(pan50m, opt="aspect", unit="degrees")
plot(dem_slope)
plot(dem_aspect)

#Add slope and aspect values to wind turbines attributes 

wind_turbines$slope <- extract(dem_slope, wind_turbines)
wind_turbines$aspect <- extract(dem_slope, wind_turbines)

#Create a viewshed ----

source("LOS.R")

#Viewshed for western area 

# Convert to latitude-longitude; EPSG code 4326
wind_turbines_ll <- st_transform(wind_turbines, 4326)
mapview(wind_turbines_ll)

west_windfarm <- dplyr::filter(wind_turbines, Turb_ID == "CC7") #turbine at centre-ish of western windfarm

# Change to coarser 500m elevation map for speed
pan500m <- aggregate(pan50m, fact=5) # fact=5 is the number of cells aggregated together

# Extract just the geometry for a single mast, and pass to viewshed function.
# Adding a 5km maximum radius
# Takes 1 to 2 minutes to run viewshed depending on your PC
west_windfarm_geom <- st_geometry(west_windfarm)[[1]]
west_viewshed <- viewshed(dem=pan500m, windfarm=west_windfarm_geom,
                          h1=1.5, h2=49, radius=5000)

# Display results
plot(pan500m)
plot(west_viewshed, add=TRUE, legend=FALSE, col="red")

#Viewshed for eastern area

# Get the OM7 turbine
east_windfarm <- dplyr::filter(wind_turbines, Turb_ID == "OM7")

# Extract geometry and calculate viewshed
east_windfarm_geom <- st_geometry(east_windfarm)[[1]]
east_viewshed <- viewshed(dem=pan500m, windfarm=east_windfarm_geom,
                          h1=1.5, h2=54, radius=5000)

# Display results
plot(pan500m)
plot(west_viewshed, add=TRUE, legend=FALSE, col="red")
plot(east_viewshed, add=TRUE, legend=FALSE, col="blue")

#Merge East and West viewsheds 

west_viewshed <- extend(west_viewshed, pan500m) # could use 50m
east_viewshed <- extend(east_viewshed, pan500m)
both_viewshed <- merge(west_viewshed, east_viewshed)
plot(pan500m, col=terrain.colors(25))
plot(both_viewshed, legend=FALSE, add=TRUE, col="red")

#Which settlements can see the viewshed? 

settlements <- st_read("gis_data/settlements.shp")

settlements_ll <- st_transform(settlements, 4326) # to look at it in the mapview mode

mapview(settlements_ll) #viewing it 

#Convert viewshed map into polygon map
both_viewshed_poly <- rasterToPolygons(both_viewshed) %>% st_as_sf()
plot(both_viewshed_poly)
print(both_viewshed_poly, n=5)

#Dissolve viewshed polygons into a single polygon

install.packages("rgeos")
library(rgeos)

both_viewshed_poly <- rasterToPolygons(both_viewshed, dissolve=TRUE) %>% st_as_sf()
plot(both_viewshed_poly)
print(both_viewshed_poly, n=5)

plot(both_viewshed_poly)

#Clip the settlements map with the dissolved viewshed map 

settlements_my_viewshed <- st_intersection(settlements, both_viewshed_poly)

both_viewshed_poly <- st_transform(both_viewshed_poly, 27700) #fixing the differences in boundaries
settlements_my_viewshed <- st_intersection(settlements, both_viewshed_poly)

plot(settlements_my_viewshed)

settlements_my_viewshed_ll <- st_transform(settlements_my_viewshed, 27700) # to look at it in the mapview mode
mapview(settlements_my_viewshed)
