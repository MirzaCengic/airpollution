pacman::p_load(tidyverse, sf, raster, rayshader, data.table, mapview, mapedit)

shp <- editMap(mapview())
myshp <- as(shp$finished, "Spatial")


mountain_pts <- editMap(mapview())

mts_sjj <- mountain_pts$finished %>% 
  mutate(
    name = c("Trebevic", "Zuc", "Igman")
  )


aa <- raster("/home/mirza/Data/SRTM DEM/N43E018.SRTMGL1.hgt/N43E018.hgt")


bih_sf <- st_read("/home/mirza/Data/gadm36_BIH.gpkg", layer = "gadm36_BIH_3")

head(bih_sf)

bih_sf$NAME_2 %>% unique

sarajevo_cant <- bih_sf %>% 
  filter(NAME_2 == "Sarajevo") %>% 
  st_cast("MULTILINESTRING") %>% 
  as("Spatial")

plot(rail_raster)
mapview(sarajevo_cant)
rail_raster <- raster(ncol=ncol(dem_cropped), nrow=nrow(dem_cropped))

rail_raster <- setExtent(rail_raster, extent(dem_cropped))
rail_raster <- rasterize(sarajevo_cant, rail_raster, 1, background=0)
rail_mat <- matrix(extract(rail_raster, extent(rail_raster)),
                   nrow=nrow(rail_raster),
                   ncol=ncol(rail_raster))
rail_mat <- rail_mat[, ncol(rail_mat):1]  # flip

dem_cropped <- crop(aa, myshp)

plot(rail_raster)
plot(as(mts_sjj, "Spatial"), add =T)
mapview(dem_cropped)
writeRaster(dem_cropped, "/home/mirza/Data/SRTM DEM/Sarajevo_valley_SRTM.tif")


ba_boundary <- getData("GADM", country = "BA", level = 2)


elmat = matrix(raster::extract(dem_cropped,raster::extent(dem_cropped)),
               nrow=ncol(dem_cropped),ncol=nrow(dem_cropped))

dim(elmat)
coordinates(dem_cropped) 
st_coordinates(mts_sjj)

mts_sjj_sp <- as(mts_sjj, "Spatial")

# Get cell id of rasters at point locations
raster::extract(dem_cropped, mts_sjj_sp, df = TRUE, cellnumbers = TRUE)


point_cells <- rowColFromCell(dem_cropped, cellFromXY(dem_cropped, mts_sjj_sp))

mapview(mts_sjj)

elmat2 <- elmat[, ncol(elmat):1]  # flip

elmat2 %>%
  sphere_shade(texture = "desert") %>%
  plot_map()

elmat3 <- elmat2 - 500

elmat3 %>%
  sphere_shade(texture = "desert") %>%
  add_water(detect_water(elmat3), color="blue") %>%
  add_shadow(ray_shade(elmat3,zscale=3,maxsearch = 300),0.7) %>%
  add_shadow(elmat3,0.7) %>%
  plot_3d(elmat3,zscale=10, fov=0,zoom=0.75,phi=45)

render_label(elmat, y = point_cells[1, 1], x = point_cells[1, 2], z=8000, zscale=50, 
             text = "Trebevic", textsize = 2, linewidth = 5)
# 
# render_label(elmat, y = 256, x = 647,
#              z = 6000, zscale = 50, 
#              text = "Hum",color="darkred", textcolor = "darkred", textsize = 2, linewidth = 5)
# 
render_label(elmat,y= point_cells[2, 1], x= point_cells[2, 2], z=8000, zscale=50,
             text = "Zuc", dashed = TRUE, textsize = 2, linewidth = 5)

render_label(elmat, y = point_cells[3, 1], x = point_cells[3, 2], z=8000, zscale=30, color = "red",
             text = "Crni vrh", textsize = 2, linewidth = 2)
###
z=100
model <- elmat %>%
  sphere_shade(sunangle = 200) %>%
  add_shadow(ambient_shade(elmat, zscale=z, anglebreaks = seq(65, 65, 1))) %>%
  add_shadow(ray_shade(elmat, zscale=z, lambert=FALSE, anglebreaks = seq(65, 65, 1))) %>%
  add_water(t(rail_mat)) # for subway routes

model %>% plot_map()


dim(elmat)
dim(t(rail_mat))
# 
# Plot 3D


model %>%  
  plot_3d(elmat, zscale=8, 
          soliddepth=-60, 
          windowsize = c(1200, 1200), 
          fov=60, 
          water=TRUE)
