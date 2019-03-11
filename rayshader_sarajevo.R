
# Script setup ------------------------------------------------------------


pacman::p_load(tidyverse, sf, raster, rayshader, janitor, data.table, mapview, mapedit, osmplotr, osmdata)

source("./R/functions_misc.R")
#
# aa <- mapedit::editMap(mapview(dem_cropped))

# Load data ---------------------------------------------------------------


# aa
# Load BiH boundaries
bih_sf <- st_read("./Data/Vector/gadm36_BIH.gpkg", layer = "gadm36_BIH_3")
crop_outline <- st_read("./Data/Vector/extent_smaller.gpkg")
# Subset only Kanton Sarajevo
sarajevo_canton <- bih_sf %>% 
  filter(NAME_2 == "Sarajevo") %>% 
  st_cast("MULTILINESTRING") %>% 
  as("Spatial")

# Load Sarajevo DEM
dem_cropped <- raster("./Data/DEM/Sarajevo_valley_SRTM.tif")
dem_cropped_small <- crop(dem_cropped, crop_outline)
# dem_cropped_small[dem_cropped_small < 545] = 545

# Load sarajevo mountans -- approximate location. Right now Igman, Trebevic, Hum, and Zuc
mts_sjj <- st_read("./Data/Vector/sarajevo_mountains.gpkg")


# Create roads and rivers layer from open street maps

# Roads
sarajevo_roads <- crop_outline %>% 
  st_bbox() %>% 
  opq() %>% 
  add_osm_feature(key = "highway") %>% 
  osmdata_sf() %>% 
  `$`(osm_lines) %>% 
  filter(highway == "primary") %>%
  st_buffer(dist = 0.0004) %>%
  st_union()

# sarajevo_roads_raw %>% 
#   st_buffer(dist = 0.0003) %>%
#   st_union()


# sarajevo_roads <- sarajevo_roads_raw %>% 
#   filter(highway == "primary") %>%
#   # filter(highway %in% c("primary", "secondary", "tertiary")) %>%
#   st_buffer(dist = 0.0003) %>%
#   st_union()

# sarajevo_roads_raw %>% 
#   tabyl(highway) %>% 
#   arrange(n)

# Rivers - needs cleaning up
sarajevo_rivers_raw <- crop_outline %>% 
  st_bbox() %>% 
  opq() %>% 
  add_osm_feature(key = "waterway") %>% 
  osmdata_sf() %>% 
  `$`(osm_lines) %>% 
  filter(name %in% c("Miljacka", "Bosna", "Miljacka Mokranjska", "Miljacka Paljanska",
                     "Mošćanica", "Dobrinja", "Željeznica", "Zujevina"))



# Create buffers of different widths
rivers_primary <- sarajevo_rivers_raw %>% 
  filter(name %in% c("Miljacka", "Bosna", "Željeznica")) %>% 
  st_buffer(dist = 0.0006) %>% 
  group_by(name) %>%
  summarise()

rivers_secondary <- sarajevo_rivers_raw %>% 
  filter(name %in% c("Miljacka Mokranjska", "Miljacka Paljanska",
                     "Mošćanica", "Dobrinja", "Zujevina")) %>% 
  st_buffer(dist = 0.0003) %>% 
  group_by(name) %>%
  summarise() 
  


# Create rivers set with different widths
rivers_width <- rbind(rivers_primary,
      rivers_secondary)

# mapview(aa)
# Rasterize openstreet map highway layer with a buffer

# rijeke <- prepare_overlay_data(sarajevo_rivers, dem_cropped_small, "blue4")
ceste <- prepare_overlay_data(sarajevo_roads, dem_cropped_small, c("transparent", "black"))

rijeke2 <- prepare_overlay_data(rivers_width, dem_cropped_small, c("transparent", "black"))


dem_ovrl <- prepare_overlay_data(dem_cropped_small, dem_cropped_small, terrain.colors(100))



mat_highway %>%
  ray_shade(zscale=50,maxsearch = 500,anglebreaks = seq(20,30,0.1)) %>%
  add_overlay(rijeke2,               alphacolor = "transparent",
alphalayer = 0.9) %>%
  add_overlay(ceste,               alphacolor = "transparent",
alphalayer = 0.7) %>%
  plot_map()

# rail_mat <- matrix(extract(rail_raster, extent(rail_raster)),
# nrow=nrow(rail_raster),
# ncol=ncol(rail_raster))
# rail_mat <- rail_mat[, ncol(rail_mat):1]  # flip

# Convert to rayshader-friendly object
# elmat <- matrix(raster::extract(dem_cropped,raster::extent(dem_cropped)),
#                 nrow=ncol(dem_cropped),ncol=nrow(dem_cropped))

#######################################################


# mat_small %>%
#   sphere_shade(texture = "desert") %>%
#   # add_water(detect_water(mat_small), color="imhof1") %>%
#   plot_map()
###


# Create ray shader object for small dem
mat_small <- rayshaderize(dem_cropped_small)
# Create shadow layer
ambmat_small <- ambient_shade(mat_small)





###



mat_small %>%
  sphere_shade(texture = "imhof4") %>%
  # sphere_shade(texture = create_texture("#fff673","#55967a","#8fb28a","#55967a","#cfe0a9")) %>%
  # add_water(detect_water(mat_small), color="desert") %>%
  add_shadow(ray_shade(mat_small,zscale = 2,maxsearch = 300),0.5) %>%
  add_shadow(ambmat_small,0.5) %>%
  add_overlay(rijeke2, 
              alphacolor = "transparent",
              alphalayer = 0.9) %>%
  add_overlay(ceste, 
              alphacolor = "transparent",
              alphalayer = 0.8) %>%
  plot_3d(mat_small,zscale = 12,fov = 0,
          theta = 175, zoom = 0.65,
          phi = 35, windowsize = c(1000,800),
          water = TRUE, 
          waterdepth = 60, # Fog height
          # wateralpha = 0.9,
          watercolor = "grey50",
          # watercolor = "imhof4",
          waterlinecolor = "white")


ls("package:rayshader")
render_snapshot(filename = "./Output/rs_plot2.png")

mat_small %>%
  sphere_shade(texture = "imhof4") %>%
  # sphere_shade(texture = create_texture("#fff673","#55967a","#8fb28a","#55967a","#cfe0a9")) %>%
  # add_water(detect_water(mat_small), color="desert") %>%
  add_shadow(ray_shade(mat_small,zscale = 2,maxsearch = 300),0.5) %>%
  add_shadow(ambmat_small,0.5) %>%
  add_overlay(dem_ovrl, 
              alphacolor = "transparent",
              alphalayer = 0.1) %>%
  add_overlay(rijeke2, 
              alphacolor = "transparent",
              alphalayer = 0.9) %>%
  
  add_overlay(ceste, 
              alphacolor = "transparent",
              alphalayer = 0.9) %>%
  plot_3d(mat_small,zscale = 12,fov = 0,
          theta = 175, zoom = 0.65,
          phi = 35, windowsize = c(1000,800),
          water = TRUE, waterdepth = 60, 
          wateralpha = 0.6,
          watercolor = "grey70")#,
          # watercolor = "imhof4",
          # waterlinecolor = "white") 
# %>% 
#   save_png(filename = "./Output/rs_plot1.png")

#### Create labels
# # Extract from points

# dim(mat_small)
render_label(mat_small, x = 818, y = 118, z = 5500, zscale=30, color = "black",
             text = "Trebević", textsize = 2, linewidth = 2)


render_label(mat_small, x = 552, y = 315, z = 1500, zscale=12,
             # dashed = TRUE,
             text = "Hum",textsize = 2, linewidth = 2)

render_label(mat_small, x = 400, y = 228, z = 2300, zscale=12, 
             # dashed = TRUE,
             text = "Miljacka",textsize = 1.75, color = "blue4", textcolor = "blue4", linewidth = 1)


render_label(mat_small, x = 125, y = 120, z = 2100, zscale=12, 
             # dashed = TRUE,
             text = "Vrelo Bosne",textsize = 1.75, color = "blue4", textcolor = "blue4", linewidth = 1)


render_label(mat_small, x = 490, y = 415, z = 1100, zscale=12,
             text = "M223",textsize = 1, color = "grey10", textcolor = "grey10", linewidth = 1)

###########################



mat_small %>%  
  plot_3d(mat_small, zscale=8, 
          soliddepth=-60, 
          windowsize = c(1200, 1200), 
          fov=60, 
          water=TRUE)
