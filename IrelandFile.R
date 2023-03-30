library(tidyverse)
library(httr)
library(sf)
library(R.utils)
library(stars)
library(rayshader)
setwd("~/Desktop/R Studio/Ireland Population Spikemap")

get_geo_data<-function(){
  filename="kontur_population_IE_20220630.gpkg.gz" 
  res<-httr::GET(url, write_disk(path =filename,overwrite = TRUE),
                 progress())
  
  R.utils::gunzip(filename, remove = F)
}

get_geo_data()

url<-"https://geodata-eu-central-1-kontur-public.s3.amazonaws.com/kontur_datasets/kontur_population_IE_20220630.gpkg.gz"




dublin_shapefile<-st_read("./airview_dublincity_roaddata_ugm3/AirView_DublinCity_RoadData_ugm3.shp") %>% 
  st_transform(crs = st_crs(pop_data))

dublin_bb<-st_bbox(dublin_shapefile)



load_file<-str_extract(url,"(?<=kontur_datasets\\/)[:alnum:]*[:punct:]+[:alnum:]*[:punct:]*[:alnum:]*[:punct:]*[:alnum:]*[:punct:]*[:alnum:]*")

pop_data<-st_read(load_file)

dublin_pop_data<-st_intersection(pop_data,dublin_shapefile)

dublin_pop_data %>% 
  ggplot()+
  geom_sf()

bb<-st_bbox(dublin_pop_data)

get_raster_size <- function() {
  height <- sf::st_distance(
    sf::st_point(c(bb[["xmin"]], bb[["ymin"]])),
    sf::st_point(c(bb[["xmin"]], bb[["ymax"]]))
  )
  width <- sf::st_distance(
    sf::st_point(c(bb[["xmin"]], bb[["ymin"]])),
    sf::st_point(c(bb[["xmax"]], bb[["ymin"]]))
  )
  
  if (height > width) {
    height_ratio <- 1
    width_ratio <- width / height
  } else {
    width_ratio <- 1
    height_ratio <- height / width
  }
  
  return(list(width_ratio, height_ratio))
}
width_ratio <- get_raster_size()[[1]]
height_ratio <- get_raster_size()[[2]]

size <- 3000
width <- round((size * width_ratio), 0)
height <- round((size * height_ratio), 0)


pop_rast <- stars::st_rasterize(
  dublin_pop_data |>
    select(population, geom),
  nx = width, ny = height)


pop_mat <- pop_rast |>
  as("Raster") |>
  rayshader::raster_to_matrix()

library(MetBrewer)
cols<-met.brewer(name = "Hiroshige",direction = -1)

texture <- grDevices::colorRampPalette(cols)(256)


pop_mat |>
  rayshader::height_shade(texture = texture) |>
  rayshader::plot_3d(
    heightmap = pop_mat,
    solid = F,
    soliddepth = 0,
    zscale = 40,
    shadowdepth = 0,
    shadow_darkness = .95,
    windowsize = c(800, 800),
    phi = 45,
    zoom = .9,
    theta = 0,
  )
rayshader::render_camera(phi = 75, zoom = .5, theta = 0)
