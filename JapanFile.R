library(tidyverse)
library(httr)
library(sf)
library(R.utils)
library(stars)
library(rayshader)
setwd("~/Desktop/R Studio/Japan Rayshader Spikemap")
url<-"https://geodata-eu-central-1-kontur-public.s3.amazonaws.com/kontur_datasets/kontur_population_JP_20220630.gpkg.gz"

get_japan_data<-function(){
  filename="kontur_population_JP_20220630.gpkg.gz" 
  res<-httr::GET(url, write_disk(path =filename,overwrite = TRUE),
              progress())
  
  R.utils::gunzip(filename, remove = F)
}

get_japan_data()

crsLONGLAT <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

load_file<-str_extract(url,"(?<=kontur_datasets\\/)[:alnum:]*[:punct:]+[:alnum:]*[:punct:]*[:alnum:]*[:punct:]*[:alnum:]*[:punct:]*[:alnum:]*")

pop_data<-st_read(load_file) %>% st_transform(crs = crsLONGLAT)


#hokkaido<-pop_data %>% st_crop(xmin=137,xmax=147,ymin=41,ymax=47)

sf::st_bbox(pop_data)


bb <- sf::st_bbox(pop_data)

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
  pop_data |>
    select(population, geom),
  nx = width, ny = height)


pop_mat <- pop_rast |>
  as("Raster") |>
  rayshader::raster_to_matrix()

library(MetBrewer)

MetBrewer::display_all()

colors<-met.brewer("Morgenstern")


texture <- grDevices::colorRampPalette(colors)(256)
pop_mat |>
  rayshader::height_shade(texture = texture) |>
  rayshader::plot_3d(
    heightmap = pop_mat,
    solid = F,
    soliddepth = 0,
    zscale = 70,
    shadowdepth = 0,
    shadow_darkness = .95,
    windowsize = c(800, 800),
    phi = 25,
    zoom = .9,
    theta = 0,
  )

rayshader::render_camera(phi = 50, zoom = .5, theta = 0)


rayshader::render_highquality(
  filename = "japan_population_2022.png",
  preview = T,
  light = T,
  samples=300,
  lightdirection = 225,
  lightaltitude = 60,
  lightintensity = 800,
  interactive = F,
  width = width, height = height
)

library(magick)
library(showtext)

font_add_google("Abel")
font_add_google("Cinzel Decorative")
list.files()
showtext.auto()
image<-image_read("japan_population_2022.png" )

colors[1]

image_annotate(image,text = "Japan",size = 150,font = "Cinzel Decorative",gravity = "north",weight = 700,color = colors[1])

