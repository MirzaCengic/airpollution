
#' Convert to rayshader object
#' 
#' Takes in raster and convert to rayshader friendly object
#'
#' @param x 
#'
#' @return
#' @export
#'
#' @examples
rayshaderize <- function(x)
{
  xx <- matrix(
    raster::extract(x,raster::extent(x)),
    nrow = ncol(x),
    ncol = nrow(x))
  return(xx)
}


#' Flip for plotting
#'
#' @param x 
#'
#' @return
#' @export
#'
#' @examples
fliplr = function(x) {
  # xx <- getValues(x)
  x[,ncol(x):1]
}

#' Prepare overlay data
#' 
#' take simple feature (from osm), and raster, and make layer for add_overlay from rayshader
#'
#' @param poly 
#' @param raster 
#'
#' @return
#' @export
#'
#' @examples
prepare_overlay_data <- function(data, raster, color = "black")
{
  if (inherits(dem_cropped_small, "RasterLayer"))
  {
    data_r <- data
  } else 
  {
    data_r <- raster::rasterize(as(data, "Spatial"), raster, 1, background=0)
    
  }
  
  mat_r <- matrix(
    raster::extract(data_r,raster::extent(data_r)),
    nrow = ncol(data_r),
    ncol = nrow(data_r))
  
  tempfilename = tempfile()
  png(tempfilename,width = nrow(mat_r), height = ncol(mat_r))
  par(mar = c(0,0,0,0))
  raster::image(fliplr(mat_r), axes = FALSE, col = color))
  dev.off()
  map_r <- png::readPNG(tempfilename)
  return(map_r)
}
