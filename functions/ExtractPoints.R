

ExtractPoints <- function(pt.matrix, raster.layer){
  # Extract in raster layer values at points
  raster::extract(raster.layer, pt.matrix) ## extract function needs to be from 'raster' package
}