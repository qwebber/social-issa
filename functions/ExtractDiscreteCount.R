

ExtractDiscreteCount <- function(pt.matrix, raster.layer, buff.size){
  # Extract from buffered points by buffer size
  e <- raster::extract(raster.layer, pt.matrix, buffer = buff.size, 
                       sp = FALSE, df = TRUE)
  
  # With extract as data.table, count total N observed and N observed by class
  d <- data.table(id = e[,1], val = e[,2])[, total := .N, by = id][, .(.N, total), by = .(val, id)]
  
  # Dcast.data.table, preserving total, one column per class
  dcast(d, id + total ~ val)
}