

moving_window <- function(df, n, by){   
  
  for(k in 1:n){
    
    df2 <- df[JDate >= k & JDate <= (k + 7)] 
    
    DI = df2[, get_sri(.SD, 'ANIMAL_ID', 
                 by = by)]
    DI$JDate = k
    out[[k]] = DI
  }
  return(rbindlist(out))
}