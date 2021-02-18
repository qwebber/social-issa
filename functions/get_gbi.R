


#' Dynamic network
#'
#' @inheritParams hr_network
#'
#' @return Graph strength for each individual. 
#' @export
#' 
get_gbi <- function(DT = NULL, id = NULL, by = NULL) {
  
  if (is.null(DT) | is.null(id)) {
    stop('DT, and id must be provided')
  }
  
  DT[, {
    d <- data.table::dcast(.SD, formula = group ~ get(id), 
                           fun.aggregate = length, value.var = 'group')
    
    gbi_df <- data.matrix(d[, !'group', with = FALSE])
    
    rownames(gbi_df) <- d$group
    
    list(gbi_df)

  }, by = by]
}