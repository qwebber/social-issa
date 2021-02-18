

#' Dynamic network
#'
#' @inheritParams hr_network
#'
#' @return Graph strength for each individual. 
#' @export
#' 
get_graph <- function(DT = NULL, id = NULL, by = NULL) {
  
  if (is.null(DT) | is.null(id)) {
    stop('DT, and id must be provided')
  }
  
  DT[, {
    d <- data.table::dcast(.SD, formula = group ~ get(id), 
                           fun.aggregate = length, value.var = 'group')
    
    gbi_df <- data.matrix(d[, !'group', with = FALSE])
    
    rownames(gbi_df) <- d$group
    
    gbi.net_df <- asnipe::get_network(gbi_df, data_format = "GBI", 
                                      association_index = "SRI")
    gbi.net_df[lower.tri(gbi.net_df)] <- NA
    diag(gbi.net_df) <- NA
    
    gbi.grph_df <- graph.adjacency(gbi.net_df,
                                                       mode = "undirected",
                                                       diag = FALSE,
                                                       weighted = TRUE)
    
                
    
  }, by = by]
  
}