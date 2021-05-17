#' Dynamic network
#'
#' @inheritParams hr_network
#'
#' @return Graph strength for each individual. 
#' @export
#' 
#' 
dynamic_network <- function(DT = NULL, id = NULL, by = NULL) {
  
  if (is.null(DT) | is.null(id)) {
    stop('DT, and id must be provided')
  }
  
  #DT <- DT[Year == "2017"]
  #id <- 'ANIMAL_ID'
  
  DT[, {
    d <- data.table::dcast(DT, formula = group ~ get(id), 
                           fun.aggregate = length, value.var = 'group')
    
    gbi_df <- data.matrix(d[, !'group', with = FALSE])
    
    rownames(gbi_df) <- d$group
    
    gbi.net_df <- asnipe::get_network(gbi_df, data_format = "GBI", times = NULL,
                                      association_index = "SRI")
    
    gbi.grph_df <- igraph::graph_from_adjacency_matrix(gbi.net_df,
                                                       mode = "undirected",
                                                       diag = FALSE,
                                                       weighted = TRUE)
    
    
    
    list(
      strength = igraph::strength(gbi.grph_df),
      Q = igraph::modularity(cluster_walktrap(gbi.grph_df)),
      gDen = graph.density(gbi.grph_df),
      membership = membership(cluster_walktrap(gbi.grph_df)),
      ID = names(igraph::degree(gbi.grph_df))
    )
  }, by = by]
}