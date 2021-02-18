#' Dynamic network
#'
#' @inheritParams hr_network
#'
#' @return Graph strength for each individual. 
#' @export
#' 
get_sri <- function(DT = NULL, id = NULL, by = NULL) {
  
  if (is.null(DT) | is.null(id)) {
    stop('DT, and id must be provided')
  }
  
  DT[, {
    d <- data.table::dcast(.SD, formula = groupEnd ~ get(id), 
                           fun.aggregate = length, value.var = 'groupEnd')
    
    gbi_df <- data.matrix(d[, !'groupEnd', with = FALSE])
    
    rownames(gbi_df) <- d$groupEnd
    
    gbi.net_df <- asnipe::get_network(gbi_df, data_format = "GBI", 
                                      association_index = "SRI")
    gbi.net_df[lower.tri(gbi.net_df)] <- NA
    diag(gbi.net_df) <- NA
     
    gbi.grph_df <- igraph::graph_from_adjacency_matrix(gbi.net_df,
                                                       mode = "undirected",
                                                       diag = FALSE,
                                                       weighted = TRUE)
    
    out <- na.omit(reshape2::melt(gbi.net_df))
    
    out <- data.table(ID1 = out$Var1, 
                      ID2 = out$Var2, 
                      sri = out$value)
    
    
    #memb <- data.table(membershipID1 = membership(fastgreedy.community(gbi.grph_df)),
    #                   ID1 = names(igraph::degree(gbi.grph_df)))
    #memb2 <- data.table(membershipID2 = membership(fastgreedy.community(gbi.grph_df)),
    #                   ID2 = names(igraph::degree(gbi.grph_df)))
    
    #all <- merge(out, memb, by = "ID1")                   
    #all2 <- merge(all, memb2, by = "ID2")                  
    
 }, by = by]
  }