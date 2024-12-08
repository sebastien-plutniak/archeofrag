frag.relations.by.layers <- function (graph, layer.attr) 
{
  .check.frag.graph(graph)
  .check.layer.argument(graph, layer.attr)
  layers <- igraph::vertex_attr(graph, layer.attr)
  if (is.null(igraph::V(graph)$name)) {
    igraph::V(graph)$name <- 1:igraph::gorder(graph)
  }
  v.list <- data.frame(v = igraph::V(graph)$name, layer = layers)
  e.list <- data.frame(igraph::as_edgelist(graph))
  e.list <- merge(e.list, v.list, by.x = "X2", by.y = "v")
  e.list <- merge(e.list, v.list, by.x = "X1", by.y = "v")
  res <- table(e.list$layer.x, e.list$layer.y)
  
  row.to.add <- colnames(res)[ ! colnames(res) %in% rownames(res)]
  col.to.add <- rownames(res)[ ! rownames(res) %in% colnames(res)]
  
  # add missing rows and columns
  for(row.name in row.to.add){
    res <- rbind(res, 0)
    rownames(res)[nrow(res)] <- row.name
  }
  for(col.name in col.to.add){
    res <- cbind(res, 0)
    colnames(res)[ncol(res)] <- col.name
  }
  # reorder rows and colums:
  res <- res[order(rownames(res)), order(colnames(res))]  
  
  # sum the upper and lower values and keep the lower triangle of the matrix:
  res[lower.tri(res)] <- res[upper.tri(res)] + res[lower.tri(res)]
  res[upper.tri(res)] <- NA
  
  res
}