.frag.graph.mirror <- function(g.observed, morphometry, x, y, z){
  
  igraph::V(g.observed)$layer <- as.character(igraph::V(g.observed)$layer)
  igraph::V(g.observed)$name <- as.character(igraph::V(g.observed)$name)
  v <- length(g.observed)
  
  # generate mirrored graph:
  if(v %% 2 == 1) { # uneven 
      g.mat <- matrix(seq_len(v - 1), ncol = 2, nrow = (v - 1) / 2, byrow = TRUE)
      g.mat <- rbind(g.mat, c(1, 0))
  } else {
    g.mat <- matrix(seq_len(v), ncol = 2, nrow = floor(v / 2), byrow = TRUE)
  }
  g.mirrored <- igraph::graph_from_data_frame(g.mat, directed = FALSE)
  
  # add attributes to the mirrored graph:
  igraph::V(g.mirrored)$name <- paste0(igraph::V(g.mirrored)$name, ".mirrored")
  igraph::V(g.mirrored)$layer <- "mirror"
   
  # merge the observed and mirrored graphs:
  g <- igraph::disjoint_union(g.observed, g.mirrored)
  g$frag_type <-  "cr"
  g
}

