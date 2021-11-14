
.compare.values <- function(h1.values, h2.values, obs.value){
  obs.value <- unlist(obs.value)
  h1.values <- unlist(h1.values)
  h2.values <- unlist(h2.values)
  # difference between H1 and H2
  wilcox.res <- wilcox.test(h1.values, h2.values, exact=FALSE)$p.value
  
  if(is.nan(wilcox.res)) return(c(NA,NA,NA,NA))
  
  # result for the wilcoxon test:
  ccl0 <- F
  if(wilcox.res < 0.05){
    ccl0 <- T
  }
  # observed value compared to H1 values:
  if(obs.value < summary(h1.values)[2]){
    ccl1 <- "lower"
  } else if(obs.value > summary(h1.values)[5]){
    ccl1 <- "higher"
  } else{
    ccl1 <- "within"
  }
  # observed value compared to H2 values:
  if(obs.value < summary(h2.values)[2]){
    ccl2 <- "lower"
  } else if(obs.value > summary(h2.values)[5]){
    ccl2 <- "higher"
  } else{
    ccl2 <- "within"
  }
  c("H1 != H2?" = ccl0, "p.value" = round(wilcox.res, 2),
    "Obs. value/H1" = ccl1, "Obs. value/H2" = ccl2)
}

frag.simul.summarise <- function(graph, layer.attr, res.h1, res.h2,
                                 cohesion1.attr = "cohesion1",
                                 cohesion2.attr = "cohesion2",
                                 admixture.attr = "admixture"){
  # todo: add params:  
  if(! is.igraph(graph)) stop("Not a graph object")
  if(is.null(vertex_attr(graph, layer.attr)))   stop("'layer.attr' is missing or does not correspond to a vertex attribute of the graph.")
  if( ! is.character(layer.attr))  stop("The parameter 'layer.attr' requires a character value.")
  if( ! layer.attr %in% names(vertex_attr(graph)) ){
    stop(paste("No '", layer.attr, "' vertices attribute.", sep=""))
  }
  if(is.null(res.h1) | is.null(res.h2)){
    stop("res.h1 and res.h2 are mandatory.")
  }
  if(sum(! colnames(res.h1) == colnames(res.h2)) != 0){
    stop("The column names of res.h1 and res.h2 are not identical.")
  }
  if(! (is.data.frame(res.h1) | is.data.frame(res.h2)) ){
    stop("Data frames are required for the res.h1 and res.h2 parameters.")
  }
  if( ! (is.character(cohesion1.attr) | is.character(cohesion2.attr) | is.character(admixture.attr))){
    stop("Character strings are required for the cohesion1.attr, cohesion2.attr, and admixture parameters.")
  }
  if( sum(c(cohesion1.attr, cohesion2.attr, admixture.attr) %in% colnames(res.h1))  != 3 ){
    stop(paste("No column named '", cohesion1.attr, "', '", cohesion2.attr, "' or '", admixture.attr, "' in the data frames.", sep=""))
  }
  colnames(res.h1)[which(colnames(res.h1) == cohesion1.attr)] <- "cohesion1"
  colnames(res.h1)[which(colnames(res.h1) == cohesion2.attr)] <- "cohesion2"
  colnames(res.h1)[which(colnames(res.h1) == admixture.attr)] <- "admixture"
  colnames(res.h2)[which(colnames(res.h2) == cohesion1.attr)] <- "cohesion1"
  colnames(res.h2)[which(colnames(res.h2) == cohesion2.attr)] <- "cohesion2"
  colnames(res.h2)[which(colnames(res.h2) == admixture.attr)] <- "admixture"
  
  # retrieve the parameters of the observed graph:  
  obs.params <- c(frag.get.parameters(graph, layer.attr),
                  frag.layers.admixture(graph, layer.attr),
                  "cohesion" = frag.layers.cohesion(graph, layer.attr),
                  "weightsum" = sum(E(graph)$weight))
  
  if(sum(! colnames(res.h1) %in% names(obs.params)) != 0){
    warning("Some simulated parameters are missing in the observed graph.")
  }
  # parameters in the observed graph and the simulated results:
  params <- intersect(colnames(res.h1), names(obs.params))
  
  # compare the observed and simulated parameters:
  res <- sapply(params, function(param)
    .compare.values(res.h1[param], res.h2[param], obs.params[param]))
  # results:
  as.data.frame(t(res))
}

