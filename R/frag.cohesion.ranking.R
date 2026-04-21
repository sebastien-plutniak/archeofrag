
frag.cohesion.ranking <- function(cohesion.df, add.math.signs = TRUE){
  if(nrow(cohesion.df) == 1) return()
  cohesion.df <- data.frame(cohesion.df)
  
  cohesion.df$unit1 <- gsub("^(.*?) ?/ ?(.*?)$", "\\1", rownames(cohesion.df), perl = TRUE)
  cohesion.df$unit2 <- gsub("^(.*?) ?/ ?(.*?)$", "\\2", rownames(cohesion.df), perl = TRUE)
  
  sp.units <- unique(c(cohesion.df$unit1, cohesion.df$unit2))
  
  count <- apply(cohesion.df, 1, function(x) x[c(ncol(cohesion.df) -1, ncol(cohesion.df))][order(x[1:2])[2] ] )
  count <- sort(table(count), decreasing = TRUE)
  count <- t(as.data.frame(count, stringsAsFactors = FALSE))
  
  sp.units <- sp.units[ ! sp.units %in% count[1, ]]
  
  count <- cbind(count, rbind(sp.units, 0))
  res <- as.numeric(count[2, ])
  names(res) <- count[1, ]
  
  if( ! add.math.signs) return(res)
  
  operators <- sapply(seq_len(length(res) -1) ,  function(x)  res[x + 1] - res[x])
  operators.char <- operators
  operators.char[operators < 0 ] <- ">"
  operators.char[operators == 0 ] <- "="
  operators.char <- c(operators.char, "")
  
  res <- rbind(
    c(rbind(names(res), operators.char)),
    c(rbind(res, rep("", length(res))))
  )
  
  res <- data.frame(res)
  colnames(res) <- res[1,]
  rownames(res) <- c("", "Count")
  res[-1, ]
}
