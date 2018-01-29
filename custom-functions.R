# function file

#opposite of %in%, for brevity
'%ni%' <- function(x,y){
  !('%in%'(x,y))
}


# start making a correlation table to anticipate multicollinearity within model
#fxn for capturing only one half of the correlation table
get.upper.tri <- function(cormat){
  cormat[lower.tri(cormat)] <- NA
  return(cormat)
}
  
#fxn for reordering based on correlation coefficient
reorder.cormat <- function(cormat){
  # Use correlation between variables as distance
  dd <- as.dist((1-cormat)/2)
  hc <- hclust(dd)
  cormat <-cormat[hc$order, hc$order]
}
  


