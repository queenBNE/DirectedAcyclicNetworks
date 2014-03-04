####################################################################################################
# Find all edge pairs that are allowed to be switched for the switching algorithm as defined in    #
# Carstens, C.J., "Motifs in Directed Acyclic Networks," Signal-Image Technology & Internet-Based  #
# Systems (SITIS), 2013 International Conference on , vol., no., pp.605,611, 2-5 Dec. 2013         #
# doi: 10.1109/SITIS.2013.99                                                                       #
# By default the squares are counted ONLY, since the memory consumption may get out of hand        #            
####################################################################################################

findSwappableEdges <- function(el, countOnly=T){
  m <- dim(el)[1]
  if(!countOnly)
    alt_squares <- list()
  k <- 1
  # For all but the last edge (since they're ordered)
  for(i in 1:(m-1)){
    # Get all the edges after the current edge
    if(i < (m-1))
      elSub <- el[(i+1):m, ]
    else
      elSub <- matrix(el[m, ], nrow=1, ncol=2)
    
    # Current edge is x --> y
    x <- el[i,1]
    y <- el[i,2]
    
    # Obtain indices of edges that have 
    #     either too small a source: s <= y
    #     or too large a target: t >= x
    tooSmallSource <- elSub[,1] <= y 
    tooBigTarget <- elSub[,2] >= x
    
    # Obtain indices of edges that are adjacent to the current edge 
    elT <- el[-i,]
    xOutNeighbours <- elT[elT[,1] == x, 2]
    sameOutNeighbour <- elSub[,2] %in% xOutNeighbours
    yInNeighbours <- elT[elT[,2] == y, 1]
    sameInNeighbour <- elSub[,1] %in% yInNeighbours
    
    # Indices for all edges that have none of the properties defined above
    # All these edges can be swapped with the current edge 
    canBeSwapped <- which(tooSmallSource + tooBigTarget + sameOutNeighbour + sameInNeighbour == 0)
    
    # Store or count the edges
    if(length(canBeSwapped) > 0)
      for(j in canBeSwapped){
        if(!countOnly)
          alt_squares[[k]] <- c(x, y, elSub[j,1], elSub[j,2])
        k <- k + 1
      }
  }
  
  # Return either the number of swappable edge-pairs or the pairs
  if(countOnly)
    return(k)
  return(alt_squares)
}