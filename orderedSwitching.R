# Implementation of the ordered switching method. 
# Takes as an input the edgelist of a directed acyclic network where vertices are 
# labelled such that for every edge (x,y) we have x > y. 
# Pairs of edges (x,y) and (u,v) are selected at random. 
# If these satisfy the following conditions: 
# 1. x != u and y != v    since a switch should change the network
# 2. x > v and u > y      since a switch should respect the topological ordering
# 3. (x,v) and (y,u) are not in the edgelist
#                         since a switch should not introduce multiple edges
# then the edges will swap heads

orderedSwitching <- function(el, swaps = 100){
  if(!is.matrix(el))
    return("Please provide a matrix")
  m <- dim(el)[1]
  count <- 0
  for(i in 1:swaps){
    toswap <- sample(1:m, 2, replace=F)
    
    e1 <- el[toswap[1],]
    x <- e1[1]
    y <- e1[2]
    
    e2 <- el[toswap[2],]
    u <- e2[1]
    v <- e2[2] 
    
    if(x != u)
      if(y != v)
        if(x > v)
          if(u > y)
            if(sum(el[,1]==x & el[,2]==v) == 0)
              if(sum(el[,1]==u & el[,2]==y) == 0){
                el[toswap[1], 2] <- v
                el[toswap[2], 2] <- y
                count <- count + 1 
              }
  }
    
  print(paste("Requested: ", swaps, " swaps. Made: ", count, " swaps", sep=""))
  return(el)
}