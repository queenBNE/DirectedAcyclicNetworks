# Create the state graph for the Markov chain corresponding to the 
# 'ordered switching model' for a small directed acyclic network. 

# Load swappableEdges.R
source('findSwappableEdges.R')

# Either use one of the example networks below 
el <- cbind(c(6,6,5,5,5,4), c(4,3,4,3,1,2))             # State-graph with 5 states
# el <- cbind(c(6,6,5,4,3,2), c(5,4,1,2,2,1))             # State-graph with 6 states
# el <- cbind(c(6,6,5,5,5,4,4,3), c(3,2,4,3,2,3,1,1))     # State-graph with 8 states
# el <- cbind(c(6,6,5,4,3,2), c(5,3,2,1,2,1))             # State-graph with 9 states

# Or create a random DAG with n vertices 
# n <- 6
# M <- round(matrix(runif(n^2), nrow=n))
# l <- lower.tri(M)                                               
# dag <- M*l
# x <- which(dag == 1) %% n
# x[x==0] <- n
# y <- ceiling(which(dag == 1)/n)
# el <- cbind(x,y)


# Sort the edge list
el <- data.frame(el)
el <- el[with(el, order(X1, X2)),]

# Edgelist of state graph and corresponding states 
psi <- matrix(nrow=0, ncol=2)
states <- list()

# Set the initial network as the current state
k <- 1
states[[k]] <- el
nextNew <- c(1)

# Simple breadth-first algorithm - stop when no new states are found
while(length(nextNew) > 0){
  newStates <- nextNew 
  nextNew <- c()
  for(j in newStates){
    swappableEdges <- findSwappableEdges(states[[j]], F)
    for(swappableEdgePair in swappableEdges){
      # Edges in current network 
      e1 <- swappableEdgePair[1:2]
      e2 <- swappableEdgePair[3:4]
      # Edges after swapping heads
      f1 <- swappableEdgePair[c(1,4)]
      f2 <- swappableEdgePair[c(3,2)]
      # Swap the edges to create a new network
      curState <- states[[j]]
      newState <- curState
      newState[which(apply(curState, 1, function(x) all(x == e1))),] = f1 
      newState[which(apply(curState, 1, function(x) all(x == e2))),] = f2
      tmp <- data.frame(newState)
      newState <- tmp[with(tmp, order(X1, X2)),]
      # Check if this network already existed in the stategraph
      isOld <- 0
      for(l in 1:length(states)){
        state <- states[[l]]
        areEqual <- prod(state == newState)
        isOld <- isOld + areEqual
        # Create an edge in the state graph from the current state to an existing 
        # state equal to the 'newstate'. 
        if(areEqual){
          # Create these undirected edges from high to low (easier for retrieval)
          if(j > l){
            idx <- which(psi[,1]==j & psi[,2]==l)
            if(length(idx) == 0)
              psi <- rbind(psi, c(j, l))
          }else{
            idx <- which(psi[,1]==l & psi[,2]==j)
            if(length(idx)==0)
              psi <- rbind(psi, c(l, j))
          }
        }
      }
      # If this is a new state, save it to the states and create an edge from current
      # state to the new state in the stategraph
      if(!isOld){
        k <- k + 1
        psi <- rbind(psi, c(k, j))
        states[[k]] <- newState
        nextNew <- c(nextNew, k)
      }
    }
  }
}

# Plot the stategraph - needs 'igraph' library 
# library('igraph')
# g <- graph.edgelist(psi, F)
# plot(g)
