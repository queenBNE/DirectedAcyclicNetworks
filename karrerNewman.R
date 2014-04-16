# This function returns the edgelist of a random directed acyclic network generated 
# using the first random graph model introduced in "Random graph models for directed 
# acyclic networks, Phys. Rev. E 80, Brian Karrer and M. E. J. Newman (2009)"

# The function takes as inputs the in-degree and out-degree sequence of a directed 
# acyclic network G=(V,E). 

# These sequences need to correspond to a topological ordering (1, ..., n) of V
# such that for every edge (i, j), i > j. 
# (i.e. for such an ordering d_in[i] = d_in(v_i) and d_out[i] = d_out(i))

# For example, the directed acyclic networks below are identical, but the vertices 
# of the first one are not labelled with a topological ordering

# V = {1,2,3}
# E = {(3,2), (3,1), (1,2)}

# V = {1,2,3}
# E = {(3,2), (2,1), (3,1)}

# For the second network the input to the Karrer Newman algorithm should be 
# d_in <- c(2,1,0)
# d_out <- c(0,1,2)

# A more interesting example is: 
# d_in <-  c(1,1,2,2,0,0)
# d_out <- c(0,0,0,1,3,2)


karrerNewmanEdgelist <- function(d_in, d_out){
   
  # Empty matrix to save edges and index
  edgelist <- matrix(nrow=sum(d_in), ncol=2)
  j <- 1
  
  # The number of 'in stubs' at each vertex (initially 0)
  in_stub_count <- rep(0, length(d_out))
  
  # For each vertex - starting with the 'oldest' vertex
  for(v in 1:length(d_in)){
    
    # If it should have outgoing edges
    if(d_out[v] > 0){
      # Create a list of vertices with in-stubs
      stubs <- c(rep(1:length(d_out), in_stub_count))
      # And pick the in-stubs to connect to
      if(length(stubs) == 1){
        out_neighbours <- stubs
      }else{
        out_neighbours <- sample(stubs, d_out[v], replace=F)
      }
      
      # Add the chosen edges to the edgelist
      for(t in out_neighbours){
        edgelist[j,] <- c(v, t)
        j <- j + 1
      }
      
      # Remove the chosen in-stubs from the in-stub counts
      nbrs <- table(out_neighbours)
      rownames <- row.names(nbrs)
      in_stub_count[as.numeric(rownames)] <- in_stub_count[as.numeric(rownames)] - nbrs[rownames]
    }
    # Add in-stubs for the current vertex
    in_stub_count[v] <- d_in[v]
  }
  # Create the
  return(edgelist)
}

