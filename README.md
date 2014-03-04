DirectedAcyclicNetworks
=======================

***********************
karrerNewman.R

I implemented the first random graph algorithm discussed in: 

Random graph models for directed acyclic networks

Phys. Rev. E 80, 2009

Brian Karrer and M. E. J. Newman

***********************
orderedSwitching.R 

I implemented the ordered switching model as discussed in:

A uniform random graph model for directed acyclic networks and its effect on motif-finding

Submitted manuscript, 2014

C. J. Carstens

This implementation is quite slow, use the adjusted version  of MFinder as described below for large networks

***********************
MFinder2.1Adj

I adjusted the MFinder software:

http://www.weizmann.ac.il/mcb/UriAlon/download/network-motif-software

When running MFinder using the following command:

The specified number of randomized networks will be saved as an output. The networks will be randomized using the ordered switching algorithm. Notice that for this to work, the original directed acyclic network HAS to have vertex IDs corresponding to a topological ordering, such that all edges (i,j) have the property that i > j. 


