DirectedAcyclicNetworks
=======================

### karrerNewman.R

This file contains an implementation of the first random graph model discussed in: "Random graph models for directed acyclic networks, Phys. Rev. E 80, Brian Karrer and M. E. J. Newman (2009)". 

***********************
### orderedSwitching.R 

This file contains an implementation of the ordered switching model as discussed in: "A uniform random graph model for directed acyclic networks and its effect on motif-finding", Submitted manuscript, C. J. Carstens (2014)."

This implementation is quite slow, use the adjusted version of MFinder as described below for large networks.

***********************
### MFinder2.1Adj

Adjusted version of the MFinder software: http://www.weizmann.ac.il/mcb/UriAlon/download/network-motif-software

Use the following command to run MFinder: 

````
./mfinder <filename> -s 2 -r 100 -ornet -rdag -nor 
````

In this case 100 randomized networks will be saved in 100 output files. The networks will be randomized using the ordered switching algorithm (-rdag flag). Notice that for this to work, the original directed acyclic network HAS to have vertex IDs corresponding to a topological ordering, such that all edges (i,j) have the property that i > j. See the above link for more information on running MFinder and the different flags and options. 

***********************

