# Breadth First Search Module
## Brief Explanation
Breadth first search module can traverse over a directed acyclic graph (DAG) having a user defined maximum node number. Module sends visited node data to the output port at every clock cycle.  All adjacent nodes of a visited node can be stored in a queue in 1 clock cycle.
## Design Overview
BFS module has clock, reset, data port, node store control and traverse start command input  interfaces. Module can store graph data by receiving node data through the data input port. Module starts traversing over the graph for requested source and destination nodes when it receives “traverse” command. 

BFS module has visited node data out port and traverse complete output signal interface. Module sends visited node data through the output port at every clock cycle. When module finishes traversing over the graph, it asserts a “complete” signal.  

Module stores graph information in node value map, node adjacency list and node adjacency number arrays. Each node in the graph is assigned to a specific index number. To access a node in the graph, assigned index values are used. Adjacency list of each node is represented as 2D array containing values of adjacent nodes. 
