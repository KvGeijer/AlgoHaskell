#Problem formulation

This project boils down to representing a graph from data given by the files in data, and then doing several BFS to find shortest path between a pair of nodes.

The nodes represent five letter words. Word A is said to be adjacent to word B if the last four letters in A are contained in B. For example there is a two way edge between "putin" and "input". If a letter occures twice in A's four last letters it must also occur twice in B, and so on.

The indata first contains N (number of nodes in graph) and Q (number of queries). The coming N lines are five letter words. The coming Q lines are two words, the code should print to standard output the shortest path from the first word to the other.

I found this particularily tricky to implement with Haskells pure data structures. 

This was a project in the course EDAF05 (algorithms, data structures and time complexity) at LTH 2020.
