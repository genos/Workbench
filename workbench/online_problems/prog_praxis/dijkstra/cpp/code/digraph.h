/*
 * digraph.h----Graham Enos----Fall 2010
 *
 * This file contains the interface to my digraph implementation for ITCS
 * 6114's programming project.
 *
 */
#ifndef DIGRAPH_H
#define DIGRAPH_H
#include <fstream>
#include <iostream>
#include <limits>
#include <map>
#include <string>
#include <sstream>
#include <vector>
#include "vertex.h"
#include "min_priority_queue.h"
using namespace std;

// A digraph is just a hashtable (map) of string --> vertex pairs with other
// functionality attached
class DiGraph {
    public:
        map<string, Vertex>* V;
        DiGraph();
        void buildFromFile(char* fileName);
        void printGraph();
        void initSingleSource(string sName);
        void relax(string uName, string vName);
        void dijkstra(string from_vert);
        void shortestPath(string from_vert, string to_vert);
};
#endif // DIGRAPH_H
