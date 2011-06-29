/*
 * digraph.cpp----Graham Enos----Fall 2010
 *
 *  This file contains my digraph implementation for ITCS 6114's programming
 *  project.
 *
 */
#include "digraph.h"
using namespace std;

// Constructor
DiGraph::DiGraph() {
    V = new map<string, Vertex>();
}

// Builds our digraph from an input file specified as a command line argument
void DiGraph::buildFromFile(char* fileName) {
    ifstream file(fileName);
    string line;

    // process each line of the file
    while(getline(file, line)) {
        stringstream linestream(line);
        string uName, vName;
        double w;

        // put vertex names into uName and vName, weight into w
        linestream >> uName >> vName >> w;

        // iterators to u and v, if in digraph already
        map<string, Vertex>::iterator uIt = V->find(uName);
        map<string, Vertex>::iterator vIt = V->find(vName);

        // if vertices u not in digraph already, insert them
        if (uIt == V->end()) {
            V->operator[](uName) = Vertex(uName);
        }
        if (vIt == V->end()) {
            V->operator[](vName) = Vertex(vName);
        }

        // update vertices u and v with edge u <--(w)--> v
        V->operator[](uName).adj->operator[](vName) = w;
        V->operator[](vName).adj->operator[](uName) = w;
    }
}

// Output of vertex set V with each vertex's adjacency list in alphabetical
// order (maps are sorted already)
void DiGraph::printGraph() {
    map<string, Vertex>::iterator v;
    map<string, double>::iterator e;

    // iterate through vertex set V, printing the name of each vertex
    for (v = V->begin(); v != V->end(); v++) {
        cout << v->first << endl;

        // iterate through adjacency list of current vertex, printing vertex and
        // edge weight
        for (e = v->second.adj->begin(); e != v->second.adj->end(); e++) {
            cout << "    " << e->first << "    " << e->second << endl;
        }
    }
}

// Initialize the digraph for shortest paths from a single source
void DiGraph::initSingleSource(string sName) {
    map<string, Vertex>::iterator v;

    // set all distances to infinity and all predecessors to zero
    for (v = V->begin(); v != V->end(); v++) {
        v->second.dist = numeric_limits<double>::infinity();
        v->second.pred = "Nil";
    }

    // set source's distance to zero
    V->operator[](sName).dist = 0.0;
}

// Relax an edge between V u and v (edge u --(w)--> v)
void DiGraph::relax(string uName, string vName) {
    Vertex u = V->operator[](uName);
    Vertex v = V->operator[](vName);
    double d = u.dist;
    double w = u.adj->operator[](v.name);

    // if v's current distance is bigger than d + w, update v accordingly
    if (d + w < v.dist) {
        V->operator[](v.name).dist = d + w;
        V->operator[](v.name).pred = uName;
    }
}

// Predecessor subtree from single source via Dijkstra's Algorithm
void DiGraph::dijkstra(string from_vert) {
    initSingleSource(from_vert);
    map<string, Vertex>::iterator vIt;
    map<string, double>::iterator adjIt;
    Vertex u;
    MinPriorityQueue Q;

    // add all vertices in G.V to priority queue Q
    for (vIt = V->begin(); vIt != V->end(); vIt++) {
        Q.insert(vIt->second);
    }
    Q.buildMinHeap();

    // Grab the entry with smallest d value in Q, relax edges between it and all
    // vertices in its adjacency list
    while (!Q.isEmpty()) {
        u = Q.extractMin();
        for (adjIt = u.adj->begin(); adjIt != u.adj->end(); adjIt++) {
            relax(u.name, adjIt->first);
        }
    }
}

// Construct path from predecessors
void DiGraph::shortestPath(string from_vert, string to_vert) {
    // run Dijkstra's Algorithm to construct predecessor subtree
    dijkstra(from_vert);
    
    // build path backwards from to_vert using predecessors with two vertices
    // (parent and child) to keep track of current vertex and predecessor and a
    // running total of distance
    vector<string> path;
    double total = 0.0;
    Vertex parentVert;
    Vertex childVert = V->operator[](to_vert);

    // add to_vert to path
    path.push_back(childVert.name);

    // traverse backwards through predecessors until we've built all the way
    // back to from_vert
    while (childVert.pred != "Nil") {
        parentVert = V->operator[](childVert.pred);
        path.push_back(parentVert.name);
        total += parentVert.adj->operator[](childVert.name);
        childVert = parentVert;
    }

    // output path and total distance
    while (path.size() != 0) {
        cout << path.back() << "   ";
        path.pop_back();
    }
    cout << total << endl;
}
