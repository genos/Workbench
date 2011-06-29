/*
 * vertex.h----Graham Enos----Fall 2010
 *
 * This file contains the interface to my vertices implementation for ITCS
 * 6114's programming project.
 *
 */

#ifndef VERTEX_H
#define VERTEX_H
#include <limits>
#include <map>
#include <string>
using namespace std;

// Vertices consist of distance (double), predecessor (pointer to vertex), and
// adjacency list (map of string --> weight)
class Vertex {
    public:
        double dist;
        string name;
        string pred;
        map<string, double>* adj;
        Vertex();
        Vertex(string n);
        bool operator< (Vertex);
};

#endif // VERTEX_H
