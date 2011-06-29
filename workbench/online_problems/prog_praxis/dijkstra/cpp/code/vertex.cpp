/*
 * vertex.cpp----Graham Enos----Fall 2010
 *
 * This file contains my vertex implementation for ITCS 6114's programming
 * project.
 *
 */

#include "vertex.h"
using namespace std;

// Constructor
Vertex::Vertex() {
    dist = numeric_limits<double>::infinity();
    name = ' ';
    pred = "Nil";
    adj = new map<string, double>();
}

// Named constructor
Vertex::Vertex(string n) {
    dist = numeric_limits<double>::infinity();
    name = n;
    pred = "Nil";
    adj = new map<string, double>();
}

// We'll compare vertices by their distance
bool Vertex::operator< (Vertex other) {
    return dist < other.dist;
}
