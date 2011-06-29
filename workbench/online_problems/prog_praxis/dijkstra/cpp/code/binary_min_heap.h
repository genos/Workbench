/*
 * binary_min_heap.h----Graham Enos----Fall 2010
 *
 * This file contains the interface to my binary min heap implementation for
 * ITCS 6114's programming project.
 *
 */
#ifndef BINARYMINHEAP_H
#define BINARYMINHEAP_H
#include <vector>
#include "vertex.h"
using namespace std;

// A binary min heap will hold entries in a vector of Vertices; we will subclass
// this with MinPriorityQueue
class BinaryMinHeap {
    public:
        vector<Vertex>* entries;
        BinaryMinHeap();
        vector<Vertex>::size_type parent(vector<Vertex>::size_type i);
        vector<Vertex>::size_type left(vector<Vertex>::size_type i);
        vector<Vertex>::size_type right(vector<Vertex>::size_type i);
        void minHeapify(vector<Vertex>::size_type i);
        void buildMinHeap();
        bool isEmpty();
};

#endif // BINARYMINHEAP_H
