/*
 * binary_min_heap.cpp----Graham Enos----Fall 2010
 *
 * This file contains my binary min heaps implementation for ITCS 6114's
 * programming project.
 *
 */
#include "binary_min_heap.h"
using namespace std;

// Constructor
BinaryMinHeap::BinaryMinHeap() {
    entries = new vector<Vertex>();
}

// Helper functions, access tree indices in vector of entries; changed from our
// textbook to account for C++'s zero-based indexing and to make use of inherent
// speed of bitwise operations
vector<Vertex>::size_type BinaryMinHeap::parent(vector<Vertex>::size_type i) {
    return (i - 1) >> 1;
}

vector<Vertex>::size_type BinaryMinHeap::left(vector<Vertex>::size_type i) {
    return (i << 1) + 1;
}

vector<Vertex>::size_type BinaryMinHeap::right(vector<Vertex>::size_type i) {
    return (i + 1) << 1;
}

// Works to maintain the min heap property; symmetric to CLRS3e's Max-Heapify
void BinaryMinHeap::minHeapify(vector<Vertex>::size_type i) {
        vector<Vertex>::size_type l = left(i);
        vector<Vertex>::size_type r = right(i);
        vector<Vertex>::size_type smallest;

        // determine the smallest of entries[i], entries[l], and entries[r]
        if ((l < entries->size()) && (entries->at(l) < entries->at(i))) {
            smallest = l;
        } else {
            smallest = i;
        }

        if ((r < entries->size()) && (entries->at(r) < entries->at(smallest))) {
            smallest = r;
        }

        if (smallest != i) {
            // swap entries[i] <--> entries[smallest], continue minHeapifying
            Vertex tmp = entries->at(smallest);
            entries->at(smallest) = entries->at(i);
            entries->at(i) = tmp;
            minHeapify(smallest);
        }
}

// Constructs min heap; symmetric to CLRS3e's Build-Max-Heap
void BinaryMinHeap::buildMinHeap() {
    vector<Vertex>::size_type i;

    // starting at last parent node, minHeapify all the way down to first entry
    for (i = parent(entries->size() - 1) ; i > 0; i--) {
        minHeapify(i);
    }
    minHeapify(0);
}

// Check if the heap is empty; used as the condition of a while loop in DiGraph
bool BinaryMinHeap::isEmpty() {
    return entries->size() == 0;
}
