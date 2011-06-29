/*
 * min_priority_queue.h----Graham Enos----Fall 2010
 *
 * This file contains the interface to my min priority queue implementation
 * for ITCS 6114's programming project.
 *
 */
#ifndef MINPRIORITYQUEUE_H
#define MINPRIORITYQUEUE_H
#include "binary_min_heap.h"
using namespace std;

// A min priority queue as a binary min heap with a few additional procedures
class MinPriorityQueue: public BinaryMinHeap {
    public:
        void decreaseKey(vector<Vertex>::size_type(i));
        void insert(Vertex v);
        Vertex minimum();
        Vertex extractMin();
};

#endif // MINPRIORITYQUEUE_H
