/*
 * min_priority_queue.cpp----Graham Enos----Fall 2010
 *
 * This file contains my priority queue implementation (as binary min heaps)
 * for ITCS 6114's programming project.
 *
 */
#include "min_priority_queue.h"
using namespace std;

// Helper function for insertion; symmetric to CLRSE3e's Heap-Increase-Key
void MinPriorityQueue::decreaseKey(vector<Vertex>::size_type i) {
    while ((i > 0) && (entries->at(i) < entries->at(parent(i)))) {
        vector<Vertex>::size_type p = parent(i);
        Vertex tmp = entries->at(p);
        entries->at(p) = entries->at(i);
        entries->at(i) = tmp;
    }
}


// Insert a vertex into the queue
void MinPriorityQueue::insert(Vertex v) {
    // place entry at back of line
    entries->push_back(v);
    decreaseKey(entries->size() - 1);
}

// The minimum element is stored at the front of the queue
Vertex MinPriorityQueue::minimum() {
    if (entries->size() == 0) {
        throw string("ERROR: queue is empty");
    } else {
        return entries->at(0);
    }
}

// Yields the minimum element from the queue, removing it while maintaining the
// min heap property
Vertex MinPriorityQueue::extractMin() {
    if (entries->size() == 0) {
        throw string("ERROR: queue is empty");
    } else {

        // grab min entry from front, bring last element forward, re-heapify
        Vertex v = entries->front();
        entries->front() = entries->back();
        entries->pop_back();
        minHeapify(0);
        return v;
    }
}
