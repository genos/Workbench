/*
 * graph.cpp----Graham Enos----Fall 2010
 *
 * This is the __MAIN__ file for my ITCS 6114 programming project.
 *
 */
#include <iostream>
#include <map>
#include <string>
#include "digraph.h"
#include "vertex.h"
using namespace std;

// Driver code; gets input from user, executes appropriate choice
bool loop(DiGraph G) {
    string choice, from_vert, to_vert;
    cout << "\nPlease choose an option:" << endl;
    cout << "For the shortest path between from_vert and to_vert, enter\n"
         << "\tpath from_vert to_vert" << endl;
    cout << "To see the complete contents of the graph, enter\n\tprint" << endl;
    cout << "To stop execution, enter\n\tquit" << endl;
    cout << "Your choice?\t";
    cin >> choice;

    // Run user's choice of action
    if (choice == "path") {
        cin >> from_vert >> to_vert;
        cout << endl;

        // Check input, to see if both vertices are in digraph
        map<string, Vertex>::iterator fromIt = G.V->find(from_vert);
        map<string, Vertex>::iterator toIt = G.V->find(to_vert);
        if (fromIt == G.V->end()) {
            cout << "Sorry, from_vert not valid." << endl;
        } else if (toIt == G.V->end()) {
            cout << "Sorry, to_vert not valid." << endl;
        } else {

            // Shortest path, via Dijkstra's Algorithm
            G.shortestPath(from_vert, to_vert);
            cout << endl;
        }
        return 1;
    } else if (choice == "print") {
        cout << endl;

        // Output graph, iterating alphabetically through vertices and their
        // adjacency lists
        G.printGraph();
        cout << endl;
        return 1;
    } else {

        // Also catches unidentified choices, not just "quit"
        cout << endl;
        return 0;
    }
}


int main(int argc, char* argv[]) {
    DiGraph G;
    bool continue_flag = 1;

    cout << "Building graph from input file, please wait..." << endl;
    G.buildFromFile(argv[1]);

    // continue looping until user halts execution
    while (continue_flag) {
        continue_flag = loop(G);
    }

    return 0;
}
