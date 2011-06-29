# makefile----Graham Enos----Fall 2010
#
# Makefile for my ITCS 6114 programming project
CXX = c++
CFLAGS = -Wall -O2
COMPILE = $(CXX) $(CFLAGS) -c
OBJS = binary_min_heap.o digraph.o graph.o min_priority_queue.o vertex.o

graph: $(OBJS)
	$(CXX) $(OBJS) -o graph 

%.o: %.cpp
	$(COMPILE) -o $@ $<

clean:
	rm $(OBJS)
