#!/usr/bin/env python
"""
My submission to Programming Praxis for "Tracking Santa":
http://programmingpraxis.com/2010/12/24/tracking-santa/
To run, save the data.js file in the same directory, delete "var locations = "
from the beginning, then do
./santa data.js

GRE, 12/26/10
"""

import json, sys
from math import atan2, cos, radians, sin, sqrt

def route(file_name):
    """
    Creates a list representing the route given in file_name (in JSON format)
    """
    f = open(file_name)
    route = json.load(f)
    f.close()
    return route

def dist(s, f):
    """
    Calculates the distance in miles between two points on the globe; see
    http://en.wikipedia.org/wiki/Great-circle_distance
    """
    s_lat, s_lng = radians(float(s['lat'])), radians(float(s['lng']))
    f_lat, f_lng = radians(float(f['lat'])), radians(float(f['lng']))
    d_lng = f_lng - s_lng
    numer = sqrt(pow(cos(f_lat) * sin(d_lng), 2) +
                 pow(cos(s_lat) * sin(f_lat) -
                     sin(s_lat) * cos(f_lat) * cos(d_lng), 2))
    denom = sin(s_lat) * sin(f_lat) + cos(s_lat) * cos(f_lat) * cos(d_lng)
    d_sigma = atan2(numer, denom)
    return 3958.761 * d_sigma

def main(f_name):
    """
    The main routine; calculates the distance Santa traveled
    """
    santas_route = route(f_name)
    total = 0
    for i in xrange(len(santas_route) - 1):
        total += dist(santas_route[i], santas_route[i + 1])
    return total

if __name__ == "__main__":
    print main(sys.argv[1])
