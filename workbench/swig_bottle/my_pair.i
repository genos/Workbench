 // my_pair.i - SWIG interface
 %module my_pair
 %{
#include "my_pair.h"
 %}
 
 
 // Parse the original header file
 %include "my_pair.h"
 
 // Instantiate some templates
 
 %template(mp_ii) my_ns::my_pair<int,int>;
 %template(mp_di) my_ns::my_pair<double,int>;
