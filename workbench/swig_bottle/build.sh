#!/bin/zsh
swig -python -c++ -o my_pair_wrap.cc my_pair.i;
g++-4.7 -c -arch i386 -fPIC my_pair_wrap.cc -o my_pair.o;
g++-4.7 -c -arch i386 -fPIC my_pair_wrap.cc -o my_pair_wrap.o \
    -I/Library/Frameworks/Python.framework/Versions/2.7/include/python2.7 \
    -L/Library/Frameworks/Python.framework/Versions/2.7/lib/python2.7;
ld -bundle -flat_namespace -undefined suppress -o _my_pair.so my_pair_wrap.o;
echo "Add __str(self)__ methods!"
