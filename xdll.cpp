//--------------------------------------------------------------------------------------------------
//xdll.cpp
//--------------------------------------------------------------------------------------------------

// compile with g++ -o libxx.so -shared -fPIC -Wl,-soname,libxx.so -L/usr/lib/X11 -I/usr/include/X11 xdll.cpp -lX11

#include <Xlib.h>
#include <stdio.h>

class a{
public:
  a() { XInitThreads(); }
};

a X;


