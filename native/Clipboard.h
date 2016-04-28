#ifndef UNICODE
 #undef _UNICODE
#else
 #ifndef _UNICODE
  #define _UNICODE
 #endif
#endif

// windows
#include <windows.h>

// C++
#include <string>

extern "C" LPCTSTR GetClipboard();
extern "C" void SetClipboard(LPCTSTR);
