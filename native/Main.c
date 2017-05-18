// windows
#include <windows.h>
#include <stdio.h>
#include <Winuser.h>
// C
#include <string.h>

/*
_tmain is a Microsoft extension.

main is, according to the C++ standard, the program's entry point. It has one of these two signatures:

int main();
int main(int argc, char* argv[]);
Microsoft has added a wmain which replaces the second signature with this:

int wmain(int argc, wchar_t* argv[]);_tmain is a Microsoft extension.

main is, according to the C++ standard, the program's entry point. It has one of these two signatures:

int main();
int main(int argc, char* argv[]);
Microsoft has added a wmain which replaces the second signature with this:

int wmain(int argc, wchar_t* argv[]);

---

With _UNICODE defined, _T translates the literal string to the L-prefixed form; otherwise, _T translates the string without the L prefix.

---

http://stackoverflow.com/questions/3571250/wwinmain-unicode-and-mingw/11706847#11706847

-municode defines:

define _UNICODE
define UNICODE

---

extern "C"

---

  wprintf(L"...") with -municode

don't work because

  In function `wmain':
  C:/repo/mingw-w64-crt-git/src/mingw-w64/mingw-w64-crt/crt/crt0_w.c:23: undefined reference to `wWinMain'

---

*/
int main(int argc, char* argv[])
{
    BOOL b = RegisterHotKey(
        NULL,
        1,
        MOD_ALT, // | MOD_NOREPEAT,
        0x42);
    if (b)  //0x42 is 'b'
    {
        printf("Hotkey 'ALT+b' registered, using MOD_NOREPEAT flag\n");
    }

    MSG msg = {0};
    while (GetMessage(&msg, NULL, 0, 0) != 0)
    {
        if (msg.message == WM_HOTKEY)
        {
            printf("WM_HOTKEY received\n");
        }
    }

    return 0;
}

// #define _UNICODE
// #define UNICODE
// #include "Workflow.h"
// #include <stdio.h>
//
// /*
//
// gcc -Wall Main.c -o Main
// Timeout /t 1 >nul
// .\Main.exe
//
// */
// int main() {
//   printf("testing Windows workflows...\n");
//   SendUnicodeChar(10);
//   return 0;
// }
