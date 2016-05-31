#include <windows.h>
#include <string.h>

WORD KEYEVENTF_KEYDOWN = 0;

LPCTSTR GetClipboard();
void    SetClipboard(LPCTSTR);

UINT SendUnicodeChar(const wchar_t);

UINT PressKeyDown(WORD);
UINT PressKeyUp(WORD);

UINT ClickMouseAt(int x, int y, int n, DWORD buttonDown, DWORD buttonUp);

HINSTANCE OpenApplication(LPCTSTR);
HINSTANCE OpenUrl(LPCTSTR);

BOOL EnableDebugPriv();
