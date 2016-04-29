#include <windows.h>
#include <string.h>

LPCTSTR GetClipboard();
void SetClipboard(LPCTSTR);

UINT InsertUnicodeChar(const wchar_t c);
