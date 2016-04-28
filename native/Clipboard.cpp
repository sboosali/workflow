#define _UNICODE
#define UNICODE

// package
#include "Clipboard.h"

// windows
#include <windows.h>

// C++
#include <string>


/*

somehow, 0x3f ~ half a NULL

lstrlen returns numbe of chars, not bytes.

sizeof(wchar_t) = 2

*/
void SetClipboard(LPCTSTR output) {
	const size_t len = (lstrlen(output) + 1) * sizeof(wchar_t); // was halving
	HGLOBAL hMem = GlobalAlloc(GMEM_MOVEABLE, len);
	memcpy(GlobalLock(hMem), output, len);
	GlobalUnlock(hMem);
	OpenClipboard(0);
	EmptyClipboard();
	SetClipboardData(CF_UNICODETEXT, hMem);
	CloseClipboard();
}


/*
if (!IsClipboardFormatAvailable(CF_TEXT)) return;

https://msdn.microsoft.com/en-us/library/ff729168(v=vs.85).aspx

EnumClipboardFormats

Depends if it is Unicode or not it appears. LPTSTR is char* if not Unicode, or w_char* if so.


*/
LPCTSTR GetClipboard()
{
	LPCTSTR _contents;
	OpenClipboard(0);

	if (IsClipboardFormatAvailable(CF_UNICODETEXT)) {
		HGLOBAL hMem = GetClipboardData(CF_UNICODETEXT);  // TODO CF_UNICODETEXT
		_contents = (LPCTSTR)GlobalLock(hMem); // lpctstr or lpcwstr?
		GlobalUnlock(hMem);
	}
	else if (IsClipboardFormatAvailable(CF_TEXT)) {
		// or ascii, and convert to widechar
		_contents = TEXT("");
	}
	else
	{
		_contents = TEXT("");
	}

	CloseClipboard();
	return _contents;
}
