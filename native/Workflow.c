#define _UNICODE
#define UNICODE

// package
//#include "Workflow.h"

// windows
#include <windows.h>

// C
#include <string.h>


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

UINT InsertUnicodeChar(const wchar_t c) {
	UINT successes = 0;

	INPUT i;
	i.type = INPUT_KEYBOARD;
	i.ki.time = 0;
	i.ki.dwExtraInfo = 0;
	i.ki.wVk = 0;

	i.ki.dwFlags = KEYEVENTF_UNICODE; // Specify the key as a unicode character
	i.ki.wScan = c; //
	successes += SendInput(1, &i, sizeof(INPUT));

	return successes;
}
