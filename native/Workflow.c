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
