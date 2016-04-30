#define _UNICODE
#define UNICODE

// package
#include "Workflow.h"

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

UINT SendUnicodeChar(const wchar_t c) {
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

/*

*/
UINT PressKeyDown(WORD key) {
	INPUT i;
	i.type = INPUT_KEYBOARD;
	i.ki.wScan = 0;
	i.ki.time = 0;
	i.ki.dwExtraInfo = 0;

	i.ki.wVk = key;
	i.ki.dwFlags = KEYEVENTF_KEYDOWN;
	return SendInput(1, &i, sizeof(INPUT));
}

/*

*/
UINT PressKeyUp(WORD key) {
	INPUT i;
	i.type = INPUT_KEYBOARD;
	i.ki.wScan = 0;
	i.ki.time = 0;
	i.ki.dwExtraInfo = 0;

	i.ki.wVk = key;
	i.ki.dwFlags = KEYEVENTF_KEYUP;
	return SendInput(1, &i, sizeof(INPUT));
}

UINT ClickMouseAt(int x, int y, int n, DWORD buttonDown, DWORD buttonUp) {
	UINT successes = 0;

		//TODO? SendMessage(NULL, BM_CLICK, 0, 0);

	const double XSCALEFACTOR = 65535 / (GetSystemMetrics(SM_CXSCREEN) - 1);
	const double YSCALEFACTOR = 65535 / (GetSystemMetrics(SM_CYSCREEN) - 1);

	POINT cursorPos;
	GetCursorPos(&cursorPos);

	double cx = cursorPos.x * XSCALEFACTOR;
	double cy = cursorPos.y * YSCALEFACTOR;

	//TODO why SCALEFACTOR?
	double nx = x * XSCALEFACTOR;
	double ny = y * YSCALEFACTOR;

	INPUT i = { 0 };
	i.type = INPUT_MOUSE;

	i.mi.dx = (LONG)nx;
	i.mi.dy = (LONG)ny;

	i.mi.dwFlags = MOUSEEVENTF_MOVE | MOUSEEVENTF_ABSOLUTE | buttonDown | buttonUp;

	// click mouse down and then up
	for (int j = 0; j < n; j++) {
		successes += SendInput(1, &i, sizeof(INPUT));
	}

	i.mi.dx = (LONG)cx;
	i.mi.dy = (LONG)cy;

	i.mi.dwFlags = MOUSEEVENTF_MOVE | MOUSEEVENTF_ABSOLUTE;

	// move mouse back
	successes += SendInput(1, &i, sizeof(INPUT));

	return successes;
}

/*

uses the PATH?

*/
HINSTANCE OpenApplication(LPCTSTR app) {
	return ShellExecute(NULL, L"open", app, NULL, NULL, SW_SHOW);
}

HINSTANCE OpenUrl(LPCTSTR url) {
	return ShellExecute(NULL, L"open", url, NULL, NULL, SW_SHOWNORMAL);
}
