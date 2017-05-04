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

wheel is {MOUSEEVENTF_WHEEL, MOUSEEVENTF_HWHEEL}

direction is {+1, -1} i.e. {forwards, backwards} i.e. {towards you, away from you}

TODO why isn't mouseData a signed INT?
> DWORD
> A 32-bit unsigned integer. The range is 0 through 4294967295 decimal.
? This type is declared in IntSafe.h as follows:
> typedef unsigned long DWORD;

*/
UINT ScrollMouseWheel(DWORD wheel, DWORD direction, DWORD distance) {
	// mouse_event(MOUSEEVENTF_WHEEL, 0, 0, direction * distance, 0);

	INPUT i;
	i.type = INPUT_MOUSE;
	i.mi.dx = 0;
	i.mi.dy = 0;
	i.mi.time = 0; // otherwise monitor falls asleep, because the last mouse event is said to be some large number (garbage memory).
	i.mi.dwExtraInfo = 0;

	i.mi.dwFlags = wheel; // MOUSEEVENTF_MOVE | MOUSEEVENTF_ABSOLUTE
	i.mi.mouseData = direction * distance;

	return SendInput(1, &i, sizeof(INPUT));
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


BOOL EnableDebugPriv()
{
	HANDLE hToken;
	LUID luid;
	TOKEN_PRIVILEGES tkp;

	OpenProcessToken(GetCurrentProcess(), TOKEN_ADJUST_PRIVILEGES | TOKEN_QUERY, &hToken);

	LookupPrivilegeValue(NULL, SE_DEBUG_NAME, &luid);

	tkp.PrivilegeCount = 1;
	tkp.Privileges[0].Luid = luid;
	tkp.Privileges[0].Attributes = SE_PRIVILEGE_ENABLED;

	BOOL wasAdjsuted = AdjustTokenPrivileges(hToken, 0, &tkp, sizeof(tkp), NULL, NULL); // ""'false' undeclared"

	CloseHandle(hToken);

	return wasAdjsuted;
}
