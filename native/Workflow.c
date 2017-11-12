#define _UNICODE
#define UNICODE

// package
#include "Workflow.h"

// windows
#include <windows.h>
#include <stdio.h>
// #include <dxva2.h>

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

  // "INPUT_KEYBOARD supports nonkeyboard-input methods
	// — such as handwriting recognition or voice recognition —
	// as if it were text input by using the KEYEVENTF_UNICODE flag."
	i.ki.dwFlags = KEYEVENTF_UNICODE; // Specify the key as a unicode character
	i.ki.wScan = c; //
	successes += SendInput(1, &i, sizeof(INPUT));

	return successes;
}

/* 40 */
size_t SizeOfInput () {
	return sizeof(INPUT); //LOL
}

/* 2 */
size_t SizeOfWideChar () {
	return sizeof(wchar_t);
}

/* we can't know which characters succeeded, we can only count the successes.

the characters array is initialized, while the`inputs` is uninitialized.
they both share the same length, `size`.

SendUnicodeString(k, &cs, &is)

*/
UINT SendUnicodeString (const int length, LPCWSTR characters, LPINPUT inputs) {
	UINT successes = 0;
	int j;

	for (int i = 0; i < length; i++) {
		j = i * sizeof(INPUT); // =40
		// identical initialization
		inputs[j].type = INPUT_KEYBOARD;
		inputs[j].ki.time = 0;
		inputs[j].ki.dwExtraInfo = 0;
		inputs[j].ki.wVk = 0;
		inputs[j].ki.dwFlags = KEYEVENTF_UNICODE; // Specify the key as a unicode character
		// differs per item
		inputs[j].ki.wScan = characters[i * sizeof(wchar_t)]; // =2
	}

	successes += SendInput(length, inputs, sizeof(INPUT));
	return successes;
}

/* "The virtual key value of a key may alter depending on the current keyboard
layout or what other keys were pressed, but the scan code will always be the
same."

*/

/* depress a key


TODO
    //This let's you do a hardware scan instead of a virtual keypress
    ip.ki.dwFlags = KEYEVENTF_SCANCODE;
    ip.ki.wScan = 0x1E;  //Set a unicode character to use (A)

*/
UINT PressKeyDown(WORD key) {
	INPUT i;
	i.type = INPUT_KEYBOARD;
	i.ki.wScan = 0;
	i.ki.time = 0;
	i.ki.dwExtraInfo = 0;

	i.ki.dwFlags = KEYEVENTF_KEYDOWN;
	i.ki.wVk = key;
	return SendInput(1, &i, sizeof(INPUT));
}

/* release a key

*/
UINT PressKeyUp(WORD key) {
	INPUT i;
	i.type = INPUT_KEYBOARD;
	i.ki.wScan = 0;
	i.ki.time = 0;
	i.ki.dwExtraInfo = 0;

	i.ki.dwFlags = KEYEVENTF_KEYUP;
	i.ki.wVk = key;
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


// If the function succeeds, the return value is nonzero.
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

/* conflicts with the Haskell programs main, I think
int main() {
  printf("testing Windows workflows...\n");
	SendUnicodeChar(97);
  SendUnicodeChar(10); // here, at the lowest binding, the newline is dropped
  return 0;
}
*/

/*

Monitor Monitor Configuration Functions
https://msdn.microsoft.com/en-us/library/ms775232.aspx
https://msdn.microsoft.com/en-us/library/ms775249.aspx

BOOL SetMonitorRedGreenOrBlueGain(
  _In_  HANDLE hMonitor,
  _In_  MC_GAIN_TYPE gtGainType,
  _In_  DWORD dwNewGain
);

HMONITOR MonitorFromWindow(
  _In_ HWND  hwnd,
  _In_ DWORD dwFlags
);

BOOL GetPhysicalMonitorsFromHMONITOR(
  _In_   HMONITOR hMonitor,
  _In_   DWORD dwPhysicalMonitorArraySize,
  _Out_  LPPHYSICAL_MONITOR pPhysicalMonitorArray
);

https://msdn.microsoft.com/en-us/library/windows/desktop/dd692950(v=vs.85).aspx

typedef enum _MC_GAIN_TYPE {
  MC_RED_GAIN    = 0,
  MC_GREEN_GAIN  = 1,
  MC_BLUE_GAIN   = 2
} MC_GAIN_TYPE;

BOOL GetMonitorRedGreenOrBlueGain(
  _In_   HANDLE hMonitor,
  _In_   MC_GAIN_TYPE gtGainType,
  _Out_  LPDWORD pdwMinimumGain,
  _Out_  LPDWORD pdwCurrentGain,
  _Out_  LPDWORD pdwMaximumGain
);

BOOL GetMonitorBrightness(
  _In_   HANDLE hMonitor,
  _Out_  LPDWORD pdwMinimumBrightness,
  _Out_  LPDWORD pdwCurrentBrightness,
  _Out_  LPDWORD pdwMaximumBrightness
);

SetMonitorColor(MC_RED_GAIN, MAXIMUM_RED);
SetMonitorColor(MC_GREEN_GAIN, MINIMUM_GREEN);
SetMonitorColor(MC_BLUE_GAIN, MINIMUM_BLUE);

*/
// BOOL SetMonitorColor( MC_GAIN_TYPE gtGainType, DWORD dwNewGain ) {
// 	HWND hw = GetActiveWindow();
// 	HMONITOR hm = MonitorFromWindow(hw,NULL);
// 	HANDLE h =
// 	SetMonitorRedGreenOrBlueGain(h)
// }
