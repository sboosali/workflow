#import <Cocoa/Cocoa.h>

#import "Workflow.h"

////////////////////////////////////////////////////////////////////////////////
// private helpers

/* Event taps


   enum CGEventTapLocation {
   kCGHIDEventTap = 0,
   kCGSessionEventTap,
   kCGAnnotatedSessionEventTap
   };

   kCGHIDEventTap Specifies that an event tap is placed at the point where HID system events enter the window server.

   kCGSessionEventTap Specifies that an event tap is placed at the point where HID system and remote control events enter a login session.

   kCGAnnotatedSessionEventTap Specifies that an event tap is placed at the point where session events have been annotated to flow to an application.

 */


/* enum CGEventSourceStateID {
   kCGEventSourceStatePrivate = -1,
   kCGEventSourceStateCombinedSessionState = 0,
   kCGEventSourceStateHIDSystemState = 1
   };

   kCGEventSourceStatePrivate
   Specifies that an event source should use a private event state table.

   kCGEventSourceStateCombinedSessionState
   Specifies that an event source should use the event state table that reflects the combined state of all event sources posting to the current user login session.

   kCGEventSourceStateHIDSystemState
   Specifies that an event source should use the event state table that reflects the combined state of all hardware event sources posting from the HID system.

   Two pre-existing event state tables are defined:

   The kCGEventSourceStateCombinedSessionState table reflects the combined state of all event sources posting to the current user login session. If your program is posting events from within a login session, you should use this source state when you create an event source.

   The kCGEventSourceStateHIDSystemState table reflects the combined state of all hardware event sources posting from the HID system. If your program is a daemon or a user space device driver interpreting hardware state and generating events, you should use this source state when you create an event source.

*/

/* appInfo

 // NSLog(@"%@", appInfo);
  NSDictionary: {
  NSApplicationBundleIdentifier = "org.gnu.Emacs";
  NSApplicationName = Emacs;
  NSApplicationPath = "/Applications/Notes.app";
  NSApplicationProcessIdentifier = 40831;
  NSApplicationProcessSerialNumberHigh = 0;
  NSApplicationProcessSerialNumberLow = 4195328;
  NSWorkspaceApplicationKey = "<NSRunningApplication: 0x7fe3c0e08b30 (org.gnu.Emacs - 40831)>";
  }
*/


/* ProcessSerialNumber
https: //developer.apple.com/legacy/library/documentation/Carbon/Reference/Process_Manager/index.html#//apple_ref/doc/c_ref/ProcessSerialNumber

struct ProcessSerialNumber { unsigned long highLongOfPSN; unsigned long lowLongOfPSN; };
typedef  struct ProcessSerialNumber  ProcessSerialNumber;
typedef  ProcessSerialNumber*        ProcessSerialNumberPtr;
*/


/* CGEventPostToPSN
https://developer.apple.com/library/mac/documentation/Carbon/Reference/QuartzEventServicesRef/index.html

void CGEventPostToPSN ( void *processSerialNumber, CGEventRef event );
*/


NSString* fromUTF8(const char* s) {
 return [[NSString alloc]
          initWithCString:s encoding:NSUTF8StringEncoding];
}

const char* toUTF8(NSString* s) {
  return [s cStringUsingEncoding:NSUTF8StringEncoding];
}

void printPSN(ProcessSerialNumber psn) {
  NSLog(@"ProcessSerialNumber: {%d,%d}", psn.highLongOfPSN, psn.lowLongOfPSN);
}

void printApplications() {
  NSLog(@"launchedApplications: %@", [[NSWorkspace sharedWorkspace] launchedApplications]);
}

// I think that uninitialized ProcessSerialNumber is always {0,0}.
// I *hope* it doesn't crash CGEventPostToPSN (hasn't yet) and that no application has both fields coinciding.
ProcessSerialNumber nullProcessSerialNumber = {0,0};

// returns pseudo-Just a pointer to a heap-allocated structure, must be freed.
// pseudo-Nothing if no application by that path is running
// TODO or pointer to a pointer to a structure (without a pointer can be null)?
// from full path or from name
ProcessSerialNumber* getApplicationPSN(const char* s) {
  // no maybe type...
  ProcessSerialNumber* psn = (ProcessSerialNumber*)malloc(sizeof(ProcessSerialNumber));
  *psn = nullProcessSerialNumber; // http://stackoverflow.com/questions/9127246/copy-struct-to-struct-in-c

  // existentially-quantified arrays...
  NSArray* appInfos = [[NSWorkspace sharedWorkspace] launchedApplications];  // runningApplications

  for (NSDictionary* appInfo in appInfos) {
    if (0 == strcmp(s, toUTF8([appInfo objectForKey:@"NSApplicationPath"]))) {
      psn->highLongOfPSN = [[appInfo objectForKey:@"NSApplicationProcessSerialNumberHigh"] unsignedIntValue];
      psn->lowLongOfPSN  = [[appInfo objectForKey:@"NSApplicationProcessSerialNumberLow"]  unsignedIntValue];
      return psn;
    }
  }

  for (NSDictionary* appInfo in appInfos) {
    if (0 == strcmp(s, toUTF8([appInfo objectForKey:@"NSApplicationName"]))) {
      psn->highLongOfPSN = [[appInfo objectForKey:@"NSApplicationProcessSerialNumberHigh"] unsignedIntValue];
      psn->lowLongOfPSN  = [[appInfo objectForKey:@"NSApplicationProcessSerialNumberLow"]  unsignedIntValue];
      return psn;
    }
  }

  return psn;
}

ProcessSerialNumber currentApplicationPSN() {
  NSDictionary *appInfo = [[NSWorkspace sharedWorkspace] activeApplication];  // frontmostApplication
  ProcessSerialNumber psn;
  psn.highLongOfPSN = [[appInfo objectForKey:@"NSApplicationProcessSerialNumberHigh"] unsignedIntValue];
  psn.lowLongOfPSN  = [[appInfo objectForKey:@"NSApplicationProcessSerialNumberLow"]  unsignedIntValue];
  return psn;
}

//
NSArray<NSDictionary*>* getApplications() {
  return [[NSWorkspace sharedWorkspace] launchedApplications];  // runningApplications
}

NSDictionary* getCurrentApplication () {
  return [[NSWorkspace sharedWorkspace] activeApplication]; // frontmostApplication
}

////////////////////////////////////////////////////////////////////////////////
// public

const char* currentApplicationPath() {
  return toUTF8([[[NSWorkspace sharedWorkspace]
   activeApplication] // frontmostApplication
    objectForKey:@"NSApplicationPath"]);
}

// can be full path (e.g. "/Applications/Work.app"), or just the name (e.g. "Work")
void openApplication(const char* s) {
  [[NSWorkspace sharedWorkspace] launchApplication:fromUTF8(s)]; // TODO Is this freed Once the block exits? Like a local declaration.
}

// simulates a key press from the keyboard. works in pop-ups like Alfred.
void pressKey(CGEventFlags modifiers, CGKeyCode key) {
  // NSLog(@"pressKey");         // debug
    CGEventSourceRef source = CGEventSourceCreate(kCGEventSourceStateHIDSystemState); // kCGEventSourceStateCombinedSessionState

    // events to press a key
    CGEventRef event1 = CGEventCreateKeyboardEvent(source, key, true);  // key down
    CGEventRef event2 = CGEventCreateKeyboardEvent(source, key, false); // key up

    // add modifiers to event
    CGEventSetFlags(event1, modifiers);
    CGEventSetFlags(event2, modifiers);

    // send a keyboard event (a quartz event) "globally"
    CGEventPost(kCGHIDEventTap, event1); // kCGSessionEventTap
    CGEventPost(kCGHIDEventTap, event2); // kCGSessionEventTap

 // free memory
 CFRelease(event1);
 CFRelease(event2);
 CFRelease(source);
}

void pressKeyDown (CGEventFlags modifiers, CGKeyCode key) {
  // NSLog(@"pressKey");         // debug
    CGEventSourceRef source = CGEventSourceCreate(kCGEventSourceStateHIDSystemState); // kCGEventSourceStateCombinedSessionState

    // events to press a key
    CGEventRef event = CGEventCreateKeyboardEvent(source, key, true);  // key down

    // add modifiers to event
    CGEventSetFlags(event, modifiers);

    // send a keyboard event (a quartz event) "globally"
    CGEventPost(kCGHIDEventTap, event); // kCGSessionEventTap

 // free memory
 CFRelease(event);
 CFRelease(source);
}

void pressKeyUp (CGEventFlags modifiers, CGKeyCode key) {
  // NSLog(@"pressKey");         // debug
    CGEventSourceRef source = CGEventSourceCreate(kCGEventSourceStateHIDSystemState); // kCGEventSourceStateCombinedSessionState

    // events to press a key
    CGEventRef event = CGEventCreateKeyboardEvent(source, key, false); // key up

    // add modifiers to event
    CGEventSetFlags(event, modifiers);

    // send a keyboard event (a quartz event) "globally"
    CGEventPost(kCGHIDEventTap, event); // kCGSessionEventTap

 // free memory
 CFRelease(event);
 CFRelease(source);
}

// may take up to 15s (!) when the keypresses sent to and after besides the one that's running the server
void pressKeyToCurrentApplication(CGEventFlags modifiers, CGKeyCode key) {
  pressKeyTo(modifiers, key, currentApplicationPSN());
}

// send a keypress to a specific process/application
void pressKeyTo(CGEventFlags modifiers, CGKeyCode key, ProcessSerialNumber psn) {

    // events to press a key
    CGEventRef event1 = CGEventCreateKeyboardEvent(NULL, key, true);  // key down
    CGEventRef event2 = CGEventCreateKeyboardEvent(NULL, key, false); // key up

    // add modifiers to event
    CGEventSetFlags(event1, modifiers);
    CGEventSetFlags(event2, modifiers);

    // send keyboard event to application process (a quartz event)
    CGEventPostToPSN(&psn, event1);
    CGEventPostToPSN(&psn, event2);

 // free memory
 CFRelease(event1);
 CFRelease(event2);
 // psn is a struct, that's been passed by value
}

const char* getClipboard() {
 return [[[NSPasteboard generalPasteboard]
   stringForType:NSStringPboardType]
    cStringUsingEncoding:NSUTF8StringEncoding];
}

void setClipboard(const char* contents) {
 [[NSPasteboard generalPasteboard] clearContents];
 [[NSPasteboard generalPasteboard] setString:fromUTF8(contents) forType:NSStringPboardType];
}

void openURL(const char* url) {
 [[NSWorkspace sharedWorkspace]
   openURL:[NSURL URLWithString: fromUTF8(url)]];
}

/*

http://stackoverflow.com/questions/1117065/cocoa-getting-the-current-mouse-position-on-the-screen

https://developer.apple.com/library/mac/documentation/Carbon/Reference/QuartzEventServicesRef
CGEventRef CGEventCreateMouseEvent ( CGEventSourceRef source, CGEventType mouseType, CGPoint mouseCursorPosition, CGMouseButton mouseButton );

 CGPointMake(CGFloat x, CGFloat y)
 {
 CGPoint p; p.x = x; p.y = y; return p;
 }

 */
void clickMouseAt

(CGEventFlags modifiers,
 UInt32 numClicks,
 CGMouseButton mouseButton, CGEventType mouseDown, CGEventType mouseUp,
 CGFloat x, CGFloat y) {

    CGPoint p = CGPointMake(x,y);

    CGEventRef eventDown = CGEventCreateMouseEvent(NULL, mouseDown, p, mouseButton);
    CGEventRef eventUp   = CGEventCreateMouseEvent(NULL, mouseUp,   p, mouseButton);

    // hold down modifiers
    CGEventSetFlags(eventDown, modifiers);
    CGEventSetFlags(eventUp,   modifiers);

    // click numClicks times. each click must say which it is (1st, 2nd, 3rd, etc)

    for (int nthClick=1; nthClick<=numClicks; nthClick++) {
        CGEventSetIntegerValueField(eventDown, kCGMouseEventClickState, nthClick);
        CGEventPost(kCGHIDEventTap, eventDown);

        CGEventSetIntegerValueField(eventUp,   kCGMouseEventClickState, nthClick);
        CGEventPost(kCGHIDEventTap, eventUp);
        [NSThread sleepForTimeInterval:0.30f]; //30ms
    }

    // free memory
    CFRelease(eventDown);
    CFRelease(eventUp);
}

void clickMouse
(CGEventFlags modifiers, UInt32 numClicks, CGMouseButton mouseButton, CGEventType mouseDown, CGEventType mouseUp) {
    CGPoint p;
    getCursorPosition(&p);
    clickMouseAt(modifiers,  numClicks,  mouseButton,  mouseDown,  mouseUp, p.x,  p.y);
}

void getCursorPosition(CGPoint* p) {
  *p = CGEventGetLocation(CGEventCreate(NULL));
}

/*
  CGEventCreateScrollWheelEvent
*/
void scrollMouse() {
}

/*
  http://stackoverflow.com/questions/8059667/set-the-mouse-location
*/
void setCursorPosition(CGFloat x, CGFloat y) {
  // CGPoint p;
  // p.x = x;
  // p.y = y;

  CGEventSourceRef source = CGEventSourceCreate(kCGEventSourceStateCombinedSessionState);
  CGEventRef mouse = CGEventCreateMouseEvent (NULL, kCGEventMouseMoved, CGPointMake(x,y), 0);

  CGEventPost(kCGHIDEventTap, mouse);

  CFRelease(mouse);
  CFRelease(source);
}

/*
 CGEventCreateKeyboardEvent(CGEventSourceRef __nullable source, CGKeyCode virtualKey, bool keyDown)

 */
void sendUnichar (unichar c) {
    CGEventSourceStateID state = kCGEventSourceStateCombinedSessionState;

    CGEventSourceRef source = CGEventSourceCreate(state);
    CGEventRef down = CGEventCreateKeyboardEvent(source, 0, true);
    CGEventRef up   = CGEventCreateKeyboardEvent(source, 0, false);

    CGEventTapLocation tap = kCGSessionEventTap;

    CGEventKeyboardSetUnicodeString(down, 1, &c);
    CGEventPost(tap, down);
    CGEventKeyboardSetUnicodeString(up, 1, &c);
    CGEventPost(tap, up);

    CFRelease(down);
    CFRelease(up);
}
