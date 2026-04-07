# gui
*cross-platform UI primitives*

@client

## specification

Platform functions for user interaction. Implementations vary by target: web (DOM), iOS (UIKit), Android (Views). Zero code calls these without knowing which platform is underneath.

User interactions are input streams. Actions (set cookie, click, etc.) are output functions.

## interface

User text submissions (arrives when the user types and hits enter):

    input string user-text$

User choice selections (arrives when the user picks a button):

    input string user-choice$

Browser cookies (keyed by name, read on demand):

    input string cookie$[string]

Show a text input and wait for the user to submit a value:

    on (string result$) <- input (string prompt)

Show a choice dialog with labelled buttons, return the chosen label:

    on (string choice$) <- choose (string option-a) or (string option-b)

Set a browser cookie (or equivalent session token on native):

    on set cookie of (string name) to (string value)

Show a message to the user:

    on show message (string text)

Remove a cookie:

    on clear cookie (string name)

Reload the current page (or refresh the current view on native):

    on reload page ()

Describe the full page state — every visible element with position, size, styles, and content:

    on (string snapshot) = describe page ()

Click on an element:

    on click on (string selector)

Type text into an input element:

    on type (string text) into input box (string selector)

Press a key on an element:

    on press (string key) on (string selector)
