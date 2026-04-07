# gui
*cross-platform UI primitives*

@client

## specification

Platform functions for user interaction. Implementations vary by target: web (DOM), iOS (UIKit), Android (Views). Zero code calls these without knowing which platform is underneath.

## interface

Show a text input and wait for the user to submit a value:

    on input (string result) = input (string prompt)

Set a browser cookie (or equivalent session token on native):

    on set cookie of (string name) to (string value)

Show a message to the user:

    on show message (string text)

Get a cookie value (returns "" if not set):

    on input (string value) = get cookie (string name)

Remove a cookie:

    on clear cookie (string name)

Show a choice dialog with labelled buttons, return the chosen label:

    on input (string choice) = choose (string option-a) or (string option-b)

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

## integration tests

Input shows a labelled text field, returns the typed value, then removes itself:

    call input ("name")
    check element "div" contains text "name"
    check element "input" exists
    check element "input" is focused
    type "alice" into element "input"
    press "Enter" on element "input"
    check result is "alice"
    check element "input" does not exist

Input works with an empty submission:

    call input ("code")
    press "Enter" on element "input"
    check result is ""

Show message displays an alert dialog:

    call show message ("hello")
    check alert appears with text "hello"

Set cookie stores a value accessible to subsequent requests:

    call set cookie of ("test") to ("abc")
    check cookie "test" is "abc"

Reload page triggers a page navigation:

    call reload page ()
    check page reloads
