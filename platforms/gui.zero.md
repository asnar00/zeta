# gui
*cross-platform UI primitives*

## specification

Platform functions for user interaction. Implementations vary by target: web (DOM), iOS (UIKit), Android (Views). Zero code calls these without knowing which platform is underneath.

## interface

Show a text input and wait for the user to submit a value:

    on (string result) = input (string prompt)

Set a browser cookie (or equivalent session token on native):

    on set cookie of (string name) to (string value)

Reload the current page (or refresh the current view on native):

    on reload page ()
