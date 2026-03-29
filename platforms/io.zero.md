# io
*file input/output*

## specification

Basic file I/O operations for reading and writing text files.

## interface

Read the contents of a text file:

    on (string content) = read file (string path)

Write a string to a text file:

    on write file (string path) (string content)

Print a string to standard output:

    on print (string message)
