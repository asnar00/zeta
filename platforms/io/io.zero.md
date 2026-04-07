# io
*file input/output*

## specification

Basic file I/O operations for reading and writing text files, and printing to standard output.

## interface

Read the contents of a text file:

    on input (string content) = read file (string path)

Write a string to a text file:

    on write (string content) to file (string path)

Print a string to standard output:

    on print (string message)
