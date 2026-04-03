# not found
*default 404 response*

## specification

Replaces the default `handle request` to return "not found" for all paths.

## interface

The `not found` function returns the default 404 body:

    not found () => "not found"

After composition, all paths return "not found":

    handle request (Http-Request(path="/")) => "not found"
    handle request (Http-Request(path="/nope")) => "not found"

## definition

    feature not-found extends website

Replace the default request handler to delegate to `not found` for all paths:

    replace (string body) = handle request (Http-Request request)
        body = not found ()

The default 404 response body:

    on (string body) = not found ()
        body = "not found"
