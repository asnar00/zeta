# landing page
*serves the noob landing page at root*

## specification

Adds a `before` extension to `handle request` that serves the landing page HTML at the root path. Non-root paths fall through to the next handler.

## interface

The `landing page` function returns the HTML content of `website/index.html`.

After composition, root path returns the landing page, other paths fall through:

    handle request (http-request(path="/")) => read file ("website/index.html")
    handle request (http-request(path="/nope")) => "not found"

## definition

    feature landing-page extends website

    before (string body) = handle request (http-request request)
        if (request.path == "/")
            body = landing page ()

    on (string body) = landing page ()
        body = read file ("website/index.html")
