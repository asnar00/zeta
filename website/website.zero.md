# website
*the nøøb website*

## specification

Serves the nøøb logo on the test domain. Prints the logo on startup, logs each request path to the terminal, and responds to each request via `handle request`.

## interface

The `handle request` function takes an http-request and returns a response body. Without other features composed, it returns the logo for all paths:

    handle request (http-request(path="/")) => "ᕦ(ツ)ᕤ"
    handle request (http-request(path="/nope")) => "ᕦ(ツ)ᕤ"

The `stop` function is a lifecycle hook for cleanup before shutdown.

## definition

    feature website

    use terminal.out$
    use http.request$, http.response$

    shared int port = 8084
    shared string logo = "ᕦ(ツ)ᕤ"

    on main (string args$)
        out$ <- logo
        http-request request$ <- serve http (port)
        for each (request) in (request$)
            out$ <- request.path
            string body = handle request (request)
            response$ <- http-response(request, body)

    on (string body) = handle request (http-request request)
        body = logo

    on stop ()
        print ("stopping")
