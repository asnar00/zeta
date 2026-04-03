# website
*the nøøb website*

## specification

Serves the nøøb logo on the test domain. Prints the logo on startup, logs each request path to the terminal, and responds to each request via `handle request`.

## interface

The `handle request` function takes an Http-Request and returns a response body. Without other features composed, it returns the logo for all paths:

    handle request (Http-Request(path="/")) => "ᕦ(ツ)ᕤ"
    handle request (Http-Request(path="/nope")) => "ᕦ(ツ)ᕤ"

The `stop` function is a lifecycle hook for cleanup before shutdown.

## definition

    feature website

Bind platform streams for terminal output and HTTP:

    use terminal.out$
    use http.request$, http.response$

Server configuration and branding:

    shared int port = 8084
    shared string logo = "ᕦ(ツ)ᕤ"

The main entry point: print the logo, start the HTTP server, and process requests in a loop:

    on main (string args$)
        out$ <- logo
        Http-Request request$ <- serve http (port)
        for each (request) in (request$)
            out$ <- request.path
            string body = handle request (request)
            response$ <- Http-Response(request, body)

Default request handler — returns the logo for all paths. Other features override this via `replace` or `before`:

    on (string body) = handle request (Http-Request request)
        body = logo

Lifecycle hook called before shutdown. Features can extend this to clean up resources:

    on stop ()
        print ("stopping")
