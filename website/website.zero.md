# website
*the noob landing page*

## specification

Serves the noob logo on the test domain. Prints the logo on startup, logs each request path to the terminal, and responds to each request.

## interface

    website () => "listening on 8084"

## definition

    feature website

    use terminal.out$
    use http.request$, http.response$

    int port = 8084
    string logo = "ᕦ(ツ)ᕤ"

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
