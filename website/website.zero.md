# website
*the noob landing page*

## specification

Serves the noob logo on the test domain. Prints the logo on startup, logs each request path to the terminal, and responds with the logo.

## interface

    website () => "listening on 8084"

## definition

    feature website

    use terminal.out$
    use http.request$, http.response$

    string logo = "ᕦ(ツ)ᕤ"

    on main (string args$)
        out$ <- logo
        http_request request$ <- serve http (8084)
        for each (request) in (request$)
            out$ <- request.path
            response$ <- http_response(request, logo)
