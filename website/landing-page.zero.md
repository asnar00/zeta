# landing page
*serves the noob landing page at root*

## definition

    feature landing-page extends website

    before (string body) = handle request (http-request request)
        if (request.path == "/")
            body = landing page ()

    on (string body) = landing page ()
        body = read file ("website/index.html")
