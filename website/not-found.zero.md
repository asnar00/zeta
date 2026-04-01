# not found
*default 404 response*

## definition

    feature not-found extends website

    replace (string body) = handle request (http-request request)
        body = not found ()

    on (string body) = not found ()
        body = "not found"
