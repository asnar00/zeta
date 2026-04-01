# admin
*RPC endpoint for runtime evaluation*

## specification

Intercepts `/@rpc/` paths and evaluates zero expressions at runtime.

Get a variable:

    /@rpc/port => "8084"
    /@rpc/landing-page-enabled => "true"

Set a variable:

    /@rpc/landing-page-enabled = false => "landing-page-enabled = false"

Call a function:

    /@rpc/stop () => "ok"
    /@rpc/get feature var ("landing-page-enabled") => "true"

## definition

    feature admin extends website

    before (string body) = handle request (http-request request)
        if ((request.path) starts with ("/@rpc/"))
            string expr = substring of (request.path) from (6)
            body = rpc eval (expr)
