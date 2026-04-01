# admin
*RPC endpoint for runtime evaluation*

## specification

Adds a `before` extension to `handle request` that intercepts `/@rpc/` paths and evaluates zero expressions at runtime. Non-RPC paths fall through to the next handler.

## interface

Get a variable: `/@rpc/port` returns `"8084"`.

Set a variable: `/@rpc/landing-page-enabled = false` returns `"landing-page-enabled = false"`.

Call a function: `/@rpc/not found ()` returns `"not found"`.

List everything: `/@rpc/` returns a directory of features, variables, and functions.

## definition

    feature admin extends website

    before (string body) = handle request (http-request request)
        if ((request.path) starts with ("/@rpc/"))
            string expr = substring of (request.path) from (6)
            body = rpc eval (expr)
