# http
*http server*

## specification

HTTP server platform. `serve http` listens on a port and produces a stream of requests. `response$` is a stream of responses paired to requests by the `http-response` type.

## interface

    type http-request
        string path = ""
        string method = ""

    type http-response
        http-request request
        string body = ""

    http-response response$

    on (http-request request$) <- serve http (int port)
