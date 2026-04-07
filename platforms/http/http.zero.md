# http
*http server*

## specification

HTTP server platform. `serve http` listens on a port and produces a stream of requests. `response$` is a stream of responses paired to requests by the `http-response` type.

## interface

    type Http-Request
        string path = ""
        string method = ""
        string token = ""

    type Http-Response
        Http-Request request
        string body = ""

    Http-Response response$

    on input (Http-Request request$) <- serve http (int port)
