Basic service using Twirp.

Example use:

```sh
$ PORT=8084 dune exec examples/twirp_tiny_httpd/calculator_server.exe
listen on http://localhost:8084/

$ curl -X POST http://localhost:8084/twirp/Calculator/add --json '{"a": 1, "b": 2}'
{"value":3}⏎
```

