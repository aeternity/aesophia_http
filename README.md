# aesophia_http

An HTTP interface to the Sophia compiler. It handles compiling
contracts and generating ACI data for contract. The examples show

HTTP interface uses port 3080, settable with application variable
aesophia_http.port.

Interface paths (see the `config/swagger.yaml` for details):

/aci - generate ACI format for contract in both JSON encoded and textual decoded forms. Tags 'code' and 'options'.

/compile - compile contract and return code in a JSON structure encoded to contract_bytearray.

/decode-call-result - Tags 'source', 'function', 'call-result', 'call-value'.

/decode-call-result/bytecode - Tags 'bytecode', 'function', 'call-result', 'call-value'.

/encode-calldata - Tags 'source', 'function' and 'arguments'.

/decode-calldata/bytecode - Input calldata and contract bytecode, get function name and VM type arguments.

/decode-calldata/source - Input calldata and (possibly partial) conctract source code, get function name and Sophia type arguments.

/validate-byte-code - Check that some bytecode was produced from the given source code.

/fate-assembler - Get the FATE assembler (as a string) from FATE bytecode.

/compiler-version - Extract the compiler version from bytecode.

/version - return the version of the Sophia compiler

/api-version - return the version of the API

/api - return the API in a JSON-term (intended to be consumed by tools)


## Usage

We publish a docker image as `aeternity/aesophia_http` - so if docker is in place
all that is needed to have an HTTP server serving the Sophia compiler API is:
```
docker run -p <PORT>:3080 aeternity/aesophia_http
```

Where `PORT` is the local port where you'd like the API to be served.

## Building

The most convenient way to start the an HTTP server serving the Sophia compiler API is using
docker. `make docker` will create a docker image `aeternity/aesophia_http:local` and it is
started by `docker run -p <PORT>:3080 aeternity/aesophia_http:local` where `PORT` is the
local port where you'd like the API to be served.

## Examples

In all the following examples we use the contract `SimpleStorage`
defined as:

### Preparation
```
contract SimpleStorage =
  record state = { data : int }
  function init(value : int) : state = { data = value }
  function get() : int = state.data
  stateful function set(value : int) = put(state{data = value})
```

To make the example calls easier to read we have bound the shell
variable `$contract` to the contract definition:

```
contract="contract SimpleStorage =
  record state = { data : int }
  entrypoint init(value : int) : state = { data = value }
  entrypoint get() : int = state.data
  stateful entrypoint set(value : int) = put(state{data = value})"
```

### Generating ACI
To get the ACI of the contract we use the `/aci` interface:

```
curl -H "Content-Type: application/json" -d "{\"code\":\"$contract\",\"options\":{}}" -X POST http://localhost:3080/aci
```
Returns:
```
{"encoded_aci":{"contract":{"functions":[{"arguments":[{"name":"value","type":"int"}],"name":"init","returns":"SimpleStorage.state","stateful":false},{"arguments":[],"name":"get","returns":"int","stateful":false},{"arguments":[{"name":"value","type":"int"}],"name":"set","returns":{"tuple":[]},"stateful":true}],"name":"SimpleStorage","state":{"record":[{"name":"data","type":"int"}]},"type_defs":[]}},"interface":"contract SimpleStorage =\n  record state = {data : int}\n  function init : (int) => SimpleStorage.state\n  function get : () => int\n  function set : (int) => ()\n"}
```
Important: If your contract code contains quotes, you need to escape every quote with a backslash:
```
    switch(expression)
      true => \"true\"    // also \"in comments
      false => \"false\"
```


This returns a structure with two fields: `encoded_aci` is a
description of the contract containing the types and functions;
`interface` is a definition of the contract suitable to be included in
other contracts.

### Compiling contract to bytecode
We can now compile the contract and get the bytecode:

```
curl -H "Content-Type: application/json" -d "{\"code\":\"$contract\", \"options\":{}}" -X POST http://localhost:3080/compile
```
Returns:
```
{"bytecode":"cb_+IBGA6AJQqlX6tyRsl4BPLit17O1UczvqlaptU2Kp4bI3PatZcC4U7D+L4Zb2QA3AAcBAoL+RNZEHwA3AQc3ABoGggABAz/+6MRetgA3AQc3ABoGggABAz+eLwMRL4Zb2Q1nZXQRRNZEHxFpbml0EejEXrYNc2V0gi8AhTYuMS4wAEhXFsk="}
```

### Validating bytecode

You can check that some particular bytecode, for instance obtained from the
chain, was compiled from given source code, using the `validate-byte-code`
endpoint:
```
curl -H "Content-Type: application/json" -d '{"source":"contract Id = entrypoint id(x : int) = x","options":{},"bytecode":"cb_+GNGA6CBDP58NrY5L7PzZrlGZ0C8aqcXIYwqv2WMpyTg8IuBTsC3nv5E1kQfADcANwAaDoI/AQM//tjzDDgANwEHBwEBAJQvAhFE1kQfEWluaXQR2PMMOAlpZIIvAIU0LjAuMABqFanJ"}' -X POST http://localhost:3080/validate-byte-code
```
Returns:
```
{}
```
Validating against source code with a different implementation of `id` returns:
```
[{"message":"Byte code does not match source code.\n- The implementation of the function id is different.\n","pos":{"col":0,"line":0},"type":"data_error"}]
```

### Getting fate assembler

You can get the FATE assembler code for some particular bytecode:
```
curl -H "Content-Type: application/json" -d '{"bytecode":"cb_+GNGA6CBDP58NrY5L7PzZrlGZ0C8aqcXIYwqv2WMpyTg8IuBTsC3nv5E1kQfADcANwAaDoI/AQM//tjzDDgANwEHBwEBAJQvAhFE1kQfEWluaXQR2PMMOAlpZIIvAIU0LjAuMABqFanJ"}' -X POST http://localhost:3080/fate-assembler
```
Returns:
```
{"fate-assembler":"FUNCTION init( ) : {tuple,[]}\n  ;; BB : 0\n          STORE store1 ()\n          RETURNR ()\nFUNCTION id( integer) : integer\n  ;; BB : 0\n          RETURNR arg0\n\n"}
```

### Encoding calldata

To encode the call data necessary to call the function `set` with the
argument `42`:

```
curl -H "Content-Type: application/json" -d "{\"function\":\"set\",\"arguments\":[\"42\"],\"source\":\"$contract\", \"options\":{}}" -X POST http://localhost:3080/encode-calldata
```
Returns:
```
{"calldata":"cb_KxHoxF62G1Sy3bqn"}
```

### Decoding call data

Call data from transactions can be decoded in two ways, either using the contract bytecode (this is a long one) which returns the VM types and values, or the Sophia contract source.

#### With Contract Bytecode

Example:
```
curl -H "Content-Type: application/json" -d "{\"calldata\":\"cb_AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACA6hZQte0c6B/XQTuHZwWpc6rFreRzqkolhGkTD+eW6BwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAABgAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACoA4Uun\",\"bytecode\":\"cb_+QYYRgKgCTTJlUBVAUHWm6tXQKIwDZi3yvR+jeNv8JCPQzLT6xT5BKX5AUmgOoWULXtHOgf10E7h2cFqXOqxa3kc6pKJYRpEw/nlugeDc2V0uMAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAIAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAADAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAGAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAoP//////////////////////////////////////////AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAC4YAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAgAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAP///////////////////////////////////////////jJoEnsSQdsAgNxJqQzA+rc5DsuLDKUV7ETxQp+ItyJgJS3g2dldLhgAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA///////////////////////////////////////////uEAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAIAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA+QKLoOIjHWzfyTkW3kyzqYV79lz0D8JW9KFJiz9+fJgMGZNEhGluaXS4wAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAgAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAMAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAYAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACg//////////////////////////////////////////8AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAALkBoAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAgAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAMAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAYAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACgAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAMAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAABQAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAEA//////////////////////////////////////////8AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAFAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAYD//////////////////////////////////////////wAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAuQFEYgAAj2IAAMKRgICAUX9J7EkHbAIDcSakMwPq3OQ7LiwylFexE8UKfiLciYCUtxRiAAE5V1CAgFF/4iMdbN/JORbeTLOphXv2XPQPwlb0oUmLP358mAwZk0QUYgAA0VdQgFF/OoWULXtHOgf10E7h2cFqXOqxa3kc6pKJYRpEw/nlugcUYgABG1dQYAEZUQBbYAAZWWAgAZCBUmAgkANgAFmQgVKBUllgIAGQgVJgIJADYAOBUpBZYABRWVJgAFJgAPNbYACAUmAA81tgAFFRkFZbYCABUVGQUIOSUICRUFCAWZCBUllgIAGQgVJgIJADYAAZWWAgAZCBUmAgkANgAFmQgVKBUllgIAGQgVJgIJADYAOBUoFSkFCQVltgIAFRUVlQgJFQUGAAUYFZkIFSkFBgAFJZkFCQVltQUFlQUGIAAMpWhTIuMC4w4czHnw==\"}" -X POST http://localhost:3080/decode-calldata/bytecode
```

Returns:
`{"arguments":[{"type":"word","value":42}],"function":"set"}`

#### With Sophia Source

And secondly, using the Sophia contract source which returns the Sophia types and values:

```
curl -H "Content-Type: application/json" -d "{\"calldata\":\"cb_AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACA6hZQte0c6B/XQTuHZwWpc6rFreRzqkolhGkTD+eW6BwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAABgAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACoA4Uun\",\"function\":\"set\",\"source\":\"$contract\"}" -X POST http://localhost:3080/decode-calldata/source
```

Returns:
```
{"arguments":[{"type":"int","value":"42"}],"function":"set"}
```

### Decoding return value
You can also decode the return value of a contract call:

```
curl -H "Content-Type: application/json" -d "{\"source\":\"$contract\",\"function\":\"get\", \"call-result\":\"ok\", \"call-value\":\"cb_VNLOFXc=\"}" -X POST http://localhost:3080/decode-call-result
```

Returns:
```
42
```

The contract call result can also be decoded using the `bytecode` as a starting point:

```
curl -H "Content-Type: application/json" -d "{\"bytecode\":\"cb_+JFGA6DcSHcAbyhLqfbIDJRe1S7ZJLCZQJBUuvMmCLK5OirpHsC4YLg8/i+GW9kANwAHKCwAggD+RNZEHwA3AQc3AAwBACcMAhoCggEDP/7oxF62ADcBBzcADAEAJwwCGgKCAQM/ni8DES+GW9kNZ2V0EUTWRB8RaW5pdBHoxF62DXNldIIvAIk0LjAuMC1yYzQAPwYsew==\",\"function\":\"get\", \"call-result\":\"ok\", \"call-value\":\"cb_VNLOFXc=\"}" -X POST http://localhost:3080/decode-call-result/bytecode
```

Returns:
```
{"function":"get","result":{"type":"int","value":42}}
```

### Getting the compiler version used
You can extract the compiler version that was used to compile some particular bytecode:

```
curl -H "Content-Type: application/json" -d '{"bytecode":"cb_+GNGA6CBDP58NrY5L7PzZrlGZ0C8aqcXIYwqv2WMpyTg8IuBTsC3nv5E1kQfADcANwAaDoI/AQM//tjzDDgANwEHBwEBAJQvAhFE1kQfEWluaXQR2PMMOAlpZIIvAIU0LjAuMABqFanJ"}' -X POST http://localhost:3080/compiler-version
```

Returns:
```
{"version":"4.0.0"}
```

### Complete API overview
To get information about the current version of the Sophia compiler, API
version and the whole actual API (paste into any [swagger file generator](https://editor.swagger.io/)):

```
curl http://localhost:3080/version

{"version":"8.0.0"}
```

```
curl http://localhost:3080/api-version

{"api-version":"8.0.0"}
```

```
curl http://localhost:3080/api

{"basePath":"/","definitions":{  ... },"info":{ ... },"paths":{ ... },"schemes":["http"],"swagger":"2.0"}
```

Note that in a future release the swagger specification will be replaced by an OpenAPI 3.0 specification.
