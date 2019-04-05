# aesophia_http

An HTTP interface to the Sophia compiler.It handles compiling
contracts and generating ACI data for contract.

HTTP interface uses port 3080, settable with application variable
aesophia_http.port.

Interface paths (see the `config/swagger.yaml` for details):

/aci - generate ACI format for contract in both JSON encoded and textual decoded forms. Tags 'code' and 'options'.

/compile - compile contract and return code in a JSON structure encoded to contract_bytearray.

/decode-data - Tags 'sophia-type' and 'data'.

/encode-calldata - Tags 'source', 'function' and 'arguments'.

/decode-calldata/bytecode - Input calldata and contract bytecode, get function name and VM type arguments.

/decode-calldata/source - Input calldata and (possibly partial) conctract source code, get function name and Sophia type arguments.

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

```
contract SimpleStorage =
  record state = { data : int }
  function init(value : int) : state = { data = value }
  function get() : int = state.data
  function set(value : int) = put(state{data = value})
```

To make the example calls easier to read we have bound the shell
variable `$contract` to the contract definition:

```
contract="contract SimpleStorage =
  record state = { data : int }
  function init(value : int) : state = { data = value }
  function get() : int = state.data
  function set(value : int) = put(state{data = value})"
```

To get the ACI of the contract we use the `/aci` interface:

```
curl -H "Content-Type: application/json" -d "{\"code\":\"$contract\",\"options\":{}}" -X POST http://localhost:3080/aci

{"encoded_aci":{"contract":{"name":"SimpleStorage","type_defs":[{"name":"state","vars":[],"typedef":{"record":[{"name":"data","type":["int"]}]}}],"functions":[{"name":"init","arguments":[{"name":"value","type":["int"]}],"returns":{"record":[{"name":"data","type":["int"]}]},"stateful":false},{"name":"get","arguments":[],"returns":"int","stateful":false},{"name":"set","arguments":[{"name":"value","type":["int"]}],"returns":{"tuple":[]},"stateful":false}]}},"interface":"contract SimpleStorage =\n  function get : () => int\n  function set : (int) => ()\n"}

```

This returns a structure with two fields: `encoded_aci` is a
description of the contract containing the types and functions;
`interface` is a definition of the contract suitable to be included in
other contracts.

We can now compile the contract and get the bytecode:

```
curl -H "Content-Type: application/json" -d "{\"code\":\"$contract\",\"options\":{}}" -X POST http://localhost:3080/compile

{"bytecode":"cb_+QYYRgKgf6Gy7VnRXycsYSiFGAUHhMs+Oeg+RJvmPzCSAnxk8LT5BKX5AUmgOoWULXtHOgf10E7h2cFqXOqxa3kc6pKJYRpEw/nlugeDc2V0uMAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAIAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAADAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAGAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAoP//////////////////////////////////////////AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAC4YAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAgAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAP///////////////////////////////////////////jJoEnsSQdsAgNxJqQzA+rc5DsuLDKUV7ETxQp+ItyJgJS3g2dldLhgAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA///////////////////////////////////////////uEAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAIAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA+QKLoOIjHWzfyTkW3kyzqYV79lz0D8JW9KFJiz9+fJgMGZNEhGluaXS4wAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAgAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAMAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAYAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACg//////////////////////////////////////////8AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAALkBoAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAgAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAMAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAYAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACgAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAMAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAABQAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAEA//////////////////////////////////////////8AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAFAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAYD//////////////////////////////////////////wAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAuQFEYgAAj2IAAMKRgICAUX9J7EkHbAIDcSakMwPq3OQ7LiwylFexE8UKfiLciYCUtxRiAAE5V1CAgFF/4iMdbN/JORbeTLOphXv2XPQPwlb0oUmLP358mAwZk0QUYgAA0VdQgFF/OoWULXtHOgf10E7h2cFqXOqxa3kc6pKJYRpEw/nlugcUYgABG1dQYAEZUQBbYAAZWWAgAZCBUmAgkANgAFmQgVKBUllgIAGQgVJgIJADYAOBUpBZYABRWVJgAFJgAPNbYACAUmAA81tgAFFRkFZbYCABUVGQUIOSUICRUFCAWZCBUllgIAGQgVJgIJADYAAZWWAgAZCBUmAgkANgAFmQgVKBUllgIAGQgVJgIJADYAOBUoFSkFCQVltgIAFRUVlQgJFQUGAAUYFZkIFSkFBgAFJZkFCQVltQUFlQUGIAAMpWhTIuMC4wymxiIQ=="}
```

To encode the call data necessary to call the function `set` with the
argument `42`:

```
curl -H "Content-Type: application/json" -d "{\"function\":\"set\",\"arguments\":[\"42\"],\"source\":\"$contract\"}" -X POST http://localhost:3080/encode-calldata

{"calldata":"cb_AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACA6hZQte0c6B/XQTuHZwWpc6rFreRzqkolhGkTD+eW6BwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAABgAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACoA4Uun"}
```

We can now decode a return value:

```
curl -H "Content-Type: application/json" -d "{\"data\":\"cb_AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACr8s/aY\",\"sophia-type\":\"int\"}" -X POST http://localhost:3080/decode-data

{"data":{"type":"word","value":42}}
```

Finally we can decode calldata. First using the contract bytecode (this is a long one) which returns the VM types and values:

```
curl -H "Content-Type: application/json" -d "{\"calldata\":\"cb_AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACA6hZQte0c6B/XQTuHZwWpc6rFreRzqkolhGkTD+eW6BwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAABgAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACoA4Uun\",\"bytecode\":\"cb_+QYYRgKgCTTJlUBVAUHWm6tXQKIwDZi3yvR+jeNv8JCPQzLT6xT5BKX5AUmgOoWULXtHOgf10E7h2cFqXOqxa3kc6pKJYRpEw/nlugeDc2V0uMAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAIAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAADAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAGAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAoP//////////////////////////////////////////AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAC4YAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAgAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAP///////////////////////////////////////////jJoEnsSQdsAgNxJqQzA+rc5DsuLDKUV7ETxQp+ItyJgJS3g2dldLhgAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA///////////////////////////////////////////uEAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAIAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA+QKLoOIjHWzfyTkW3kyzqYV79lz0D8JW9KFJiz9+fJgMGZNEhGluaXS4wAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAgAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAMAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAYAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACg//////////////////////////////////////////8AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAALkBoAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAgAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAMAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAYAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACgAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAMAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAABQAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAEA//////////////////////////////////////////8AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAFAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAYD//////////////////////////////////////////wAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAuQFEYgAAj2IAAMKRgICAUX9J7EkHbAIDcSakMwPq3OQ7LiwylFexE8UKfiLciYCUtxRiAAE5V1CAgFF/4iMdbN/JORbeTLOphXv2XPQPwlb0oUmLP358mAwZk0QUYgAA0VdQgFF/OoWULXtHOgf10E7h2cFqXOqxa3kc6pKJYRpEw/nlugcUYgABG1dQYAEZUQBbYAAZWWAgAZCBUmAgkANgAFmQgVKBUllgIAGQgVJgIJADYAOBUpBZYABRWVJgAFJgAPNbYACAUmAA81tgAFFRkFZbYCABUVGQUIOSUICRUFCAWZCBUllgIAGQgVJgIJADYAAZWWAgAZCBUmAgkANgAFmQgVKBUllgIAGQgVJgIJADYAOBUoFSkFCQVltgIAFRUVlQgJFQUGAAUYFZkIFSkFBgAFJZkFCQVltQUFlQUGIAAMpWhTIuMC4w4czHnw==\"}" -X POST http://localhost:3080/decode-calldata/bytecode

{"arguments":[{"type":"word","value":42}],"function":"set"}
```

And second using the Sophia contract source which returns the Sophia types and values:

```
curl -H "Content-Type: application/json" -d "{\"calldata\":\"cb_AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACA6hZQte0c6B/XQTuHZwWpc6rFreRzqkolhGkTD+eW6BwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAABgAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACoA4Uun\",\"function\":\"set\",\"source\":\"$contract\"}" -X POST http://localhost:3080/decode-calldata/source

{"arguments":[{"type":[105,110,116],"value":[52,50]}],"function":"set"}
```

To get information about the current version of the Sophia compiler, API
version and the actual API (paste into any [swagger file generator](https://editor.swagger.io/)):

```
curl http://localhost:3080/version

{"version":"2.0.0"}
```

```
curl http://localhost:3080/api-version

{"api-version":"2.0.0"}
```

```
curl http://localhost:3080/api

{"api":{"swagger":"2.0","info":{"description":"This is the [Aeternity](https://www.aeternity.com/) compiler API.","version":"2.0.0","title":"Aeternity node","termsOfService":"https://www.aeternity.com/terms/","contact":{"email":"apiteam@aeternity.com"}},"basePath":"/","schemes":["http"],"paths":{"/aci":{"post":{"description":"Generate an Aeternity Contract Interface (ACI) for contract","operationId":"GenerateACI","consumes":["application/json"],"produces":["application/json"],"parameters":[{"in":"body","name":"body","description":"contract code","required":true,"schema":{"$ref":"#/definitions/Contract"}}],"responses":{"200":{"description":"ACI for contract","schema":{"$ref":"#/definitions/ACI"}},"403":{"description":"Invalid input","schema":{"$ref":"#/definitions/Error"}}}}},"/compile":{"post":{"description":"Compile a sophia contract from source and return byte code","operationId":"CompileContract","consumes":["application/json"],"produces":["application/json"],"parameters":[{"in":"body","name":"body","description":"contract code","required":true,"schema":{"$ref":"#/definitions/Contract"}}],"responses":{"200":{"description":"Byte code response","schema":{"$ref":"#/definitions/ByteCode"}},"403":{"description":"Invalid contract","schema":{"$ref":"#/definitions/Error"}}}}},"/decode-data":{"post":{"description":"Decode data as retuned by a contract call. - Legacy decoding","operationId":"DecodeData","consumes":["application/json"],"produces":["application/json"],"parameters":[{"in":"body","name":"body","description":"Binary data in Sophia ABI format","required":true,"schema":{"$ref":"#/definitions/SophiaBinaryData"}}],"responses":{"200":{"description":"Json encoded data","schema":{"$ref":"#/definitions/SophiaJsonData"}},"400":{"description":"Invalid data","schema":{"$ref":"#/definitions/Error"}}}}},"/encode-calldata":{"post":{"description":"Encode Sophia function call according to sophia ABI.","operationId":"EncodeCalldata","consumes":["application/json"],"produces":["application/json"],"parameters":[{"in":"body","name":"body","description":"Sophia function call - contract code + function name + arguments","required":true,"schema":{"$ref":"#/definitions/FunctionCallInput"}}],"responses":{"200":{"description":"Binary encoded calldata","schema":{"$ref":"#/definitions/Calldata"}},"403":{"description":"Invalid contract","schema":{"$ref":"#/definitions/Error"}}}}},"/version":{"get":{"description":"Get the version of the underlying Sophia compiler version","operationId":"Version","produces":["application/json"],"parameters":[],"responses":{"200":{"description":"Sophia compiler version","schema":{"$ref":"#/definitions/CompilerVersion"}},"500":{"description":"Error","schema":{"$ref":"#/definitions/Error"}}}}},"/api-version":{"get":{"description":"Get the version of the API","operationId":"APIVersion","produces":["application/json"],"parameters":[],"responses":{"200":{"description":"Sophia compiler version","schema":{"$ref":"#/definitions/APIVersion"}},"500":{"description":"Error","schema":{"$ref":"#/definitions/Error"}}}}},"/api":{"get":{"description":"Get the Api description","operationId":"Api","produces":["application/json"],"parameters":[],"responses":{"200":{"description":"API description","schema":{"$ref":"#/definitions/API"}},"403":{"description":"Error","schema":{"$ref":"#/definitions/Error"}}}}}},"definitions":{"Contract":{"type":"object","required":["code","options"],"properties":{"code":{"type":"string"},"options":{"$ref":"#/definitions/CompileOpts"}},"example":{"code":"code","options":{"src_file":"src_file","file_system":"{}"}}},"CompileOpts":{"type":"object","properties":{"file_system":{"type":"object","description":"An explicit file system, mapping file names to file content","properties":{}},"src_file":{"type":"string","description":"Name of contract source file - only used in error messages"}},"example":{"src_file":"src_file","file_system":"{}"}},"APIVersion":{"type":"object","required":["api-version"],"properties":{"api-version":{"type":"string","description":"API compiler version"}},"example":{"api-version":"api-version"}},"CompilerVersion":{"type":"object","required":["version"],"properties":{"version":{"type":"string","description":"Sophia compiler version"}},"example":{"version":"version"}},"Error":{"type":"object","required":["reason"],"properties":{"reason":{"type":"string"}}},"ACI":{"type":"object","required":["encoded_aci","interface"],"properties":{"encoded_aci":{"type":"object","properties":{}},"interface":{"type":"string"}},"example":{"interface":"interface","encoded_aci":"{}"}},"API":{"type":"object","properties":{"api":{"type":"object","description":"Swagger API description","properties":{}}},"example":{"api":"{}"}},"ByteCode":{"type":"object","required":["bytecode"],"properties":{"bytecode":{"$ref":"#/definitions/EncodedByteArray"}},"example":{"bytecode":{}}},"SophiaBinaryData":{"type":"object","required":["data","sophia-type"],"properties":{"sophia-type":{"type":"string"},"data":{"type":"string"}},"example":{"data":"data","sophia-type":"sophia-type"}},"SophiaJsonData":{"type":"object","required":["data"],"properties":{"data":{"type":"object","properties":{}}},"example":{"data":"{}"}},"FunctionCallInput":{"type":"object","required":["arguments","function","source"],"properties":{"source":{"type":"string","description":"(Possibly partial) Sophia contract code"},"function":{"type":"string","description":"Name of function to call"},"arguments":{"type":"array","description":"Array of function call arguments","items":{"type":"string"}}},"example":{"function":"function","arguments":["arguments","arguments"],"source":"source"}},"Calldata":{"type":"object","required":["calldata"],"properties":{"calldata":{"$ref":"#/definitions/EncodedByteArray"}},"example":{"calldata":{}}},"EncodedByteArray":{"type":"string","description":"Prefixed (cb_) Base64Check encoded byte array"}}}}
```
