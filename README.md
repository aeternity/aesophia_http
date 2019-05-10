# aesophia_http

An HTTP interface to the Sophia compiler. It handles compiling
contracts and generating ACI data for contract. The examples show 

HTTP interface uses port 3080, settable with application variable
aesophia_http.port.

Interface paths (see the `config/swagger.yaml` for details):

/aci - generate ACI format for contract in both JSON encoded and textual decoded forms. Tags 'code' and 'options'.

/compile - compile contract and return code in a JSON structure encoded to contract_bytearray.

/decode-data - Tags 'sophia-type' and 'data'.

/decode-returndata/bytecode - Input bytecode, function name and encoded return data, get decoded return data.

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

### Preparation
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

### Generating ACI
To get the ACI of the contract we use the `/aci` interface:

```
curl -H "Content-Type: application/json" -d "{\"code\":\"$contract\",\"options\":{}}" -X POST http://localhost:3080/aci
```
Returns:
```
{"encoded_aci":{"contract":{"name":"SimpleStorage","state":{"record":[{"name":"data","type":"int"}]},"type_defs":[],"functions":[{"name":"init","arguments":[{"name":"value","type":"int"}],"returns":{"record":[{"name":"data","type":"int"}]},"stateful":false},{"name":"get","arguments":[],"returns":"int","stateful":false},{"name":"set","arguments":[{"name":"value","type":"int"}],"returns":{"tuple":[]},"stateful":false}]}},"interface":"contract SimpleStorage =\n  function get : () => int\n  function set : (int) => ()\n"}
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
curl -H "Content-Type: application/json" -d "{\"code\":\"$contract\",\"options\":{}}" -X POST http://localhost:3080/compile
```

Returns:
```
{"bytecode":"cb_+QYYRgKgf6Gy7VnRXycsYSiFGAUHhMs+Oeg+RJvmPzCSAnxk8LT5BKX5AUmgOoWULXtHOgf10E7h2cFqXOqxa3kc6pKJYRpEw/nlugeDc2V0uMAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAIAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAADAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAGAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAoP//////////////////////////////////////////AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAC4YAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAgAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAP///////////////////////////////////////////jJoEnsSQdsAgNxJqQzA+rc5DsuLDKUV7ETxQp+ItyJgJS3g2dldLhgAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA///////////////////////////////////////////uEAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAIAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA+QKLoOIjHWzfyTkW3kyzqYV79lz0D8JW9KFJiz9+fJgMGZNEhGluaXS4wAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAgAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAMAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAYAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACg//////////////////////////////////////////8AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAALkBoAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAgAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAMAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAYAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACgAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAMAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAABQAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAEA//////////////////////////////////////////8AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAFAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAYD//////////////////////////////////////////wAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAuQFEYgAAj2IAAMKRgICAUX9J7EkHbAIDcSakMwPq3OQ7LiwylFexE8UKfiLciYCUtxRiAAE5V1CAgFF/4iMdbN/JORbeTLOphXv2XPQPwlb0oUmLP358mAwZk0QUYgAA0VdQgFF/OoWULXtHOgf10E7h2cFqXOqxa3kc6pKJYRpEw/nlugcUYgABG1dQYAEZUQBbYAAZWWAgAZCBUmAgkANgAFmQgVKBUllgIAGQgVJgIJADYAOBUpBZYABRWVJgAFJgAPNbYACAUmAA81tgAFFRkFZbYCABUVGQUIOSUICRUFCAWZCBUllgIAGQgVJgIJADYAAZWWAgAZCBUmAgkANgAFmQgVKBUllgIAGQgVJgIJADYAOBUoFSkFCQVltgIAFRUVlQgJFQUGAAUYFZkIFSkFBgAFJZkFCQVltQUFlQUGIAAMpWhTIuMC4wymxiIQ=="}
```

### Encoding calldata

To encode the call data necessary to call the function `set` with the
argument `42`:

```
curl -H "Content-Type: application/json" -d "{\"function\":\"set\",\"arguments\":[\"42\"],\"source\":\"$contract\"}" -X POST http://localhost:3080/encode-calldata
```
Returns:
```
{"calldata":"cb_AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACA6hZQte0c6B/XQTuHZwWpc6rFreRzqkolhGkTD+eW6BwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAABgAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACoA4Uun"}
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
curl -H "Content-Type: application/json" -d "{\"data\":\"cb_AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACr8s/aY\",\"sophia-type\":\"int\"}" -X POST http://localhost:3080/decode-data
```

Returns:
```
{"data":{"type":"word","value":42}}
```

### Decoding return value with the bytecode

It is also possible to decode the a return value of a function using the bytecode, the function name and the encoded return value:

```
curl -v -H "Content-Type: application/json" -d "{\"bytecode\":\"cb_+QYYRgKgCTTJlUBVAUHWm6tXQKIwDZi3yvR+jeNv8JCPQzLT6xT5BKX5AUmgOoWULXtHOgf10E7h2cFqXOqxa3kc6pKJYRpEw/nlugeDc2V0uMAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAIAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAADAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAGAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAoP//////////////////////////////////////////AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAC4YAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAgAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAP///////////////////////////////////////////jJoEnsSQdsAgNxJqQzA+rc5DsuLDKUV7ETxQp+ItyJgJS3g2dldLhgAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA///////////////////////////////////////////uEAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAIAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA+QKLoOIjHWzfyTkW3kyzqYV79lz0D8JW9KFJiz9+fJgMGZNEhGluaXS4wAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAgAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAMAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAYAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACg//////////////////////////////////////////8AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAALkBoAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAgAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAMAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAYAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACgAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAMAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAABQAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAEA//////////////////////////////////////////8AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAFAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAYD//////////////////////////////////////////wAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAuQFEYgAAj2IAAMKRgICAUX9J7EkHbAIDcSakMwPq3OQ7LiwylFexE8UKfiLciYCUtxRiAAE5V1CAgFF/4iMdbN/JORbeTLOphXv2XPQPwlb0oUmLP358mAwZk0QUYgAA0VdQgFF/OoWULXtHOgf10E7h2cFqXOqxa3kc6pKJYRpEw/nlugcUYgABG1dQYAEZUQBbYAAZWWAgAZCBUmAgkANgAFmQgVKBUllgIAGQgVJgIJADYAOBUpBZYABRWVJgAFJgAPNbYACAUmAA81tgAFFRkFZbYCABUVGQUIOSUICRUFCAWZCBUllgIAGQgVJgIJADYAAZWWAgAZCBUmAgkANgAFmQgVKBUllgIAGQgVJgIJADYAOBUoFSkFCQVltgIAFRUVlQgJFQUGAAUYFZkIFSkFBgAFJZkFCQVltQUFlQUGIAAMpWhTIuMC4w4czHnw==\",\"function\":\"get\",\"return\":\"cb_AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACr8s/aY\"}"  -X POST http://localhost:3080/decode-returndata/bytecode
```

Returns:
```
{"data":{"type":"word","value":42}}`
```

### Complete API overview

To get information about the current version of the Sophia compiler, API
version and the whole actual API (paste into any [swagger file generator](https://editor.swagger.io/)):

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

{"basePath":"/","definitions":{"ACI":{"example":{"encoded_aci":"{}","interface":"interface"},"properties":{"encoded_aci":{"properties":{},"type":"object"},"interface":{"type":"string"}},"required":["encoded_aci","interface"],"type":"object"},"API":{"description":"Swagger API description","type":"object"},"APIVersion":{"example":{"api-version":"api-version"},"properties":{"api-version":{"description":"API compiler version","type":"string"}},"required":["api-version"],"type":"object"},"ByteCode":{"example":{"bytecode":{}},"properties":{"bytecode":{"$ref":"#/definitions/EncodedByteArray"}},"required":["bytecode"],"type":"object"},"Calldata":{"example":{"calldata":{}},"properties":{"calldata":{"$ref":"#/definitions/EncodedByteArray"}},"required":["calldata"],"type":"object"},"CompileOpts":{"example":{"file_system":"{}","src_file":"src_file"},"properties":{"file_system":{"description":"An explicit file system, mapping file names to file content","properties":{},"type":"object"},"src_file":{"description":"Name of contract source file - only used in error messages","type":"string"}},"type":"object"},"CompilerVersion":{"example":{"version":"version"},"properties":{"version":{"description":"Sophia compiler version","type":"string"}},"required":["version"],"type":"object"},"Contract":{"example":{"code":"code","options":{"file_system":"{}","src_file":"src_file"}},"properties":{"code":{"type":"string"},"options":{"$ref":"#/definitions/CompileOpts"}},"required":["code","options"],"type":"object"},"DecodeCalldataBytecode":{"example":{"bytecode":null,"calldata":{}},"properties":{"bytecode":{"$ref":"#/definitions/EncodedByteArray","description":"Compiled contract"},"calldata":{"$ref":"#/definitions/EncodedByteArray","description":"Calldata to dissect"}},"required":["bytecode","calldata"],"type":"object"},"DecodeCalldataSource":{"example":{"calldata":{},"source":"source"},"properties":{"calldata":{"$ref":"#/definitions/EncodedByteArray","description":"Calldata to dissect"},"source":{"description":"(Possibly partial) Sophia contract code","type":"string"}},"type":"object"},"DecodeReturnDataBytecode":{"example":{"bytecode":{},"function":"function","return":"return"},"properties":{"bytecode":{"$ref":"#/definitions/EncodedByteArray","description":"Compiled contract"},"function":{"type":"string"},"return":{"type":"string"}},"required":["bytecode","function","return"],"type":"object"},"DecodedCalldata":{"example":{"arguments":["{}","{}"],"function":"function"},"properties":{"arguments":{"items":{"properties":{},"type":"object"},"type":"array"},"function":{"type":"string"}},"required":["arguments","function"],"type":"object"},"EncodedByteArray":{"description":"Prefixed (cb_) Base64Check encoded byte array","type":"string"},"Error":{"properties":{"reason":{"type":"string"}},"required":["reason"],"type":"object"},"FunctionCallInput":{"example":{"arguments":["arguments","arguments"],"function":"function","source":"source"},"properties":{"arguments":{"description":"Array of function call arguments","items":{"type":"string"},"type":"array"},"function":{"description":"Name of function to call","type":"string"},"source":{"description":"(Possibly partial) Sophia contract code","type":"string"}},"required":["arguments","function","source"],"type":"object"},"SophiaBinaryData":{"example":{"data":"data","sophia-type":"sophia-type"},"properties":{"data":{"type":"string"},"sophia-type":{"type":"string"}},"required":["data","sophia-type"],"type":"object"},"SophiaJsonData":{"example":{"data":"{}"},"properties":{"data":{"properties":{},"type":"object"}},"required":["data"],"type":"object"}},"info":{"contact":{"email":"apiteam@aeternity.com"},"description":"This is the [Aeternity](https://www.aeternity.com/) compiler API.","termsOfService":"https://www.aeternity.com/terms/","title":"Aeternity node","version":"2.1.0"},"paths":{"/aci":{"post":{"consumes":["application/json"],"description":"Generate an Aeternity Contract Interface (ACI) for contract","operationId":"GenerateACI","parameters":[{"description":"contract code","in":"body","name":"body","required":true,"schema":{"$ref":"#/definitions/Contract"}}],"produces":["application/json"],"responses":{"200":{"description":"ACI for contract","schema":{"$ref":"#/definitions/ACI"}},"403":{"description":"Invalid input","schema":{"$ref":"#/definitions/Error"}}}}},"/api":{"get":{"description":"Get the Api description","operationId":"Api","parameters":[],"produces":["application/json"],"responses":{"200":{"description":"API description","schema":{"$ref":"#/definitions/API"}},"403":{"description":"Error","schema":{"$ref":"#/definitions/Error"}}}}},"/api-version":{"get":{"description":"Get the version of the API","operationId":"APIVersion","parameters":[],"produces":["application/json"],"responses":{"200":{"description":"Sophia compiler version","schema":{"$ref":"#/definitions/APIVersion"}},"500":{"description":"Error","schema":{"$ref":"#/definitions/Error"}}}}},"/compile":{"post":{"consumes":["application/json"],"description":"Compile a sophia contract from source and return byte code","operationId":"CompileContract","parameters":[{"description":"contract code","in":"body","name":"body","required":true,"schema":{"$ref":"#/definitions/Contract"}}],"produces":["application/json"],"responses":{"200":{"description":"Byte code response","schema":{"$ref":"#/definitions/ByteCode"}},"403":{"description":"Invalid contract","schema":{"$ref":"#/definitions/Error"}}}}},"/decode-calldata/bytecode":{"post":{"consumes":["application/json"],"description":"Identify function name and arguments in Calldata for a compiled contract","operationId":"DecodeCalldataBytecode","parameters":[{"description":"Calldata + compiled contract","in":"body","name":"body","required":true,"schema":{"$ref":"#/definitions/DecodeCalldataBytecode"}}],"produces":["application/json"],"responses":{"200":{"description":"Binary encoded calldata","schema":{"$ref":"#/definitions/DecodedCalldata"}},"403":{"description":"Invalid contract","schema":{"$ref":"#/definitions/Error"}}}}},"/decode-calldata/source":{"post":{"consumes":["application/json"],"description":"Identify function name and arguments in Calldata for a (partial) contract","operationId":"DecodeCalldataSource","parameters":[{"description":"Calldata + contract (stub) code","in":"body","name":"body","required":true,"schema":{"$ref":"#/definitions/DecodeCalldataSource"}}],"produces":["application/json"],"responses":{"200":{"description":"Binary encoded calldata","schema":{"$ref":"#/definitions/DecodedCalldata"}},"403":{"description":"Invalid contract","schema":{"$ref":"#/definitions/Error"}}}}},"/decode-data":{"post":{"consumes":["application/json"],"description":"Decode data as retuned by a contract call. - Legacy decoding","operationId":"DecodeData","parameters":[{"description":"Binary data in Sophia ABI format","in":"body","name":"body","required":true,"schema":{"$ref":"#/definitions/SophiaBinaryData"}}],"produces":["application/json"],"responses":{"200":{"description":"Json encoded data","schema":{"$ref":"#/definitions/SophiaJsonData"}},"403":{"description":"Invalid data","schema":{"$ref":"#/definitions/Error"}}}}},"/decode-returndata/bytecode":{"post":{"consumes":["application/json"],"description":"Decode return value from function all.","operationId":"DecodeReturnDataBytecode","parameters":[{"description":"Binary data in Sophia ABI format","in":"body","name":"body","required":true,"schema":{"$ref":"#/definitions/DecodeReturnDataBytecode"}}],"produces":["application/json"],"responses":{"200":{"description":"Json encoded data","schema":{"$ref":"#/definitions/SophiaJsonData"}},"403":{"description":"Invalid data","schema":{"$ref":"#/definitions/Error"}}}}},"/encode-calldata":{"post":{"consumes":["application/json"],"description":"Encode Sophia function call according to sophia ABI.","operationId":"EncodeCalldata","parameters":[{"description":"Sophia function call - contract code + function name + arguments","in":"body","name":"body","required":true,"schema":{"$ref":"#/definitions/FunctionCallInput"}}],"produces":["application/json"],"responses":{"200":{"description":"Binary encoded calldata","schema":{"$ref":"#/definitions/Calldata"}},"403":{"description":"Invalid contract","schema":{"$ref":"#/definitions/Error"}}}}},"/version":{"get":{"description":"Get the version of the underlying Sophia compiler version","operationId":"Version","parameters":[],"produces":["application/json"],"responses":{"200":{"description":"Sophia compiler version","schema":{"$ref":"#/definitions/CompilerVersion"}},"500":{"description":"Error","schema":{"$ref":"#/definitions/Error"}}}}}},"schemes":["http"],"swagger":"2.0"}
```
