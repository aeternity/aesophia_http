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

/version - return the version of the Sophia compiler

/api-version - return the version of the API

/api - return the API in a JSON-term (intended to be consumed by tools)

## Building

The most convenient way to start the an HTTP server serving the Sophia compiler API is using
docker. `make docker` will create a docker image `aeternity/aesophia_http:latest` and it is
started by `docker run -p <PORT>:3080 aeternity/aesophia_http:local` where `PORT` is the
local port where you'd like the API to be served.
