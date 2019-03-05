# aesophia_http

An HTTP interface to the Sophia compiler.It handles compiling
contracts and generating ACI data for contract.

HTTP interface uses port 8080, settable with application variable
aesophia_http.port.

Interface paths:

/aci - generate ACI format for contract in both JSON encoded and textual decoded forms. Tags 'code' and 'options'.

/compile - compile contract and return code in a JSON structure encoded to contract_bytearray.

/decode-data - Tags 'sophia-type' and 'data'.

/encode-calldata - Tags 'source', 'function' and 'arguments'.
