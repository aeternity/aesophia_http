# aesophia_http

An HTTP interface to the Sophia compiler.It handles compiling
contracts and generating ACI data for contract.

HTTP interface uses port 3080, settable with application variable
aesophia_http.port.

Interface paths:

/contracts/code/compile - compile contract and return code in a JSON structure encoded to contract_bytearray.

/contracts/code/aci - generate ACI format for contract in both JSON encoded and textual decoded forms.

Code in body under tag code.

This is definitely WIP.
