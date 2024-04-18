# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]
### Added
- Expose compiler warnings
### Changed
### Removed

## [8.0.0]
### Added
- Expose compiler warnings
### Changed
- Updated to [Sophia 8.0.0](https://github.com/aeternity/aesophia/blob/master/CHANGELOG.md#800)

## [7.6.1]
### Changed
- Updated to [Sophia 7.4.1](https://github.com/aeternity/aesophia/blob/master/CHANGELOG.md#741)
- Updated `aeserialization` to [v1.1.0](https://github.com/aeternity/aeserialization/releases/tag/v1.1.0)
  and `aebytecode` to [v3.4.0](https://github.com/aeternity/aebytecode/releases/tag/v3.4.0)

## [7.6.0]
### Changed
- Updated to [Sophia 7.4.0](https://github.com/aeternity/aesophia/blob/master/CHANGELOG.md#740)

## [7.5.0]
### Changed
- Updated to [Sophia 7.3.0](https://github.com/aeternity/aesophia/blob/master/CHANGELOG.md#730)
- Aligned HTTP API version with HTTP Compiler version (7.5.0) - Sophia compiler version is 7.3.0

## [7.4.0]
### Added
- GenerateACI accepts contract interface again
### Changed
- Updated to [Sophia 7.2.1](https://github.com/aeternity/aesophia/blob/master/CHANGELOG.md#721)
- GenerateACI returns ACI formatted the same way as in aesophia\_cli
- Increased `idle_timeout` to 10 minutes to allow slow compilations to succeed

## [7.3.0]
### Changed
- Switched to building multiplatform docker images

## [7.2.0]
### Changed
- Updated to [Sophia 7.1.0](https://github.com/aeternity/aesophia/blob/master/CHANGELOG.md#710)

## [7.1.1]
### Changed
- Fix a dependecy issue stopping `compile` endpoint from working properly

## [7.1.0]
### Added
- `aci` property to the result of CompileContract with ACI formatted the same way as in aesophia\_cli

## [7.0.1]
### Changed
- Updated to [Sophia 7.0.1](https://github.com/aeternity/aesophia/blob/master/CHANGELOG.md#701)
- Rename APIVersion to ApiVersion
- Don't require options object

## [7.0.0]
### Changed
- Updated to [Sophia 7.0.0](https://github.com/aeternity/aesophia/blob/master/CHANGELOG.md#700)
### Removed
- Support for AEVM
- Backend parameters
- `decode-data` endpoint (it was used by AEVM exclusively)

## [6.1.0]
### Changed
- Updated to [Sophia 6.1.0](https://github.com/aeternity/aesophia/blob/master/CHANGELOG.md#610---2021-10-20)

## [6.0.2]
### Added
### Changed
- Update to [Sophia 6.0.2](https://github.com/aeternity/aesophia/blob/master/CHANGELOG.md#602)
### Removed

## [6.0.1]
### Added
### Changed
- Update to [Sophia 6.0.1](https://github.com/aeternity/aesophia/blob/master/CHANGELOG.md#601)
### Removed

## [6.0.0]
### Added
### Changed
- Update to [Sophia 6.0.0](https://github.com/aeternity/aesophia/blob/master/CHANGELOG.md#600)
### Removed

## [5.0.0]
### Added
### Changed
- Update to [Sophia 5.0.0](https://github.com/aeternity/aesophia/blob/master/CHANGELOG.md#500)
### Removed

## [4.3.2] - 2020-06-11
### Note
- Skipped 4.3.1 release as this was used as fix only in `aesophia_cli`
### Added
- Add `external_encoded_aci` parameter to ACI endpoint returning ACI of any contract but the main
### Changed
### Removed

## [4.3.0] - 2020-04-02
### Added
### Changed
- Update to [Sophia 4.3.0](https://github.com/aeternity/aesophia/blob/master/CHANGELOG.md#430)
### Removed

## [4.2.1] - 2020-03-12
### Added
### Changed
- Always return `Vary` HTTP header together with CORS headers to prevent caching issues
### Removed

## [4.2.0] - 2020-01-15
### Added
- Added the endpoint `fate-assembler`
- Added the endpoint `compiler-version`
### Changed
- Update to [Sophia 4.2.0](https://github.com/aeternity/aesophia/blob/master/CHANGELOG.md#420---2020-01-15)
### Removed

## [4.1.0] - 2019-11-26
### Added
- Added the endpoint `validate-byte-code`
### Changed
- Update to [Sophia 4.1.0](https://github.com/aeternity/aesophia/blob/master/CHANGELOG.md#410---2019-11-26)
### Removed

## [4.0.0] - 2019-10-11
### Added
- Added the endpoint `decode-call-result/bytecode`
- The compiler now supports both FATE and AEVM - therefore many of the APIs now have an
  additional configuration field `backend` that can be either `fate` or `aevm` (`fate` is
  the default). See the Swagger file for details.
- Some of the APIs were missing the option to have a `filesystem` configuration option,
  this has been corrected. Again, see the Swagger file for details.
### Changed
- Update to [Sophia 4.0.0](https://github.com/aeternity/aesophia/blob/master/CHANGELOG.md#400---2019-10-11)
- The error reporting has been updated - see [Swagger file](https://github.com/aeternity/aesophia_http/blob/master/config/swagger.yaml)
  for details.
### Removed

## [3.2.0] - 2019-06-28
### Added
### Changed
- Update to [Sophia 3.2.0](https://github.com/aeternity/aesophia/blob/master/CHANGELOG.md#320---2019-06-28)
### Removed

## [3.1.0] - 2019-06-03
### Added
- `/decode-call-result` - new endpoint, returns ACI style JSON values for a contract
  call result.
### Changed
- Update to latest Sophia - this includes an updated and improved ACI module +
  a bug fix for create_calldata (negative literals).
### Removed

## [3.0.0] - 2019-05-21
### Added
### Changed
- `/decode-calldata/` endpoints - return ACI style JSON data also for this endpoint.
- Update to Sophia [v3.0.0](https://github.com/aeternity/aesophia/releases/tag/v3.0.0)
### Removed

## [2.1.0] - 2019-04-11
### Added
- `/decode-calldata/bytecode` endpoint - Identify function name and arguments in Calldata for a compiled contract
- `/decode-calldata/source` endpoint - Identify function name and arguments in Calldata for a (partial) contract
- CORS response headers support

### Changed
- Update to Sophia [v2.1.0](https://github.com/aeternity/aesophia/releases/tag/v2.1.0)


## [2.0.0] - 2019-03-11
### Added
- Sophia compiler [2.0.0](https://github.com/aeternity/aesophia/releases/tag/v2.0.0)
- Initial HTTP interface: /aci, /compile, /decode-data, /encode-calldata, /version, /api-version, /api
- Docker support (aeternity/aesophia_http)

[Unreleased]: https://github.com/aeternity/aesophia_http/compare/v8.0.0...HEAD
[8.0.0]: https://github.com/aeternity/aesophia_http/compare/v7.6.1...v8.0.0
[7.6.1]: https://github.com/aeternity/aesophia_http/compare/v7.6.0...v7.6.1
[7.6.0]: https://github.com/aeternity/aesophia_http/compare/v7.5.0...v7.6.0
[7.5.0]: https://github.com/aeternity/aesophia_http/compare/v7.4.0...v7.5.0
[7.4.0]: https://github.com/aeternity/aesophia_http/compare/v7.3.0...v7.4.0
[7.3.0]: https://github.com/aeternity/aesophia_http/compare/v7.2.0...v7.3.0
[7.2.0]: https://github.com/aeternity/aesophia_http/compare/v7.1.1...v7.2.0
[7.1.1]: https://github.com/aeternity/aesophia_http/compare/v7.1.0...v7.1.1
[7.1.0]: https://github.com/aeternity/aesophia_http/compare/v7.0.1...v7.1.0
[7.0.1]: https://github.com/aeternity/aesophia_http/compare/v7.0.0...v7.0.1
[7.0.0]: https://github.com/aeternity/aesophia_http/compare/v6.1.0...v7.0.0
[6.1.0]: https://github.com/aeternity/aesophia_http/compare/v6.0.2...v6.1.0
[6.0.2]: https://github.com/aeternity/aesophia_http/compare/v6.0.1...v6.0.2
[6.0.1]: https://github.com/aeternity/aesophia_http/compare/v6.0.0...v6.0.1
[6.0.0]: https://github.com/aeternity/aesophia_http/compare/v5.0.0...v6.0.0
[5.0.0]: https://github.com/aeternity/aesophia_http/compare/v4.3.2...v5.0.0
[4.3.2]: https://github.com/aeternity/aesophia_http/compare/v4.3.0...v4.3.2
[4.3.0]: https://github.com/aeternity/aesophia_http/compare/v4.2.1...v4.3.0
[4.2.1]: https://github.com/aeternity/aesophia_http/compare/v4.2.0...v4.2.1
[4.2.0]: https://github.com/aeternity/aesophia_http/compare/v4.1.0...v4.2.0
[4.1.0]: https://github.com/aeternity/aesophia_http/compare/v4.0.0...v4.1.0
[4.0.0]: https://github.com/aeternity/aesophia_http/compare/v3.2.0...v4.0.0
[3.2.0]: https://github.com/aeternity/aesophia_http/compare/v3.1.0...v3.2.0
[3.1.0]: https://github.com/aeternity/aesophia_http/compare/v3.0.0...v3.1.0
[3.0.0]: https://github.com/aeternity/aesophia_http/compare/v2.1.0...v3.0.0
[2.1.0]: https://github.com/aeternity/aesophia_http/compare/v2.0.0...v2.1.0
[2.0.0]: https://github.com/aeternity/aesophia_http/releases/tag/v2.0.0
