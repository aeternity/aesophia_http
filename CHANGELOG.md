# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]
### Added
### Changed
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

[Unreleased]: https://github.com/aeternity/aesophia_http/compare/v2.1.0...HEAD
[2.1.0]: https://github.com/aeternity/aesophia_http/compare/v2.0.0...v2.1.0
[2.0.0]: https://github.com/aeternity/aesophia_http/releases/tag/v2.0.0
