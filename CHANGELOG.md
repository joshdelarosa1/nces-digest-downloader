# Changelog

All notable changes to this project will be documented in this file.

The format is based on Keep a Changelog, and this project adheres to Semantic Versioning.

## [Unreleased]

### Added

- Canonical root governance docs (`CONTRIBUTING.md`, `CODE_OF_CONDUCT.md`, `SECURITY.md`)
- MkDocs documentation site scaffold under `docs/`
- Docs CI workflow for docs build, markdown lint, and local link checks
- Reference generation script for API and script function inventory

### Changed

- `README.md` rewritten for architecture, usage, configuration, troubleshooting, and docs workflow
- `.github` governance files converted to stubs pointing at root canonical files

## [1.1.0] - 2025-04-01

### Added

- Full integration of parallel scraping and downloading using `{furrr}`
- Page title enhancement logic for NCES Digest metadata (`enhance_with_page_titles`)
- Robust hash-based integrity verification of downloaded Excel files
- Extraction of document metadata from `.xlsx` files
- Throttling logic to regulate HTTP request intervals and prevent server overload

### Improved

- Enhanced error handling in network operations (`safe_read_html`, `download_excel_file`)
- Expanded retry logic using exponential backoff with jitter
- Browser-like headers for better NCES server compatibility
- Modular logging system with timestamped log files and hash registry

### Security

- Added vulnerability disclosure guidance and security policy
- URL input validation and guards against malformed scraping calls

## [1.0.0] - 2023-03-15

### Added

- Initial implementation of Digest scraping and Excel file downloads
- Command-line interface via `main.R`
- Logging and configuration module structure
