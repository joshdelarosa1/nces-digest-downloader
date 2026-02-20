# Changelog

All notable changes to this project will be documented in this file.

---

## [1.1.0] - 2025-04-01

### ✨ Added

- Full integration of parallel scraping and downloading using `{furrr}`
- Page title enhancement logic for NCES Digest metadata (`enhance_with_page_titles`)
- Robust hash-based integrity verification of downloaded Excel files
- Extraction of document metadata from `.xlsx` files (e.g., author, created date)
- Throttling logic to regulate HTTP request intervals and prevent server overload

### 🛠️ Improved

- Enhanced error handling in all network operations (`safe_read_html`, `download_excel_file`)
- Expanded retry logic using exponential backoff with jitter
- Browser-like headers for better server compatibility with NCES sites
- Modular logging system with timestamped log files and hash registry

### 🔐 Security

- Added `.github/SECURITY.md` with responsible disclosure policy
- Enabled GitHub Dependabot alerts and security updates
- Validated all URL inputs and prevented NA/null scraping calls

---

## [1.0.0] - 2023-03-15

### 🛠️ Initial Release

- Basic implementation of NCES Digest scraping and Excel file downloads
- CLI interface via `main.R`
- Project structure setup with logging, configuration, and utility modules
