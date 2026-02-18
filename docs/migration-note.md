# Migration Note

## 2026-02-17 Documentation Restructure

This repository documentation was reorganized to centralize canonical project docs and add a
buildable documentation site.

### What Changed

- Added root canonical governance docs:
  - `CONTRIBUTING.md`
  - `CODE_OF_CONDUCT.md`
  - `SECURITY.md`
  - `CHANGELOG.md`
- Converted `.github` policy files into stubs pointing to root canonical files.
- Added MkDocs site structure under `docs/` with navigation and operational pages.
- Added generated reference docs pipeline (`scripts/generate_reference_docs.R`).

### What Stayed the Same

- Runtime downloader behavior and script flow.
- Existing license and repository structure.
- Legacy `NEWS.md` kept for historical continuity.
