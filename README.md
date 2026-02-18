# NCES Digest Downloader

[![Docs CI](https://img.shields.io/github/actions/workflow/status/joshdelarosa1/nces-digest-downloader/docs.yml?branch=main&label=docs%20ci)](https://github.com/joshdelarosa1/nces-digest-downloader/actions/workflows/docs.yml)
[![Documentation](https://img.shields.io/badge/docs-MkDocs-blue)](docs/)
[![Coverage](https://img.shields.io/badge/coverage-TBD-lightgrey)](https://github.com/joshdelarosa1/nces-digest-downloader)

Automated retrieval of NCES Digest of Education Statistics Excel tables with filtering,
resumable downloads, and file-integrity logging.

## Project Overview

This project downloads Excel tables from the NCES Digest website, captures table metadata,
and writes structured outputs and logs for reproducible analysis workflows.

## Features

- Scrapes Digest table indexes for selected years.
- Filters by year, table number, or custom combinations.
- Extracts direct Excel links with fallback URL construction.
- Downloads files with retries, throttling, and integrity checks.
- Records hash and download metadata for change tracking.
- Supports parallel processing with conservative defaults.

## Architecture Summary

```text
main.R
  |
  +-- R/config.R              # config loading and validation
  +-- R/utils.R               # retries, filesystem, hashing helpers
  +-- code/URL_DIGEST.R       # scrape table catalog by digest year
  +-- code/find_excel_path.R  # resolve table -> excel URL
  +-- code/download_files.R   # download, verify, and log artifacts

Outputs:
  output/                     # downloaded Excel files
  log/                        # download logs + hash registry
```

## Prerequisites

- macOS, Linux, or Windows
- R >= 4.1.0
- Internet access for NCES downloads (runtime)
- Optional for docs contributors:
  - Python 3.10+
  - `mkdocs`

## Install

```bash
git clone https://github.com/joshdelarosa1/nces-digest-downloader.git
cd nces-digest-downloader
Rscript install_dependencies.R
```

## Quickstart

Run with defaults:

```bash
Rscript main.R
```

Run with explicit parameters:

```bash
Rscript main.R \
  --years 24,23 \
  --mode custom \
  --tables 101.10,204.30 \
  --parallel 2 \
  --output output
```

## Usage Examples

Download all tables for one year:

```bash
Rscript main.R --years 24 --mode year_only
```

Download specific tables across available years:

```bash
Rscript main.R --mode table_only --tables 101.10,204.30
```

## Configuration and Environment Variables

Primary configuration is defined in `main.R` defaults and optional CLI flags.
You can also pass a YAML config path where supported by your workflow.

| Setting | Purpose | Default |
|---|---|---|
| `years` | Two-digit digest years to process | `c(22)` |
| `filter_mode` | `all`, `year_only`, `table_only`, `custom` | `year_only` |
| `filter_years` | Year filters with `d` prefix | from `years` |
| `filter_tables` | Specific table IDs | empty |
| `max_parallel` | Parallel worker count (`0` = auto) | `0` |
| `output_dir` | Download target directory | `output` |
| `log_dir` | Log output directory | `log` |
| `resume_previous` | Resume behavior for existing artifacts | `TRUE` |
| `verbose` | Verbose messaging | `FALSE` |

Environment variables:

- Copy `.Renviron.example` to `.Renviron` and set local values.
- Runtime config is read with `Sys.getenv()` via `get_env()` in `R/config.R`.
- Keep `.Renviron` and `.env*` local only; they are ignored by Git.

Example local setup:

```bash
cp .Renviron.example .Renviron
echo 'NCES_BASE_URL=https://nces.ed.gov/programs/digest/' >> .Renviron
```

## Troubleshooting

### Missing packages

Run:

```bash
Rscript install_dependencies.R
```

### Download failures

- Some table pages do not expose valid Excel links.
- Review `log/download_log_*.csv` and retry with lower parallelism.

### Permission errors

- Ensure write permissions to `output/` and `log/`.

### Unexpected HTML instead of Excel

- The downloader validates content size and file signatures.
- Check network restrictions and NCES availability.

## FAQ

### Does this project support offline data downloads?
No. NCES data retrieval requires network access.

### Can documentation be built offline?
Yes. After installing docs tooling once, docs generation/build checks run without network calls.

### Which changelog is canonical?
`CHANGELOG.md` is canonical. `NEWS.md` is retained as legacy history.

## Documentation

- Docs source: `docs/`
- Site config: `mkdocs.yml`
- Migration note: `docs/migration-note.md`

Build docs locally:

```bash
Rscript scripts/generate_reference_docs.R
mkdocs build --strict
```

## Security

Run local security scans before opening a PR:

```bash
scripts/scan_secrets.sh
Rscript scripts/scan_code.R
```

Disclosure summary:

- Do not report vulnerabilities in public issues.
- Use the private reporting workflow in `SECURITY.md`.
- Include impact, repro steps, and affected versions.

## Support

- Bugs and feature requests: GitHub Issues
- Security reports: see `SECURITY.md`
- Contributor workflow: see `CONTRIBUTING.md`

## License

Licensed under the MIT License. See `LICENSE`.

## Acknowledgements

- National Center for Education Statistics (NCES)
- R package ecosystem maintainers (tidyverse, httr, rvest, furrr, and related packages)
