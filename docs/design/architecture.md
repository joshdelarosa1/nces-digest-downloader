# Architecture

## Flow Overview

```text
[CLI / main.R]
      |
      +--> [R/config.R] load and validate config
      +--> [R/utils.R] retries, hashing, filesystem helpers
      +--> [code/URL_DIGEST.R] table metadata discovery
      +--> [code/find_excel_path.R] Excel link resolution
      +--> [code/download_files.R] file download and log persistence
      |
      +--> output/ (artifacts)
      +--> log/ (download log + hash registry)
```

## Component Responsibilities

- `main.R`: orchestration, argument handling, environment preparation.
- `R/config.R`: default config + validation safeguards.
- `R/utils.R`: shared utilities (HTML retrieval, directory creation, hashing, registry checks).
- `code/URL_DIGEST.R`: discovers available tables for configured years.
- `code/find_excel_path.R`: derives Excel URLs and table titles.
- `code/download_files.R`: downloads files, captures metadata, writes logs.

## Failure Handling Strategy

- Retry and backoff for HTTP requests.
- Guardrails for malformed URLs and small/non-Excel payloads.
- Hash registry for repeat-run integrity awareness.
- Detailed per-file status logs for triage.
