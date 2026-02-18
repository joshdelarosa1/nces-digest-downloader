# Getting Started

## Prerequisites

- R >= 4.1.0
- Internet access for NCES data downloads
- Optional for docs contributors: Python 3.10+ and `mkdocs`

## Install Dependencies

```bash
Rscript install_dependencies.R
```

## First Run

```bash
Rscript main.R
```

## Expected Outputs

After a successful run:

- Downloaded tables in `output/`
- Execution logs in `log/`
- Hash registry in `log/hash_registry.csv`

## Verify Unit Tests

```bash
Rscript -e 'testthat::test_dir("tests/testthat")'
```

Note: existing repository baseline tests may fail independently of docs-only changes.

## Build Documentation Locally

```bash
Rscript scripts/generate_reference_docs.R
mkdocs build --strict
```

These commands are designed to run offline once dependencies are already installed.
