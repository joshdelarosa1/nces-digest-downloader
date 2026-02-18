# Contributing

## Scope

This project uses SemVer for releases and Conventional Commits for commit messages.
Contributions should preserve current runtime behavior unless explicitly approved.

## Development Workflow

1. Create a branch from `main`.
2. Use a descriptive branch name, for example:
   - `docs/update-mkdocs-navigation`
   - `fix/download-retry-logging`
3. Make focused, reviewable commits.
4. Open a pull request with problem statement, changes, and validation results.

## Branching and Commits

- Default branch: `main`
- Commit style: Conventional Commits (`feat:`, `fix:`, `docs:`, `chore:`)
- Keep commit history clear and atomic.

Example commit messages:

- `docs: add MkDocs runbook and release process pages`
- `fix: improve hash registry error handling`

## Code Style and Formatting

### R code

- Use explicit namespaces in scripts when conflicts are likely.
- Prefer readable names (`snake_case`) and deterministic behavior.
- Avoid hidden state and avoid changing working directory inside scripts.

### Documentation

- Keep headings logically ordered.
- Use accessible, inclusive language.
- Ensure local links and anchors resolve.

## Testing and Validation

Run these commands before opening a pull request:

```bash
Rscript -e 'testthat::test_dir("tests/testthat")'
Rscript scripts/generate_reference_docs.R
mkdocs build --strict
markdownlint "**/*.md" --ignore site --ignore node_modules
Rscript scripts/check_local_links.R README.md docs
scripts/scan_secrets.sh
Rscript scripts/scan_code.R
```

If tests currently fail due unrelated baseline issues, document failures clearly in the PR.

## Documentation Workflow

- Root canonical docs:
  - `CONTRIBUTING.md`
  - `CODE_OF_CONDUCT.md`
  - `SECURITY.md`
  - `CHANGELOG.md`
- `.github/*.md` policy files are thin stubs that point to root canonical files.

## Release Process (SemVer)

1. Update `CHANGELOG.md` under `Unreleased` and/or create version section.
2. Update `DESCRIPTION` and `project_config.yml` versions.
3. Validate tests and docs checks.
4. Create a signed release commit and signed tag.
5. Publish release notes from `CHANGELOG.md`.

## Security Disclosures

Do not open public issues for vulnerabilities.
Report privately using guidance in `SECURITY.md`.

## Pre-commit and Style

- Install hooks with `pre-commit install`.
- Use `{styler}` conventions through the `style-files` pre-commit hook.
- Keep tests deterministic with `{testthat}` and avoid external-state dependencies.
- Optional DCO sign-off is supported (`git commit -s`).

## Code of Conduct

By participating, you agree to follow `CODE_OF_CONDUCT.md`.
