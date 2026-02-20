# Release Process

## Versioning Policy

- Semantic Versioning (`MAJOR.MINOR.PATCH`)
- Changelog format: Keep a Changelog
- Commit style: Conventional Commits

## Release Checklist

1. Update `CHANGELOG.md` (`Unreleased` -> new version section).
2. Update version metadata (`DESCRIPTION`, `project_config.yml`).
3. Run validation commands:

```bash
Rscript -e 'testthat::test_dir("tests/testthat")'
Rscript scripts/generate_reference_docs.R
mkdocs build --strict
Rscript scripts/check_local_links.R README.md docs
```

1. Create a signed release commit.
2. Create a signed tag (`vX.Y.Z`).
3. Publish release notes from `CHANGELOG.md`.

## Suggested Conventional Commit Mapping

- `feat:` -> typically `MINOR`
- `fix:` -> typically `PATCH`
- `feat!` or `BREAKING CHANGE:` -> `MAJOR`
- `docs:`, `chore:`, `test:` -> no direct version bump unless paired with behavior changes
