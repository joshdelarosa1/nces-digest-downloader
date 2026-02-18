# Security Findings (Local Offline Scan)

Scan date: 2026-02-18 (UTC)

## 1) Suspected secrets

No high-confidence secrets were detected in tracked files.

- `reports/rg_secrets.txt`: 0 masked matches
- `reports/secrets_scan.txt`: `critical_count=0`, `high_count=0`

Notes:
- `detect-secrets` and `gitleaks` were not installed locally during this run, so their local reports are placeholders.
- CI workflows enforce both scanners with network-enabled runners.

## 2) High-risk code issues

No active high/critical code-risk patterns remain after remediation.

Remediated in this patch:
- Replaced shell-interpolated fallback command in `update_project_metadata.R:228` with `system2()` argument-safe invocation.

Current medium findings:
- `reports/code_scan.json`: style/lint-only findings.
- `DESCRIPTION:1`: package check is intentionally skipped by local scanner when `NAMESPACE` is absent.

## 3) Large files (>10MB)

No tracked files above 10MB were found in Git history tip.

Suggested action:
- Keep binary artifacts out of Git where possible.
- Use Git LFS for large `.rds`, `.qs`, `.png`, `.pdf`, `.zip` assets when needed.

## 4) Remediation and next actions

1. Rotate/revoke any credentials that may have been exposed before this hardening pass.
2. Install local tooling and re-run:
   - `detect-secrets scan --all-files > reports/detect_secrets.json`
   - `gitleaks detect --source . --config .gitleaks.toml --report-path reports/gitleaks.json`
3. Run:
   - `scripts/scan_secrets.sh`
   - `Rscript scripts/scan_code.R`
4. Verify CI security workflow status before release.
