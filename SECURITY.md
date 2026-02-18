# Security Policy

## Supported Versions

| Version | Supported |
|---|---|
| 1.1.x | Yes |
| 1.0.x | No |
| < 1.0.0 | No |

## Reporting a Vulnerability

Report vulnerabilities privately by emailing `josh.delarosa1@gmail.com`.

Please include:

- A clear description of the issue
- Reproduction steps or proof-of-concept
- Impact assessment
- Suggested remediation (if available)

Do not disclose vulnerabilities in public issues or pull requests.

When reporting potential credential leaks, do not include raw secret values.
Mask values as `***` and include only short fingerprints.

## Response Targets

- Initial acknowledgement: within 2 business days
- Triage decision: within 5 business days
- Status updates: at least every 7 calendar days while open

## Disclosure Process

1. Receive and validate report.
2. Assess severity and scope.
3. Prepare and test remediation.
4. Coordinate disclosure timeline with reporter.
5. Publish fix and release notes.

## Security Practices in This Repository

- Input validation on scraping and download flows
- Retry limits, throttling, and backoff for remote requests
- Hash-based file integrity registry
- Deterministic test scope for local unit tests
- Local secret/code scanning scripts under `scripts/`

## Local Verification

Run these checks before opening a security-sensitive PR:

```bash
scripts/scan_secrets.sh
Rscript scripts/scan_code.R
Rscript -e 'testthat::test_dir("tests/testthat")'
```

If credentials were exposed, rotate/revoke them immediately and review
`scripts/sanitize-history.sh` before public disclosure.

## Out of Scope

- Vulnerabilities in third-party dependencies that are not directly controlled in this repository
  (report upstream where appropriate)
- Issues requiring unsupported versions
