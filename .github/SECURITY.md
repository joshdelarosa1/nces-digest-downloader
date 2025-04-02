# Security Policy

## Reporting a Vulnerability

If you discover a security vulnerability in this project, we encourage responsible disclosure.

Please report it **privately** by contacting:

📧 josh.delarosa1@gmail.com

Do not create a public issue. We will respond within 2 business days and work to resolve the issue promptly.

---

## Supported Versions

| Version | Supported | Notes                                |
|---------|-----------|--------------------------------------|
| 1.1.0   | ✅ Yes     | Latest stable release                |
| <1.1.0  | ❌ No      | Please upgrade to the latest version |

---

## Security Measures in Place

This repository includes the following built-in security features:

- **URL validation** and **retry limits** on download attempts to prevent denial-of-service (DoS) or abuse of remote servers.
- **Exponential backoff** and **throttling** in all HTTP requests to avoid rate limiting and reduce network risk.
- **SHA hash tracking** to verify file integrity and detect tampering.
- **Isolated parallel processing** using `{furrr}` with seed-based reproducibility for safe, deterministic execution across threads.
- **Input validation** for URLs and table filters to avoid malformed or unsafe scraping operations.
- **Dependency setup** via a dedicated `install_dependencies.R` script to ensure consistent environment configuration.

---

## Additional Recommendations

- Use caution when running this code in automated pipelines or on shared infrastructure.
- Ensure secure file permissions on the `log/` and `output/` directories.
- Validate downloaded files and monitor changes using the built-in hash registry.

---

## Dependencies and Vulnerabilities

This project uses publicly available R packages from CRAN. Any security issues discovered in these dependencies should be reported through:

- The [R Project Bugzilla](https://bugs.r-project.org/)
- GitHub's built-in [Dependabot Alerts](https://docs.github.com/en/code-security)

---

## Disclaimer

This software is provided **"as is"** without warranty of any kind. Use at your own risk and ensure compliance with the [NCES Digest of Education Statistics](https://nces.ed.gov/programs/digest/) data use policies.

---

Thank you for helping keep this project secure!