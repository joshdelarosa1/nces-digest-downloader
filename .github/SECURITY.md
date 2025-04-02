# Security Policy

## Reporting a Vulnerability

If you discover a security vulnerability in this project, we encourage responsible disclosure.

Please **report it privately via email** to:

📧 josh.delarosa1@gmail.com

Avoid publicly disclosing the issue until we have had a chance to review and address it.

---

## Supported Versions

| Version | Supported | Notes                                |
|---------|-----------|--------------------------------------|
| 1.0.1   | ✅ Yes     | Latest release as of April 2025      |
| <1.0.0  | ❌ No      | Please upgrade to the latest version |

---

## Security Measures in Place

This repository includes the following built-in security features:

- **URL validation** and **retry limits** on download attempts to prevent DoS or server abuse&#8203;:contentReference[oaicite:0]{index=0}.
- **Exponential backoff and throttling** in all HTTP requests to avoid rate limiting and reduce network risk&#8203;:contentReference[oaicite:1]{index=1}.
- **SHA hash tracking** for verifying file integrity and preventing tampering&#8203;:contentReference[oaicite:2]{index=2}.
- **Isolated parallel processing** via `{furrr}` with seed-based reproducibility for safe multi-threaded execution&#8203;:contentReference[oaicite:3]{index=3}.
- **Input validation** for URLs and table filters to prevent malformed downloads or script errors&#8203;:contentReference[oaicite:4]{index=4}.
- Dependencies are managed and checked using a dedicated setup script (`install_dependencies.R`)&#8203;:contentReference[oaicite:5]{index=5}.

---

## Additional Recommendations

- If using this code on shared systems or servers, validate all configuration inputs (especially table names and paths).
- Ensure proper file permissions for the `output/` and `log/` directories.
- Monitor your network activity if using bulk downloads in automated environments.

---

## Dependencies and Vulnerabilities

This project uses several R packages from CRAN and Bioconductor. While CRAN maintains a generally secure ecosystem, known vulnerabilities in packages should be reported via:

- [R security bugzilla](https://bugs.r-project.org/)
- GitHub’s [Dependabot alerts](https://docs.github.com/en/code-security)

---

## Disclaimer

This tool is provided **as-is** without warranty. Use it responsibly and ensure compliance with the NCES data usage policies.

If you have further questions or need support, please open an issue on GitHub or contact the maintainer directly.
