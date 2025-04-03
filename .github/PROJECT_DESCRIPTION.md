# NCES Digest Downloader - Project Description

## Overview

The NCES Digest Downloader is designed to help users download and process Excel files from the National Center for Education Statistics (NCES) Digest of Education Statistics. It offers customizable filtering, robust error handling, and parallel downloading capabilities.

## Key Features

- **Download Automation:** Automatically downloads Excel files based on user-defined criteria.
- **Filtering Options:** Supports filtering by digest year, table number, or both.
- **Resilient Downloading:** Implements throttling, exponential backoff, and retries to handle network issues gracefully.
- **Centralized Metadata Management:** Uses a YAML configuration (`project_config.yml`) to maintain version, release date, and changelog, with an update script (`update_project_metadata.R`) to propagate these details across the project.
- **Logging and Verification:** Maintains detailed logs and uses file hash verification to ensure data integrity.

## Responsible Use

Users must operate the NCES Digest Downloader responsibly:
- **Compliance:** Ensure that your usage complies with NCES terms of service.
- **Throttling & Parallelism:** Use the default (or conservative) settings for throttling and parallel downloads to avoid overloading NCES servers.
- **Ethical Use:** Employ the tool for legitimate research or educational purposes only.

## Contribution and Maintenance

For details on contributing to this project, please see [CONTRIBUTING.md](CONTRIBUTING.md).

## Versioning

Project metadata is maintained via `project_config.yml`. Current version: **1.1.0**.

## License

This project is licensed under the MIT License.
