# NCES Digest Data Downloader

A streamlined tool for downloading Excel files from the National Center for Education Statistics (NCES) Digest of Education Statistics.

## Overview

This tool allows you to:

- Download tables from any NCES Digest year (e.g., 2024, 2023, etc.)
- Filter downloads by specific table numbers or years
- Track file changes using hash verification
- Easily resume interrupted downloads
- Process downloads reliably with built-in error handling

The strength of this program lies in its ability to handle variations across different Digests. For example, some files are in the `.xls` format while others are in `.xlsx`. By scraping the Digest menus, the system dynamically adjusts to download the correct file format. Additionally, because some Digest tables are not updated every year, the scraping process allows the program to locate available tables without needing to try every sequential permutation. The program also extracts metadata from `.xlsx` files to capture details about the table in a machine-readable format. Finally, the ability to download files in bulk—either by year or table number—significantly enhances the user experience.

This tool is designed for Digest users of all levels. Please share your feedback or report any defects or feature requests using the issues log on GitHub.

## Quick Start

### First-time Setup

1. Make sure you have R installed on your computer (version 3.6 or higher).
2. Download or clone this repository.
3. Run the setup script to install the required packages:

   ```
   Rscript install_dependencies.R
   ```

### Running the Downloader

1. Edit the configuration in `main.R` to select which years you want to download.
2. Run the main script:

   ```
   Rscript main.R
   ```

3. The downloaded files will be saved in the `output` directory.
4. Download logs will be saved in the `log` directory.

## Configuration Options

You can customize the downloader by editing the variables at the top of `main.R`:

```R
# Years to download (e.g., 24 = 2024, 23 = 2023, etc.)
YEARS_TO_DOWNLOAD <- c(24)

# Download mode:
# "all"        - Download all tables for the specified years
# "year_only"  - Only tables from specified years (default)
# "table_only" - Only specified tables (regardless of year)
# "custom"     - Both year and table filters applied
DOWNLOAD_MODE <- "year_only"

# When DOWNLOAD_MODE is "table_only" or "custom", specify table numbers
# e.g., c("101.10", "204.30") for specific tables
FILTER_TABLES <- c()

# Maximum number of simultaneous downloads (0 = auto-detect based on system)
MAX_PARALLEL_DOWNLOADS <- 0
```

You can also run the script with command-line arguments:

```
Rscript main.R --years 24,23 --mode year_only --output custom_folder
```

## Download Structure

The downloaded files are organized into a directory structure by year, chapter, and subchapter:

```
output/
├── d24/                           # Year (2024)
│   ├── chapter_1/                 # Chapter 1
│   │   ├── subchapter_101/        # Subchapter 101
│   │   │   ├── d24_tabn101_10.xlsx
│   │   │   └── d24_tabn101_20.xlsx
│   │   ├── subchapter_102/
│   │   └── ...
│   ├── chapter_2/
│   └── ...
├── d23/                           # Year (2023)
└── ...
```

## File Hash Registry

The program maintains a registry of file hashes in `log/hash_registry.csv` to track changes in files over time. This registry includes:

- File paths
- MD5 hashes
- Download dates
- Timestamps

## Project Structure

```
.
├── R/                      # Core utility functions
│   ├── config.R            # Configuration handling
│   └── utils.R             # Helper functions
├── code/                   # Main processing scripts
│   ├── URL_DIGEST.R        # Scrapes the Digest menu
│   ├── download_files.R    # Downloads Excel files
│   └── find_excel_path.R   # Extracts Excel links
├── main.R                  # Main script (edit this)
├── install_dependencies.R  # Package installer
├── log/                    # Download logs (auto-created)
└── output/                 # Downloaded files (auto-created)
```

## Troubleshooting

### Common Issues

1. **Missing Packages**:  
   Run `Rscript install_dependencies.R` to install all required packages.

2. **Download Failures**:  
   Some tables might not have associated Excel files available. Check the download log for details.

3. **Permission Errors**:  
   Ensure you have write permissions for the `output` and `log` directories.

4. **Parallel Processing Errors**:  
   The tool uses sequential processing by default for reliability. If you encounter errors with parallel processing, edit `download_files.R` and ensure `worker_count` is set to 1.

5. **Handler Errors**:  
   If you see a "handlers on the stack" error, ensure that `progressr` is not loaded in your R environment. The tool has been updated to avoid dependencies on progressr.

## Log Files

Detailed logs are saved in the `log` directory:

- `download_log_YYYYMMDD_HHMMSS.csv` – Information about each download attempt.
- `hash_registry.csv` – Registry of file hashes for change tracking.

## About NCES Digest Data

The [Digest of Education Statistics](https://nces.ed.gov/programs/digest/) provides a comprehensive compilation of statistical information covering education from kindergarten through graduate school, including data on enrollment, graduates, teachers, finances, and more.

## Disclaimer

This tool is not affiliated with or endorsed by the National Center for Education Statistics. Use of this tool should comply with NCES terms of service.

## License

This project is licensed under the MIT License – see the LICENSE file for details.
