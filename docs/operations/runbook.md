# Operations Runbook

## Standard Run Sequence

1. Install dependencies.
2. Execute downloader with selected configuration.
3. Review logs and summary counts.

```bash
Rscript install_dependencies.R
Rscript main.R --years 24 --mode year_only
```

## Health Checks

- Confirm `output/` and `log/` directories are writable.
- Confirm `log/download_log_*.csv` is created for each run.
- Confirm `log/hash_registry.csv` updates as files are processed.

## Common Failure Modes

### Network/HTTP failures

- Symptoms: repeated retry messages, failed rows in download log.
- Action: reduce parallelism, retry, confirm NCES site availability.

### Missing Excel links

- Symptoms: no `excel_url` for a table row.
- Action: inspect source page and fallback URL behavior.

### Permission errors

- Symptoms: directory creation or file write errors.
- Action: adjust filesystem permissions for `output/` and `log/`.

## Incident Triage Checklist

1. Capture command used.
2. Capture failing table numbers and years.
3. Attach relevant `download_log` rows.
4. Open issue with reproducible command and observed errors.
