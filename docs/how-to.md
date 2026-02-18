# How-To Guides

## Download One Digest Year

```bash
Rscript main.R --years 24 --mode year_only
```

## Download Specific Tables

```bash
Rscript main.R --mode table_only --tables 101.10,204.30
```

## Combine Year and Table Filters

```bash
Rscript main.R --years 24,23 --mode custom --tables 101.10,204.30
```

## Resume Existing Downloads

`main.R` defaults to resume behavior through `RESUME_PREVIOUS <- TRUE`.

Run again with the same configuration to skip already-present artifacts.

## Adjust Parallelism Conservatively

```bash
Rscript main.R --parallel 1
```

Use low values when debugging or when NCES responses are unstable.

## Change Output Directory

```bash
Rscript main.R --output output_custom
```

## Troubleshoot Missing Excel Links

1. Review `log/download_log_*.csv` for `error_message` details.
2. Retry with `--parallel 1`.
3. Validate whether the source table page still publishes an Excel artifact.
