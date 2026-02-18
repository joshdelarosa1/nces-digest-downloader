#!/usr/bin/env bash
set -euo pipefail

REPORT_DIR="reports"
SUMMARY_FILE="${REPORT_DIR}/secrets_scan.txt"
RG_FILE="${REPORT_DIR}/rg_secrets.txt"
DETECT_FILE="${REPORT_DIR}/detect_secrets.json"
GITLEAKS_FILE="${REPORT_DIR}/gitleaks.json"

mkdir -p "${REPORT_DIR}"

critical_count=0
high_count=0

printf "Secret scan started: %s\n" "$(date -u +"%Y-%m-%dT%H:%M:%SZ")" > "${SUMMARY_FILE}"

if command -v detect-secrets >/dev/null 2>&1; then
  if detect-secrets scan --all-files > "${DETECT_FILE}" 2>/dev/null; then
    findings_count=$(rg -n '"results"\s*:\s*\{\s*\}' "${DETECT_FILE}" >/dev/null && echo 0 || echo 1)
    if [ "${findings_count}" -gt 0 ]; then
      high_count=$((high_count + 1))
    fi
  else
    printf "detect-secrets: scan failed\n" >> "${SUMMARY_FILE}"
    high_count=$((high_count + 1))
  fi
else
  printf "detect-secrets: not installed\n" >> "${SUMMARY_FILE}"
fi

if command -v gitleaks >/dev/null 2>&1; then
  if gitleaks detect --no-banner --redact --source . \
    --config .gitleaks.toml --report-format json --report-path "${GITLEAKS_FILE}"; then
    :
  else
    printf "gitleaks: findings detected or scan failed\n" >> "${SUMMARY_FILE}"
    high_count=$((high_count + 1))
  fi
else
  printf "gitleaks: not installed\n" >> "${SUMMARY_FILE}"
fi

# Use redacted match replacement to avoid printing raw candidate values.
rg -n --hidden -g '!.git' -g '!renv/**' -g '!packrat/**' \
  -e 'AKIA[0-9A-Z]{16}' \
  -e 'gh[pousr]_[A-Za-z0-9]{20,}' \
  -e 'xox[baprs]-[A-Za-z0-9-]{10,}' \
  -e 'AIza[0-9A-Za-z_-]{35}' \
  -e '(?i)(api[_-]?key|client_secret|password|passwd|authorization|bearer)\s*[:=]\s*\S+' \
  -e '-----BEGIN (RSA|EC|OPENSSH|PRIVATE KEY)-----' \
  --only-matching --replace '[MASKED_MATCH]' . > "${RG_FILE}" || true

rg_matches=$(wc -l < "${RG_FILE}" | tr -d ' ')
if [ "${rg_matches}" -gt 0 ]; then
  high_count=$((high_count + 1))
fi

# Key material is treated as critical.
if rg -q 'PRIVATE KEY' "${RG_FILE}"; then
  critical_count=$((critical_count + 1))
fi

{
  printf "critical_count=%s\n" "${critical_count}"
  printf "high_count=%s\n" "${high_count}"
  printf "rg_matches=%s\n" "${rg_matches}"
  printf "reports=%s,%s,%s\n" "${RG_FILE}" "${DETECT_FILE}" "${GITLEAKS_FILE}"
} >> "${SUMMARY_FILE}"

if [ "${critical_count}" -gt 0 ] || [ "${high_count}" -gt 0 ]; then
  printf "Secret scan failed: critical/high findings detected.\n" >&2
  exit 1
fi

printf "Secret scan passed with no critical/high findings.\n"
