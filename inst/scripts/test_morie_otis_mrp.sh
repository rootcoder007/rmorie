#!/usr/bin/env bash
# test_morie_otis_mrp.sh
#
# User-runnable reproducibility test for the OTIS A01RCDD MRP.
# Runs otis_MRP.R end-to-end into a timestamped output directory,
# producing CSVs for every result table and a manifest.json with
# expected vs observed values for every numerical claim.
#
# Vansh Singh Ruhela <vansh.ruhela@mail.utoronto.ca>
# ORCID 0009-0004-1750-3592
#
# USAGE:
#   ./test_morie_otis_mrp.sh
#   ./test_morie_otis_mrp.sh /path/to/alternative.RData
#
# Output:
#   ./results_<TIMESTAMP>/
#     ├── 01_orc_person_year.csv
#     ├── 02_descriptive_full_sample.csv
#     ├── 03_descriptive_matched_sample.csv
#     ├── 04_matched_sample.csv
#     ├── 05_nb_glmm_coefficients.csv
#     ├── 06_DML_res_pool.csv
#     ├── 07_DML_res_by_year.csv
#     ├── manifest.json
#     └── run.log
#

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
R_SCRIPT="${SCRIPT_DIR}/otis_MRP.R"
DEFAULT_RDATA="/Volumes/VSR/rootcoderfiles/OTIS-RC/correctional_stats_report_environment1b.RData"
INPUT_RDATA="${1:-${DEFAULT_RDATA}}"

TIMESTAMP="$(date +%Y%m%d-%H%M%S)"
OUTPUT_DIR="${SCRIPT_DIR}/results_${TIMESTAMP}"

mkdir -p "${OUTPUT_DIR}"

# -------- Locate Rscript ----------
RSCRIPT="$(command -v Rscript 2>/dev/null || true)"
if [[ -z "${RSCRIPT}" ]]; then
  for cand in /opt/homebrew/bin/Rscript /usr/local/bin/Rscript /usr/bin/Rscript; do
    [[ -x "${cand}" ]] && RSCRIPT="${cand}" && break
  done
fi
if [[ -z "${RSCRIPT}" ]]; then
  echo "ERROR: Rscript not found. Install R first." >&2
  exit 2
fi

# -------- Header ----------
cat <<HDR
==========================================================
test_morie_otis_mrp.sh — Reproducibility test
==========================================================
Script:       ${R_SCRIPT}
Input data:   ${INPUT_RDATA}
Output dir:   ${OUTPUT_DIR}
Rscript:      ${RSCRIPT}
Timestamp:    ${TIMESTAMP}
==========================================================

HDR

# -------- Sanity checks ----------
if [[ ! -f "${R_SCRIPT}" ]]; then
  echo "ERROR: otis_MRP.R not found at ${R_SCRIPT}" >&2
  exit 3
fi
if [[ ! -f "${INPUT_RDATA}" ]]; then
  echo "ERROR: input RData not found at ${INPUT_RDATA}" >&2
  exit 4
fi

# -------- Invoke R ----------
"${RSCRIPT}" "${R_SCRIPT}" "${INPUT_RDATA}" "${OUTPUT_DIR}" 2>&1 | tee "${OUTPUT_DIR}/run.log"

EXIT_CODE=${PIPESTATUS[0]}

# -------- Summary from manifest ----------
if [[ -f "${OUTPUT_DIR}/manifest.json" ]]; then
  echo ""
  echo "=========================================================="
  echo "PARSED MANIFEST SUMMARY"
  echo "=========================================================="
  # Use python for JSON parsing if available, otherwise grep
  if command -v python3 >/dev/null 2>&1; then
    python3 - <<PYEOF
import json, sys
with open("${OUTPUT_DIR}/manifest.json") as fh:
    m = json.load(fh)
results = m.get("results", {})
groups = {}
for name, r in results.items():
    g = r.get("group", "general")
    groups.setdefault(g, []).append((name, r))
total = len(results)
pass_n = sum(1 for r in results.values() if r["status"] == "PASS")
diff_n = sum(1 for r in results.values() if r["status"] == "DIFFER")
print(f"\nTotal checks:  {total}")
print(f"  PASS:        {pass_n}  ({100*pass_n/total:.1f}%)")
print(f"  DIFFER:      {diff_n}  ({100*diff_n/total:.1f}%)")
print("\nBy group:")
for g, items in sorted(groups.items()):
    pn = sum(1 for _, r in items if r["status"] == "PASS")
    dn = sum(1 for _, r in items if r["status"] == "DIFFER")
    print(f"  {g:20s}  PASS={pn:2d}  DIFFER={dn:2d}")
print("\nDIFFER details:")
for name, r in results.items():
    if r["status"] == "DIFFER":
        obs = r["observed"]; exp = r["expected"]
        obs_s = f"{obs:.4f}" if isinstance(obs,(int,float)) else str(obs)
        exp_s = f"{exp:.4f}" if isinstance(exp,(int,float)) else str(exp)
        print(f"  - {name}: observed={obs_s} expected={exp_s} diff={r['diff']:.4f}")
PYEOF
  else
    echo "(python3 not available — manifest at ${OUTPUT_DIR}/manifest.json)"
  fi
fi

echo ""
echo "Results: ${OUTPUT_DIR}"
echo "Exit code: ${EXIT_CODE}"
exit ${EXIT_CODE}
