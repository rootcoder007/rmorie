#!/bin/sh
# Refuses tracked files that are working notes or per-developer tool
# settings; they belong outside the repository.
bad=$(git ls-files | grep -E '(^|/)(BRANCH_PLAN|CLAUDE|GEMINI|AGENTS)\.md$|_SHELF\.md$|_TRIAGE\.md$|PLACEHOLDERS\.md$|LIBRARY-SCAN|(^|/)council\.py$|backup_four_way' || true)
if [ -z "$bad" ]; then echo "private-file gate: OK"; exit 0; fi
echo "Working notes or tool settings must not be committed:"
echo "$bad"
exit 1
