# Security Policy

## Supported Versions

| Version | Supported |
| ------- | --------- |
| 0.9.x   | Yes       |
| < 0.9   | No        |

## Reporting a Vulnerability

Please report security vulnerabilities by email to **hadesllm@proton.me**
with the subject line `[SECURITY] rmorie`. Encrypted reports preferred —
PGP key on Keybase at @rootcoder007.

We aim to:

1. Acknowledge within 72 hours.
2. Triage + initial response within 7 days.
3. Patch + coordinated disclosure within 30 days for high-severity issues
   (CVSS >= 7.0), 60 days for moderate.

Please do **not** open public GitHub issues for security reports.

## Threat model — what rmorie defends against

rmorie ships R code that talks to public open-data APIs over HTTPS. The
hardened paths assume:

- The host (your laptop / CI runner) is trusted.
- The user is trusted.
- All upstream open-data portals are *untrusted* — any response may be
  malicious (oversized payload, malformed JSON, injected R/SQL code in
  a string column, etc.).

**rmorie does not** automatically write remote content to your filesystem,
`eval(parse(...))` any remote string, or pass remote strings to `system()`.
HTTPS is enforced; cert verification is enabled by default; timeouts cap
network calls. If you find a function that violates these properties,
report it as a security issue.

## Supply-chain hardening

- All GitHub Actions are SHA-pinned (immutable references).
- Dependabot tracks GitHub-Actions + R-package updates.
- CodeQL scans on every push.
- Releases are GPG-signed where the runner supports it.

## Out of scope

- Public CRAN dependencies (we trust CRAN; report upstream).
- User-supplied data passed to rmorie functions (your responsibility).
- Race conditions in `tempdir()` (file the report against R itself).
