# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Dockerfile -- reproducible rmorie environment.
#
# Builds a clean Linux image with R + all of rmorie's system + R
# dependencies pre-installed, so a user can do:
#
#     docker pull ghcr.io/rootcoder007/rmorie:latest
#     docker run --rm -it ghcr.io/rootcoder007/rmorie R -e 'library(rmorie)'
#
# Multi-stage build:
#   1. Builder stage: compiles the package source (Rcpp + C++).
#   2. Runtime stage: minimal image with only the installed library.
#
# Base image: rocker/r-ver pinned to a specific R minor version so
# the image is reproducible across rebuilds.

# ---- Stage 1: builder ----
FROM rocker/r-ver:4.6.1 AS builder

ARG DEBIAN_FRONTEND=noninteractive
ENV TZ=Etc/UTC

# System libraries rmorie's C++ backends link against.
# - libcurl:    HTTP client (rmorie's libcurl-backed ingest)
# - libssl, libsodium: classical crypto family
# - libxml2:    httr2 + xml2 (Suggests)
# - pkg-config: configure script probes
#
# liboqs (post-quantum: ML-KEM-768 + ML-DSA-65) is intentionally NOT
# installed in this image. It is not packaged in stable Ubuntu repos
# (still too new), and ships only via the Open Quantum Safe Project's
# own builds. rmorie's PQC functions detect liboqs absence at runtime
# via morie_crypto_liboqs_available() and return a clear error. Users
# who need PQC can either:
#   (a) build a custom image on top of this one with liboqs from source
#   (b) install rmorie from source on a host with liboqs pre-installed
RUN apt-get update && apt-get install -y --no-install-recommends \
      git \
      pkg-config \
      libcurl4-openssl-dev \
      libssl-dev \
      libsodium-dev \
      libxml2-dev \
      libfontconfig1-dev \
      libfreetype6-dev \
      libharfbuzz-dev \
      libfribidi-dev \
      libpng-dev \
      libtiff5-dev \
      libjpeg-dev \
      libicu-dev \
      ca-certificates \
   && rm -rf /var/lib/apt/lists/*

WORKDIR /build
COPY DESCRIPTION NAMESPACE configure configure.win cleanup ./
COPY R/ R/
COPY src/ src/
COPY inst/ inst/
COPY man/ man/

# Install only hard dependencies (Imports + LinkingTo). Suggests
# stay opt-in so the image is small. Users opt-in at runtime via
# `rmorie::rmorie_install_extras()`.
#
# remotes::install_deps reads DESCRIPTION and installs all non-base
# Imports + LinkingTo packages. Hard-listing them in
# install.packages() drifts (the wrapper-as-extender campaign added
# `here` to Imports; the old line installed only Rcpp/RcppArmadillo
# and the build failed with "dependency 'here' is not available").
RUN R -e 'install.packages("remotes", repos = "https://p3m.dev/cran/__linux__/noble/latest"); \
          remotes::install_deps(".", dependencies = c("Depends","Imports","LinkingTo"), \
                                repos = c("https://rootcoder007.r-universe.dev", "https://p3m.dev/cran/__linux__/noble/latest"), \
                                upgrade = "never")'
RUN R CMD INSTALL --no-test-load --no-help --no-html .

# Smoke-check the package loads and the new tox surface is exported.
RUN R -e 'library(rmorie); stopifnot(is.function(morie_tox_calibration))' >/dev/null

# ---- Stage 2: runtime ----
FROM rocker/r-ver:4.6.1 AS runtime

ARG DEBIAN_FRONTEND=noninteractive
ENV TZ=Etc/UTC
ENV R_LIBS_USER=/usr/local/lib/R/site-library

# Only the runtime shared libraries (no -dev packages).
# liboqs deliberately absent here too -- matches the builder stage.
RUN apt-get update && apt-get install -y --no-install-recommends \
      libcurl4 \
      libssl3 \
      libsodium23 \
      libxml2 \
      ca-certificates \
   && rm -rf /var/lib/apt/lists/*

# Copy the installed R library + the source's inst/ for `extdata/` access.
COPY --from=builder /usr/local/lib/R/site-library/ /usr/local/lib/R/site-library/

# Drop to a non-root user; never run R as root inside the container.
RUN useradd -m -s /bin/bash rmorie
USER rmorie
WORKDIR /home/rmorie

# Smoke-load on `docker run` with no args; otherwise pass through to R.
ENTRYPOINT ["R", "--no-save"]
CMD ["-e", "library(rmorie); cat(\"rmorie OK\\n\")"]
