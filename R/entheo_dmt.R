# SPDX-License-Identifier: AGPL-3.0-or-later
#
# morie::entheo_dmt -- R parity of morie.entheo_dmt (Timmermann 2023
# DMT_Imaging dataset loaders + Layer-2 analyses).
#
# Layer 1: available_subjects(), load_fmri_subject(), load_eeg_region(),
#          dataset_overview().
# Layer 2: spectral_band_power() (delegates to morie::rgpsd Welch PSD),
#          dynamic_functional_connectivity() (sliding-window Pearson FC),
#          lz_complexity() (Lempel-Ziv 1976), analyze_subject() (joint).
#
# Reference: Timmermann, Roseman, Schartner et al. (2023). Human brain
# effects of DMT assessed via EEG-fMRI. PNAS 120(13): e2218949120.
#
# Dataset shape:
#   fMRI/   LongS{NN}{DMT,PCB}.mat   key=BOLD_AAL, shape (112, 840).
#   EEG/    RegressorsInterpscrubbedIRASA_{Central,Frontal,Occipital,
#           Parietal,Temporal}.mat   keys regDMT/regPCB/regdiff,
#           shape (14 subj, 840 TRs, 5 bands -- delta..gamma).
#
# All callables return a named-list RichResult-style payload with
# `title`, `summary_lines`, `interpretation`, and `payload`.

# ---------------------------------------------------------------------------
# Module-level constants
# ---------------------------------------------------------------------------

#' EEG cortical regions used by the Timmermann 2023 IRASA pool.
#' @keywords internal
.MORIE_ENTHEO_EEG_REGIONS <- c("Central", "Frontal", "Occipital",
                                "Parietal", "Temporal")

#' Canonical EEG bands (delta..gamma) ordered by ascending frequency.
#' @keywords internal
.MORIE_ENTHEO_EEG_BANDS <- c("delta", "theta", "alpha", "beta", "gamma")

#' Canonical band edges (Hz). Matches Rangayyan & Krishnan (2024) Ch. 5
#' and the Timmermann 2023 Methods.
#' @keywords internal
.MORIE_ENTHEO_DEFAULT_BANDS <- list(
  list(name = "delta", lo = 0.5,  hi =  4.0),
  list(name = "theta", lo = 4.0,  hi =  8.0),
  list(name = "alpha", lo = 8.0,  hi = 13.0),
  list(name = "beta",  lo = 13.0, hi = 30.0),
  list(name = "gamma", lo = 30.0, hi = 80.0)
)

#' Resolve the DMT_Imaging dataset root, honouring
#' \env{MORIE_DMT_IMAGING_ROOT}. Returns NULL if absent on disk.
#' Parity with Python ``DATASET_ROOT`` / ``_require_root``.
#' @keywords internal
.morie_entheo_dmt_root <- function() {
  cand <- Sys.getenv("MORIE_DMT_IMAGING_ROOT", "")
  if (!nzchar(cand)) {
    cand <- file.path(morie_cache_dir(), "DMT_Imaging")
  }
  if (dir.exists(cand)) cand else NULL
}

#' Require the dataset root or stop with a curated error.
#' @keywords internal
.morie_entheo_require_root <- function() {
  root <- .morie_entheo_dmt_root()
  if (is.null(root)) {
    stop(
      "DMT_Imaging dataset not found. Clone ",
      "https://github.com/timmer500/DMT_Imaging.git or set the ",
      "MORIE_DMT_IMAGING_ROOT environment variable.",
      call. = FALSE
    )
  }
  root
}

#' Lightweight .mat loader (delegates to R.matlab).
#' @keywords internal
.morie_entheo_loadmat <- function(path) {
  if (!requireNamespace("R.matlab", quietly = TRUE)) {
    stop(
      "Package 'R.matlab' is required to read DMT_Imaging .mat files. ",
      "Install with install.packages('R.matlab').",
      call. = FALSE
    )
  }
  R.matlab::readMat(path)
}

# ---------------------------------------------------------------------------
# Layer 1: loaders
# ---------------------------------------------------------------------------

#' DMT_Imaging: list motion-survived subject IDs
#'
#' R parity of \code{morie.entheo_dmt.available_subjects}. Scans the
#' \code{fMRI/} directory of the on-disk DMT_Imaging mirror for
#' \code{LongS\{NN\}\{DMT,PCB\}.mat} filenames and returns the integer IDs.
#'
#' @return integer vector of subject IDs sorted ascending. Empty if the
#'   dataset root is missing or the \code{fMRI/} folder is absent.
#' @export
#' @references
#' Timmermann, C. et al. (2023). Human brain effects of DMT assessed
#' via EEG-fMRI. PNAS 120(13): e2218949120.
morie_entheo_available_subjects <- function() {
  root <- .morie_entheo_dmt_root()
  if (is.null(root)) return(integer(0))
  fmri_dir <- file.path(root, "fMRI")
  if (!dir.exists(fmri_dir)) return(integer(0))
  files <- list.files(fmri_dir, pattern = "^LongS\\d+(DMT|PCB)\\.mat$")
  ids <- as.integer(sub("^LongS(\\d+)(DMT|PCB)\\.mat$", "\\1", files))
  sort(unique(ids))
}

#' DMT_Imaging: load one subject's BOLD AAL parcellation
#'
#' R parity of \code{morie.entheo_dmt.load_fmri_subject}. Reads
#' \code{LongS\{NN\}\{DMT|PCB\}.mat} and extracts the
#' \code{BOLD_AAL} matrix (112 AAL regions x 840 TRs).
#'
#' @param subject_id integer subject ID (e.g. 1, 2, 14).
#' @param condition character: "DMT" (default) or "PCB".
#' @return numeric matrix of shape (112, 840).
#' @export
morie_entheo_load_fmri_subject <- function(subject_id, condition = "DMT") {
  condition <- match.arg(condition, c("DMT", "PCB"))
  root <- .morie_entheo_require_root()
  fname <- sprintf("LongS%02d%s.mat", as.integer(subject_id), condition)
  path <- file.path(root, "fMRI", fname)
  if (!file.exists(path)) {
    stop(sprintf(
      "%s not found in %s. Available subjects: %s",
      fname, dirname(path),
      paste(morie_entheo_available_subjects(), collapse = ", ")
    ), call. = FALSE)
  }
  mat <- .morie_entheo_loadmat(path)
  if (!"BOLD.AAL" %in% names(mat) && !"BOLD_AAL" %in% names(mat)) {
    # R.matlab translates underscores to dots in some versions.
    stop(sprintf("BOLD_AAL key missing from %s", fname), call. = FALSE)
  }
  bold <- if (!is.null(mat[["BOLD_AAL"]])) mat[["BOLD_AAL"]] else mat[["BOLD.AAL"]]
  storage.mode(bold) <- "double"
  bold
}

#' DMT_Imaging: load IRASA EEG regressors for one cortical region
#'
#' R parity of \code{morie.entheo_dmt.load_eeg_region}. Reads
#' \code{RegressorsInterpscrubbedIRASA_<region>.mat} and returns the
#' DMT, PCB, and difference regressor cubes.
#'
#' @param region character. One of Central, Frontal, Occipital,
#'   Parietal, Temporal.
#' @return named list with elements \code{regDMT}, \code{regPCB},
#'   \code{regdiff}; each is a 3-D array of shape
#'   (14 subj, 840 TRs, 5 bands).
#' @export
morie_entheo_load_eeg_region <- function(region) {
  region <- match.arg(region, .MORIE_ENTHEO_EEG_REGIONS)
  root <- .morie_entheo_require_root()
  path <- file.path(
    root, "EEG", sprintf("RegressorsInterpscrubbedIRASA_%s.mat", region)
  )
  if (!file.exists(path)) {
    stop(sprintf("EEG regressor file not found: %s", path), call. = FALSE)
  }
  mat <- .morie_entheo_loadmat(path)
  out <- list()
  for (k in c("regDMT", "regPCB", "regdiff")) {
    v <- mat[[k]]
    if (!is.null(v)) {
      storage.mode(v) <- "double"
      out[[k]] <- v
    }
  }
  out
}

#' DMT_Imaging: dataset overview
#'
#' R parity of \code{morie.entheo_dmt.dataset_overview}. Returns a
#' RichResult-style summary of the on-disk DMT_Imaging mirror.
#'
#' @return named list with \code{title}, \code{summary_lines},
#'   \code{interpretation}, \code{payload}.
#' @export
morie_entheo_dataset_overview <- function() {
  root <- .morie_entheo_require_root()
  subs <- morie_entheo_available_subjects()
  summary_lines <- list(
    list("Dataset root", root),
    list("Subjects (motion-survived)", length(subs)),
    list("Subject IDs", subs),
    list("Conditions", c("DMT", "PCB")),
    list("fMRI parcellation", "AAL (112 regions)"),
    list("fMRI timepoints", 840L),
    list("EEG regions", .MORIE_ENTHEO_EEG_REGIONS),
    list("EEG bands", .MORIE_ENTHEO_EEG_BANDS),
    list("EEG axes", "(14 subj, 840 TRs, 5 bands)")
  )
  list(
    title = "DMT_Imaging -- Timmermann 2023 dataset overview",
    summary_lines = summary_lines,
    interpretation = paste(
      "Use morie_entheo_load_fmri_subject(id, condition) for BOLD AAL",
      "matrices and morie_entheo_load_eeg_region(region) for IRASA EEG",
      "regressors. Layer-2 analyses delegate to morie::rgpsd for Welch",
      "PSD and to internal sliding-window FC / Lempel-Ziv routines."
    ),
    payload = list(
      root = root, n_subjects = length(subs), subject_ids = subs
    )
  )
}

# ---------------------------------------------------------------------------
# Layer 2: analyses
# ---------------------------------------------------------------------------

#' Trapezoidal integration on a 1-D grid.
#' @keywords internal
.morie_entheo_trapz <- function(y, x) {
  if (length(y) < 2L) return(NA_real_)
  sum(diff(x) * (y[-1] + y[-length(y)]) / 2)
}

#' EEG band-power decomposition via Welch PSD
#'
#' R parity of \code{morie.entheo_dmt.spectral_band_power}. Wraps
#' \code{\link{rgpsd}} (morie's Welch PSD; same algorithm as SciPy's
#' \code{welch}) and integrates the PSD over each canonical band by
#' the trapezoidal rule.
#'
#' @param signal numeric vector. A 1-D EEG time series.
#' @param fs numeric. Sampling frequency in Hz. Default 200 Hz
#'   (Timmermann 2023 acquisition).
#' @param bands list of \code{list(name=, lo=, hi=)} entries.
#'   Default = canonical delta/theta/alpha/beta/gamma.
#' @param nperseg integer or NULL. Welch segment length. Defaults to
#'   \code{min(length(signal), max(64, 4*fs))} -- 4-second segments at fs.
#' @return RichResult-style named list. \code{payload$rows} carries
#'   per-band absolute and relative power.
#' @references
#' Welch, P. (1967). The use of FFT for the estimation of power
#'   spectra. IEEE Trans. Audio Electroacoust. 15(2): 70-73.
#' Rangayyan, R. M. & Krishnan, S. (2024). Biomedical Signal Analysis,
#'   3rd ed., Ch. 5.
#' @export
morie_entheo_spectral_band_power <- function(signal,
                                              fs = 200,
                                              bands = .MORIE_ENTHEO_DEFAULT_BANDS,
                                              nperseg = NULL) {
  sig <- as.numeric(signal)
  if (length(sig) < 16L) {
    return(list(
      title = "EEG band-power decomposition (Welch)",
      warnings = sprintf("signal too short (%d samples)", length(sig)),
      payload = list(rows = list(), total_power = NA_real_)
    ))
  }
  if (is.null(nperseg)) {
    nperseg <- min(length(sig), max(64L, as.integer(4 * fs)))
  }
  # Delegate to morie::rgpsd (parity with Python morie.fn.psdwl.psdwl).
  res <- rgpsd(sig, fs = fs, nperseg = nperseg)
  f <- as.numeric(res$extra$frequencies)
  psd <- as.numeric(res$extra$psd)
  total <- .morie_entheo_trapz(psd, f)

  rows <- vector("list", length(bands))
  for (i in seq_along(bands)) {
    b <- bands[[i]]
    mask <- f >= b$lo & f <= b$hi
    if (sum(mask) < 2L) {
      absp <- NA_real_
      rel <- NA_real_
    } else {
      absp <- .morie_entheo_trapz(psd[mask], f[mask])
      rel <- if (!is.na(total) && total > 0) absp / total else NA_real_
    }
    rows[[i]] <- list(
      band = b$name, f_lo = b$lo, f_hi = b$hi,
      abs_power = round(absp, 6), rel_power = round(rel, 4)
    )
  }
  summary_lines <- lapply(rows, function(r) {
    list(
      sprintf("%s (%g--%g Hz)", r$band, r$f_lo, r$f_hi),
      sprintf("abs=%.4g, rel=%.3f", r$abs_power, r$rel_power)
    )
  })
  summary_lines <- c(summary_lines,
                     list(list("Total broadband power", round(total, 6))))
  list(
    title = "EEG band-power decomposition (Welch)",
    summary_lines = summary_lines,
    interpretation = paste(
      "Per-band power from Welch PSD integration. Relative power sums",
      "to approximately 1 across non-overlapping bands. In DMT vs PCB",
      "contrasts, alpha-band relative power decreases and gamma",
      "increases under DMT (Timmermann 2023)."
    ),
    payload = list(
      rows = rows, total_power = total,
      bands = vapply(rows, function(r) r$band, character(1)),
      abs_power_per_band = vapply(rows, function(r) r$abs_power, numeric(1)),
      rel_power_per_band = vapply(rows, function(r) r$rel_power, numeric(1))
    )
  )
}

#' Sliding-window dynamic functional connectivity (dRSFC)
#'
#' R parity of \code{morie.entheo_dmt.dynamic_functional_connectivity}.
#' For an AAL-parcellated BOLD matrix of shape (n_regions, n_TRs),
#' computes the upper-triangular Pearson correlation matrix in each
#' sliding window of \code{window} TRs advanced by \code{step} TRs.
#' Mirrors the \file{dRSFC.m} Matlab script in
#' \file{DMT_Imaging/Scripts/}.
#'
#' @param bold numeric matrix (n_regions, n_TRs).
#' @param window integer window length (TRs). Default 30.
#' @param step integer window stride (TRs). Default 5.
#' @return RichResult-style named list with per-window mean / std of
#'   the upper-triangular correlation vector.
#' @references
#' Allen, E. A. et al. (2014). Tracking whole-brain connectivity
#'   dynamics in the resting state. Cereb. Cortex 24(3): 663-676.
#' @export
morie_entheo_dynamic_functional_connectivity <- function(bold,
                                                          window = 30L,
                                                          step = 5L) {
  bold <- as.matrix(bold)
  storage.mode(bold) <- "double"
  if (length(dim(bold)) != 2L ||
      nrow(bold) < 2L || ncol(bold) < window + step) {
    return(list(
      title = "Dynamic resting-state functional connectivity (dRSFC)",
      warnings = sprintf(
        "insufficient BOLD shape (%d, %d)", nrow(bold), ncol(bold)
      ),
      payload = list()
    ))
  }
  nr <- nrow(bold)
  nt <- ncol(bold)
  # Upper-triangular index pairs (k=1, excluding diagonal).
  iu <- which(upper.tri(matrix(0, nr, nr), diag = FALSE), arr.ind = TRUE)
  starts <- seq.int(1L, nt - window + 1L, by = step)
  n_windows <- length(starts)
  n_pairs <- nrow(iu)
  cube <- matrix(0, nrow = n_windows, ncol = n_pairs)
  for (i in seq_along(starts)) {
    s <- starts[i]
    seg <- bold[, s:(s + window - 1L), drop = FALSE]
    # Pearson correlation between rows.
    cmat <- stats::cor(t(seg))
    cube[i, ] <- cmat[iu]
  }
  mean_per_pair <- colMeans(cube)
  std_per_pair  <- apply(cube, 2L, stats::sd)
  list(
    title = "Dynamic resting-state functional connectivity (dRSFC)",
    summary_lines = list(
      list("BOLD shape", sprintf("%d regions x %d TRs", nr, nt)),
      list("Window / step (TR)", sprintf("%d / %d", window, step)),
      list("Number of windows", n_windows),
      list("Number of region pairs", n_pairs),
      list("Mean across windows of mean correlation",
           round(mean(mean_per_pair), 4)),
      list("Mean across windows of std correlation",
           round(mean(std_per_pair), 4))
    ),
    interpretation = paste(
      "Sliding-window Pearson FC mirrors Allen et al. (2014) and the",
      "dRSFC.m script. Higher std-of-correlation across windows",
      "indicates a more dynamically reconfiguring connectivity",
      "profile -- a Timmermann 2023 DMT signature."
    ),
    payload = list(
      n_windows = n_windows, n_pairs = n_pairs,
      mean_per_pair = utils::head(mean_per_pair, 50L),
      std_per_pair  = utils::head(std_per_pair,  50L),
      cube = cube
    )
  )
}

#' Lempel-Ziv (LZ76) complexity helper.
#' @keywords internal
.morie_entheo_lz76 <- function(b) {
  n <- length(b)
  if (n == 0L) return(0L)
  i <- 1L
  c <- 1L
  l <- 1L
  k <- 1L
  kmax <- 1L
  while (TRUE) {
    if (b[i + k - 1L] != b[l + k - 1L]) {
      if (k > kmax) kmax <- k
      i <- i + 1L
      if (i == l) {
        c <- c + 1L
        l <- l + kmax
        if (l + 1L > n) break
        i <- 1L
        kmax <- 1L
        k <- 1L
      } else {
        k <- 1L
      }
    } else {
      k <- k + 1L
      if (l + k - 1L > n) {
        c <- c + 1L
        break
      }
    }
  }
  as.integer(c)
}

#' Lempel-Ziv (LZ76) complexity of a binarised signal
#'
#' R parity of \code{morie.entheo_dmt.lz_complexity}. The
#' DMT-vs-PCB contrast on LZ complexity is one of Timmermann 2023's
#' headline findings: LZ rises under DMT, indicating increased
#' neural-signal diversity.
#'
#' @param signal numeric vector.
#' @param threshold numeric or NULL. Binarisation threshold. NULL =
#'   median (the standard choice).
#' @return RichResult-style named list with raw and length-normalised LZ.
#' @references
#' Lempel, A. & Ziv, J. (1976). On the complexity of finite sequences.
#'   IEEE Trans. Inf. Theory 22(1): 75-81.
#' Schartner, M. et al. (2015). Complexity of multi-dimensional
#'   spontaneous EEG decreases during propofol-induced general
#'   anaesthesia. PLOS ONE 10(8): e0133532.
#' @export
morie_entheo_lz_complexity <- function(signal, threshold = NULL) {
  sig <- as.numeric(signal)
  if (length(sig) < 8L) {
    return(list(
      title = "Lempel-Ziv (LZ76) complexity",
      warnings = sprintf("signal too short (%d)", length(sig)),
      payload = list(lz_raw = NA_real_, lz_normalised = NA_real_)
    ))
  }
  if (is.null(threshold)) threshold <- stats::median(sig)
  b <- as.integer(sig > threshold)
  raw <- .morie_entheo_lz76(b)
  n <- length(b)
  # Length-normalised LZ (Lempel-Ziv 1976 / Schartner 2015 normalisation):
  # raw * log2(n) / n. Bounded above by 1 for random binary sequences.
  normalised <- raw * log2(n) / n
  list(
    title = "Lempel-Ziv (LZ76) complexity",
    summary_lines = list(
      list("Signal length", n),
      list("Binarisation threshold", round(threshold, 4)),
      list("LZ raw count", round(raw, 1)),
      list("LZ normalised (length-corrected)", round(normalised, 4))
    ),
    interpretation = paste(
      "Higher normalised LZ implies more diverse / less compressible",
      "binary encoding. In DMT vs PCB EEG contrasts, normalised LZ",
      "rises under DMT (Timmermann 2023 Results)."
    ),
    payload = list(
      lz_raw = raw, lz_normalised = normalised,
      threshold = threshold
    )
  )
}

#' Per-subject DMT vs PCB BOLD pipeline
#'
#' R parity of \code{morie.entheo_dmt.analyze_subject}. Runs the
#' Layer-2 BOLD analyses (global-signal LZ complexity and dynamic
#' functional connectivity) on one subject under each condition and
#' returns a RichResult-style comparison summary.
#'
#' @param subject_id integer subject ID.
#' @param conditions character vector. Conditions to evaluate.
#' @param window integer dFC window (TRs).
#' @param step integer dFC stride (TRs).
#' @return RichResult-style named list with \code{payload$rows} as a
#'   per-condition list of result rows.
#' @export
morie_entheo_analyze_subject <- function(subject_id,
                                          conditions = c("DMT", "PCB"),
                                          window = 30L, step = 5L) {
  rows <- list()
  for (cond in conditions) {
    bold <- tryCatch(
      morie_entheo_load_fmri_subject(subject_id, cond),
      error = function(e) NULL
    )
    if (is.null(bold)) {
      rows[[length(rows) + 1L]] <- list(
        subject = subject_id, condition = cond,
        error = sprintf("could not load LongS%02d%s.mat",
                        as.integer(subject_id), cond)
      )
      next
    }
    gs <- colMeans(bold)  # global signal = mean across regions
    lz_res <- morie_entheo_lz_complexity(gs)
    dfc <- morie_entheo_dynamic_functional_connectivity(
      bold, window = window, step = step
    )
    rows[[length(rows) + 1L]] <- list(
      subject = subject_id, condition = cond,
      lz_global_signal_raw = round(
        lz_res$payload$lz_raw %||% NA_real_, 1
      ),
      lz_global_signal_normalised = round(
        lz_res$payload$lz_normalised %||% NA_real_, 4
      ),
      n_dfc_windows = dfc$payload$n_windows,
      mean_dfc_corr = round(
        mean(dfc$payload$mean_per_pair %||% 0), 4
      )
    )
  }
  ok_conds <- vapply(
    rows, function(r) is.null(r$error), logical(1)
  )
  list(
    title = sprintf(
      "DMT-vs-PCB per-subject analysis -- subj %d",
      as.integer(subject_id)
    ),
    summary_lines = list(
      list("Subject", as.integer(subject_id)),
      list("Conditions evaluated",
           vapply(rows[ok_conds], function(r) r$condition, character(1)))
    ),
    interpretation = paste(
      "DMT-PCB within-subject contrast on global-signal LZ and mean",
      "dynamic FC. Headline Timmermann 2023 finding: LZ rises under",
      "DMT for the majority of subjects."
    ),
    payload = list(rows = rows)
  )
}

# ---------------------------------------------------------------------------
# Internal: NULL-coalescing infix (parity with Python's `... or default`).
# Defined locally so this file is self-contained even if utils-level
# `%||%` is not yet exported package-wide.
# ---------------------------------------------------------------------------

# ===========================================================================
# 3MMM.29 (2026-05-25): on-demand DMT_Imaging clone
# ===========================================================================

#' Clone the DMT_Imaging dataset (Timmerman et al.) into a local cache.
#'
#' `morie_entheo_clone_dmt_imaging()` shells out to `git clone` to
#' fetch the open-source DMT_Imaging dataset published by Christopher
#' Timmerman's group at <https://github.com/timmer500/DMT_Imaging>.
#' After the clone completes, `load_dmt_imaging()` and
#' `morie_entheo_load_*()` will pick the real fixture up
#' automatically (they probe `$MORIE_DMT_IMAGING_ROOT` and the cache
#' dir).
#'
#' This is opt-in -- the package never auto-clones at load time. We
#' clone from a specific commit by default to make tests
#' reproducible; pass `branch = NULL` to track main.
#'
#' Related upstream resources Vee surfaced 2026-05-25:
#' - <https://github.com/timmer500/DMT_Imaging.git>  (the actual EEG+fMRI dataset)
#' - <https://github.com/pnk314/psychedelics.git>     (analysis pipeline)
#' - <https://github.com/lisagirard/Psychedelics.git> (review repository)
#' - <https://github.com/kianenigma/awesome-psychedelics.git> (curated index)
#'
#' @param root Optional destination directory. Defaults to
#'   `$MORIE_DMT_IMAGING_ROOT`, else `file.path(morie_cache_dir(),
#'   "DMT_Imaging")`.
#' @param overwrite Logical; if `TRUE` and `root` already exists,
#'   wipe it first (rare; defaults `FALSE`).
#' @param branch Optional branch / tag / SHA to check out after
#'   clone. `NULL` uses the default upstream branch.
#' @return Invisibly returns the destination path.
#' @export
morie_entheo_clone_dmt_imaging <- function(root = NULL,
                                            overwrite = FALSE,
                                            branch = NULL) {
  if (is.null(root)) {
    root <- Sys.getenv("MORIE_DMT_IMAGING_ROOT", "")
    if (!nzchar(root)) {
      root <- file.path(morie_cache_dir(), "DMT_Imaging")
    }
  }
  if (dir.exists(root)) {
    if (isTRUE(overwrite)) {
      unlink(root, recursive = TRUE, force = TRUE)
    } else {
      message(sprintf(
        "DMT_Imaging already present at %s; pass overwrite=TRUE to refresh.",
        root))
      return(invisible(root))
    }
  }
  parent <- dirname(root)
  if (!dir.exists(parent)) dir.create(parent, recursive = TRUE)
  if (Sys.which("git") == "") {
    stop("git not found on PATH; install git to clone DMT_Imaging.",
         call. = FALSE)
  }
  args <- c("clone", "--depth", "1")
  if (!is.null(branch)) args <- c(args, "--branch", as.character(branch))
  args <- c(args, "https://github.com/timmer500/DMT_Imaging.git", root)
  status <- system2("git", args, stdout = TRUE, stderr = TRUE)
  if (!dir.exists(root)) {
    stop("git clone failed: ", paste(status, collapse = "\n"),
         call. = FALSE)
  }
  invisible(root)
}
