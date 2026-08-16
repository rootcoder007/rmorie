# Rangayyan time-frequency and multiresolution analysis -- the R mirror of
# the Python bsatf module: STFT and its inverse, Wigner-Ville and Cohen's
# class, the CWT/scalogram, the orthogonal and biorthogonal wavelet banks,
# EMD/EEMD/VMD, the Hilbert-Huang spectrum, and the Chapter 4.8 echo and
# cepstrum family.
#
# Internal: no roxygen, no @export, no NAMESPACE entry.
#
# Index convention.  Python indexes from 0, R from 1.  Every lag, shift,
# scale and bin below has been translated deliberately.  Payload fields
# that name a POSITION IN A RETURNED VECTOR (ridge, s1_index, s2_index,
# dominant_leaf, dominant_imf, triggers, r_peaks, structures$sample) are
# 1-based here, as an R caller expects.  Payload fields that name the
# BOOK'S OWN sample-index variable n -- the echo family's n, echo_delay,
# impulses, and the matching-pursuit translation -- stay 0-based, because
# there they are a mathematical argument and not an index into anything.

# --- coercion --------------------------------------------------------------

.tf_need <- function(x, name = "x", minlen = 2L) {
  v <- as.numeric(x)
  if (length(v) < minlen) {
    stop(sprintf(
      "%s must have at least %d samples, got %d",
      name, as.integer(minlen), length(v)
    ))
  }
  if (any(!is.finite(v))) {
    stop(sprintf("%s contains a non-finite sample", name))
  }
  v
}

# --- direct DFT / IDFT (O(N^2), not an FFT -- the Python arm is the same) ---

.tf_dft <- function(x) {
  z <- as.complex(x)
  n <- length(z)
  a <- Re(z)
  b <- Im(z)
  i0 <- seq_len(n) - 1
  vapply(i0, function(k) {
    w <- -2 * pi * k / n
    cs <- cos(w * i0)
    sn <- sin(w * i0)
    complex(
      real = .morie_fsum(a * cs - b * sn),
      imaginary = .morie_fsum(a * sn + b * cs)
    )
  }, complex(1))
}

.tf_idft <- function(X) {
  z <- as.complex(X)
  n <- length(z)
  a <- Re(z)
  b <- Im(z)
  k0 <- seq_len(n) - 1
  vapply(k0, function(i) {
    w <- 2 * pi * i / n
    cs <- cos(w * k0)
    sn <- sin(w * k0)
    complex(
      real = .morie_fsum(a * cs - b * sn) / n,
      imaginary = .morie_fsum(b * cs + a * sn) / n
    )
  }, complex(1))
}

# --- analysis windows (eq 8.7 plus the usual raised cosines) ---------------

.tf_win <- function(name, m) {
  m <- as.integer(m)
  if (m < 1L) stop("window length must be >= 1")
  nm <- tolower(as.character(name))
  if (nm %in% c("rect", "rectangular", "boxcar", "none")) {
    return(rep(1, m))
  }
  if (m == 1L) {
    return(1)
  }
  i <- seq_len(m) - 1
  if (nm %in% c("hann", "hanning")) {
    return(0.5 - 0.5 * cos(2 * pi * i / (m - 1)))
  }
  if (nm == "hamming") {
    return(0.54 - 0.46 * cos(2 * pi * i / (m - 1)))
  }
  if (nm %in% c("bartlett", "triang")) {
    return(1 - abs((i - (m - 1) / 2) / ((m - 1) / 2)))
  }
  stop(sprintf("unknown window '%s'; use rect, hann, hamming or bartlett", nm))
}

# --- analytic signal, Sec 5.5.3 -------------------------------------------

.tf_analytic <- function(x) {
  v <- as.numeric(x)
  n <- length(v)
  X <- .tf_dft(v)
  h <- numeric(n)
  h[1L] <- 1
  if (n %% 2L == 0L) {
    h[n %/% 2L + 1L] <- 1
    if (n %/% 2L >= 2L) h[2L:(n %/% 2L)] <- 2
  } else {
    if ((n + 1L) %/% 2L >= 2L) h[2L:((n + 1L) %/% 2L)] <- 2
  }
  .tf_idft(X * h)
}

# --- Daubechies scaling filters, Ten Lectures on Wavelets Table 6.1 --------
# Copied verbatim from the Python arm; not re-derived.

.TF_DBTAPS <- list(
  `1` = c(0.7071067811865476, 0.7071067811865476),
  `2` = c(
    0.48296291314469025, 0.836516303737469, 0.22414386804185735,
    -0.12940952255092145
  ),
  `3` = c(
    0.3326705529509569, 0.8068915093133388, 0.4598775021193313,
    -0.13501102001039084, -0.08544127388224149, 0.035226291882100656
  ),
  `4` = c(
    0.23037781330885523, 0.7148465705525415, 0.6308807679295904,
    -0.02798376941698385, -0.18703481171888114, 0.030841381835986965,
    0.032883011666982945, -0.010597401784997278
  ),
  `5` = c(
    0.160102397974125, 0.6038292697974729, 0.7243085284377726,
    0.13842814590110342, -0.24229488706619015, -0.03224486958502952,
    0.07757149384006515, -0.006241490213011705, -0.012580751999015526,
    0.003335725285001549
  ),
  `6` = c(
    0.11154074335008017, 0.4946238903983854, 0.7511339080215775,
    0.3152503517092432, -0.22626469396516913, -0.12976686756709563,
    0.09750160558707936, 0.02752286553001629, -0.031582039318031156,
    0.0005538422009938016, 0.004777257511010651, -0.00107730108499558
  ),
  `7` = c(
    0.07785205408506236, 0.39653931948230575, 0.7291320908465551,
    0.4697822874053586, -0.14390600392910627, -0.22403618499416572,
    0.07130921926705004, 0.0806126091510659, -0.03802993693503463,
    -0.01657454163101562, 0.012550998556013784, 0.00042957797300470274,
    -0.0018016407039998328, 0.0003537138000010399
  ),
  `8` = c(
    0.05441584224308161, 0.3128715909144659, 0.6756307362980128,
    0.5853546836548691, -0.015829105256023893, -0.2840155429624281,
    0.00047248457399797254, 0.128747426620186, -0.017369301002022108,
    -0.04408825393106472, 0.013981027917015516, 0.008746094047015655,
    -0.004870352993451574, -0.000391740373376471, 0.0006754494059985568,
    -0.00011747678400228192
  ),
  `9` = c(
    0.03807794736316728, 0.24383467463766728, 0.6048231236767786,
    0.6572880780366389, 0.13319738582208895, -0.29327378327258685,
    -0.09684078322087904, 0.14854074933476008, 0.030725681478322865,
    -0.06763282905952399, 0.000250947114834164, 0.022361662123515244,
    -0.004723204757894831, -0.004281503681904723, 0.0018476468829611268,
    0.00023038576399541288, -0.0002519631889981789, 3.934732031627159e-05
  ),
  `10` = c(
    0.026670057900950818, 0.18817680007762133, 0.5272011889309198,
    0.6884590394525921, 0.2811723436604265, -0.24984642432648865,
    -0.19594627437659665, 0.12736934033574265, 0.09305736460380659,
    -0.07139414716586077, -0.02945753682194567, 0.03321267405893324,
    0.0036065535669883944, -0.010733175482979604, 0.0013953517469940798,
    0.00199240529499085, -0.0006858566950046825, -0.0001164668549943862,
    9.358867000108985e-05, -1.326420300235487e-05
  )
)

.tf_dbname <- function(wavelet) {
  w <- gsub("[-_]", "", tolower(trimws(as.character(wavelet))))
  if (w %in% c("haar", "db1", "d2")) {
    return(1L)
  }
  if (grepl("^db[0-9]+$", w)) {
    k <- as.integer(substring(w, 3L))
    if (!is.null(.TF_DBTAPS[[as.character(k)]])) {
      return(k)
    }
  }
  if (grepl("^d[0-9]+$", w)) {
    k <- as.integer(substring(w, 2L))
    if (k %% 2L == 0L && !is.null(.TF_DBTAPS[[as.character(k %/% 2L)]])) {
      return(k %/% 2L)
    }
  }
  stop(sprintf(
    "unknown wavelet '%s'; use 'haar' or 'db1'..'db10'",
    as.character(wavelet)
  ))
}

.tf_filters <- function(wavelet) {
  h <- .TF_DBTAPS[[as.character(.tf_dbname(wavelet))]]
  L <- length(h)
  if (abs(.morie_fsum(h * h) - 1) > 1e-9) {
    stop(sprintf("scaling filter for '%s' is not unit-norm", as.character(wavelet)))
  }
  if (L %/% 2L >= 2L) {
    for (m in seq_len(L %/% 2L - 1L)) {
      if (abs(.morie_fsum(h[seq_len(L - 2L * m)] *
        h[seq_len(L - 2L * m) + 2L * m])) > 1e-9) {
        stop(sprintf(
          "scaling filter for '%s' is not orthogonal",
          as.character(wavelet)
        ))
      }
    }
  }
  g <- ((-1)^(seq_len(L) - 1)) * rev(h)
  list(h = h, g = g, rec_lo = h, rec_hi = g)
}

# --- decimated filter bank, periodic extension -----------------------------

.tf_dwtstep <- function(a, h, g) {
  a <- as.numeric(a)
  n <- length(a)
  if (n %% 2L == 1L) {
    a <- c(a, a[n])
    n <- n + 1L
  }
  half <- n %/% 2L
  j0 <- seq_along(h) - 1
  lo <- numeric(half)
  hi <- numeric(half)
  for (k in seq_len(half)) {
    v <- a[((2 * (k - 1) + j0) %% n) + 1L]
    lo[k] <- .morie_fsum(h * v)
    hi[k] <- .morie_fsum(g * v)
  }
  list(lo = lo, hi = hi)
}

.tf_idwtstep <- function(lo, hi, h, g) {
  half <- length(lo)
  n <- 2L * half
  L <- length(h)
  out <- numeric(n)
  for (k in seq_len(half)) {
    for (j in seq_len(L)) {
      m <- ((2 * (k - 1) + (j - 1)) %% n) + 1L
      out[m] <- out[m] + lo[k] * h[j] + hi[k] * g[j]
    }
  }
  out
}

.tf_dwt <- function(x, wavelet, levels) {
  f <- .tf_filters(wavelet)
  a <- as.numeric(x)
  levels <- as.integer(levels)
  if (levels < 1L) stop("levels must be >= 1")
  maxlev <- 0L
  m <- length(a)
  while (m >= length(f$h) && m >= 2L) {
    maxlev <- maxlev + 1L
    m <- (m + 1L) %/% 2L
  }
  if (levels > maxlev) {
    stop(sprintf(
      paste0(
        "levels=%d exceeds the maximum %d for a signal of ",
        "length %d with filter length %d"
      ),
      levels, maxlev, length(x), length(f$h)
    ))
  }
  details <- vector("list", levels)
  lengths <- integer(levels)
  for (i in seq_len(levels)) {
    lengths[i] <- length(a)
    st <- .tf_dwtstep(a, f$h, f$g)
    a <- st$lo
    details[[i]] <- st$hi
  }
  list(approx = a, details = rev(details), lengths = rev(lengths))
}

.tf_idwt <- function(a, details, lengths, wavelet) {
  f <- .tf_filters(wavelet)
  cur <- as.numeric(a)
  for (i in seq_along(details)) {
    cur <- .tf_idwtstep(cur, details[[i]], f$h, f$g)
    cur <- cur[seq_len(lengths[i])]
  }
  cur
}

# --- undecimated (a-trous) transform, Nason & Silverman (1995) -------------

.tf_swt <- function(x, wavelet, levels) {
  f <- .tf_filters(wavelet)
  n <- length(x)
  a <- as.numeric(x)
  levels <- as.integer(levels)
  details <- vector("list", levels)
  approxes <- vector("list", levels)
  j0 <- seq_along(f$h) - 1
  for (lev in seq_len(levels)) {
    step <- 2^(lev - 1)
    lo <- numeric(n)
    hi <- numeric(n)
    for (i in seq_len(n)) {
      v <- a[(((i - 1) + j0 * step) %% n) + 1L]
      lo[i] <- .morie_fsum(f$h * v)
      hi[i] <- .morie_fsum(f$g * v)
    }
    details[[lev]] <- hi
    approxes[[lev]] <- lo
    a <- lo
  }
  list(approx = a, details = details, approxes = approxes)
}

# --- natural cubic spline (EMD envelopes, Sec 9.4 step 2) ------------------

.tf_spline <- function(xs, ys, xq) {
  xs <- as.numeric(xs)
  ys <- as.numeric(ys)
  xq <- as.numeric(xq)
  n <- length(xs)
  if (n < 2L) stop("need at least two knots for a spline")
  if (n == 2L) {
    s <- (ys[2] - ys[1]) / (xs[2] - xs[1])
    return(ys[1] + s * (xq - xs[1]))
  }
  hh <- diff(xs)
  alpha <- numeric(n)
  for (i in 2L:(n - 1L)) {
    alpha[i] <- 3 * ((ys[i + 1] - ys[i]) / hh[i] - (ys[i] - ys[i - 1]) / hh[i - 1])
  }
  l <- numeric(n)
  l[1] <- 1
  mu <- numeric(n)
  z <- numeric(n)
  for (i in 2L:(n - 1L)) {
    l[i] <- 2 * (xs[i + 1] - xs[i - 1]) - hh[i - 1] * mu[i - 1]
    mu[i] <- hh[i] / l[i]
    z[i] <- (alpha[i] - hh[i - 1] * z[i - 1]) / l[i]
  }
  l[n] <- 1
  cc <- numeric(n)
  b <- numeric(n - 1L)
  d <- numeric(n - 1L)
  for (j in (n - 1L):1L) {
    cc[j] <- z[j] - mu[j] * cc[j + 1]
    b[j] <- (ys[j + 1] - ys[j]) / hh[j] - hh[j] * (cc[j + 1] + 2 * cc[j]) / 3
    d[j] <- (cc[j + 1] - cc[j]) / (3 * hh[j])
  }
  # Python picks j = 0 for t <= xs[0], n-2 for t >= xs[-1], else the largest
  # knot at or below t.  findInterval gives that directly, once clamped.
  jj <- pmin(pmax(findInterval(xq, xs), 1L), n - 1L)
  dt <- xq - xs[jj]
  ys[jj] + b[jj] * dt + cc[jj] * dt * dt + d[jj] * dt * dt * dt
}

# --- extrema / zero crossings ---------------------------------------------

.tf_extrema <- function(x) {
  n <- length(x)
  mx <- integer(0)
  mn <- integer(0)
  i <- 2L
  while (i <= n - 1L) {
    if (x[i] > x[i - 1L]) {
      j <- i
      while (j <= n - 1L && x[j + 1L] == x[i]) j <- j + 1L
      if (j <= n - 1L && x[j] > x[j + 1L]) mx <- c(mx, (i + j) %/% 2L)
      i <- j + 1L
      next
    }
    if (x[i] < x[i - 1L]) {
      j <- i
      while (j <= n - 1L && x[j + 1L] == x[i]) j <- j + 1L
      if (j <= n - 1L && x[j] < x[j + 1L]) mn <- c(mn, (i + j) %/% 2L)
      i <- j + 1L
      next
    }
    i <- i + 1L
  }
  list(mx = mx, mn = mn)
}

.tf_zerox <- function(x) {
  n <- length(x)
  if (n < 2L) {
    return(0L)
  }
  a <- x[seq_len(n - 1L)]
  b <- x[-1L]
  sum((a < 0 & b >= 0) | (a > 0 & b <= 0))
}

# --- sifting / EMD, Sec 9.4 steps 1-6 --------------------------------------

.tf_sift <- function(x, maxiter = 50L, tol = 0.05) {
  h <- as.numeric(x)
  n <- length(h)
  tt <- seq_len(n)
  maxiter <- as.integer(maxiter)
  for (it in seq_len(maxiter)) {
    ex <- .tf_extrema(h)
    if (length(ex$mx) < 2L || length(ex$mn) < 2L) {
      return(list(imf = h, iterations = it, converged = TRUE))
    }
    up <- .tf_spline(c(1L, ex$mx, n), c(h[1L], h[ex$mx], h[n]), tt)
    lo <- .tf_spline(c(1L, ex$mn, n), c(h[1L], h[ex$mn], h[n]), tt)
    mean_env <- (up + lo) / 2
    newh <- h - mean_env
    den <- .morie_fsum(h * h)
    sd <- if (den > 0) .morie_fsum((newh - h)^2) / den else 0
    h <- newh
    if (sd < tol) {
      return(list(imf = h, iterations = it, converged = TRUE))
    }
  }
  list(imf = h, iterations = maxiter, converged = FALSE)
}

.tf_emd <- function(x, maxmodes = 10L, tol = 0.05) {
  res <- as.numeric(x)
  imfs <- list()
  for (k in seq_len(as.integer(maxmodes))) {
    ex <- .tf_extrema(res)
    if (length(ex$mx) + length(ex$mn) < 3L) break
    s <- .tf_sift(res, 50L, tol)
    imfs[[length(imfs) + 1L]] <- s$imf
    res <- res - s$imf
  }
  list(imfs = imfs, residual = res)
}

# --- mother wavelets, eqs (8.115)/(8.116) ---------------------------------

.tf_mother <- function(name, t, w0 = 5) {
  nm <- tolower(trimws(as.character(name)))
  if (nm %in% c("mexh", "mexicanhat", "sombrero", "ricker")) {
    return(as.complex((1 - t * t) * exp(-0.5 * t * t)))
  }
  if (nm == "morlet") {
    env <- exp(-0.5 * t * t) / (pi^0.25)
    return((exp(complex(imaginary = w0 * t)) - exp(-0.5 * w0 * w0)) * env)
  }
  if (nm %in% c("haar", "db1")) {
    out <- rep(as.complex(0), length(t))
    out[t >= 0 & t < 0.5] <- as.complex(1)
    out[t >= 0.5 & t < 1] <- as.complex(-1)
    return(out)
  }
  stop(sprintf("unknown wavelet '%s'; use 'morlet', 'mexh' or 'haar'", nm))
}

.tf_support <- function(name) {
  nm <- tolower(trimws(as.character(name)))
  v <- c(
    morlet = 4, mexh = 5, mexicanhat = 5, sombrero = 5, ricker = 5,
    haar = 1, db1 = 1
  )[[nm]]
  if (is.null(v)) stop(sprintf("unknown wavelet '%s'", nm))
  v
}

.tf_cwt <- function(x, scales, wavelet = "morlet", w0 = 5) {
  v <- as.numeric(x)
  n <- length(v)
  rad <- .tf_support(wavelet)
  out <- vector("list", length(scales))
  for (si in seq_along(scales)) {
    s <- scales[si]
    if (s <= 0) stop("scales must be positive")
    half <- as.integer(trunc(rad * s)) + 1L
    row <- complex(length.out = n)
    for (tau in seq_len(n)) {
      lo <- max(1L, tau - half)
      hi <- min(n, tau + half)
      idx <- lo:hi
      row[tau] <- sum(v[idx] * Conj(.tf_mother(wavelet, (idx - tau) / s, w0))) / sqrt(s)
    }
    out[[si]] <- row
  }
  out
}

# --- Wigner-Ville, eq (8.123), Claasen-Mecklenbrauker form ----------------

.tf_wvd <- function(x, fs, nfreq = NULL) {
  v <- as.numeric(x)
  n <- length(v)
  nf <- as.integer(if (is.null(nfreq) || nfreq == 0) n else nfreq)
  z <- .tf_analytic(v)
  freqs <- (seq_len(nf) - 1) * fs / (2 * nf)
  tfd <- matrix(0, nrow = n, ncol = nf)
  for (i in seq_len(n)) {
    m <- min(i - 1L, n - i)
    taus <- (-m):m
    ker <- z[i + taus] * Conj(z[i - taus])
    for (k in seq_len(nf)) {
      w <- -2 * pi * (k - 1) / nf
      tfd[i, k] <- 2 * Re(sum(ker * exp(complex(imaginary = w * taus))))
    }
  }
  list(tfd = tfd, freqs = freqs)
}

.tf_smooth2d <- function(tfd, tlen, flen) {
  nt <- nrow(tfd)
  nf <- ncol(tfd)
  gauss <- function(L) {
    L <- as.integer(L)
    if (L <= 1L) {
      return(1)
    }
    sig <- L / 6
    w <- exp(-0.5 * (((seq_len(L) - 1) - (L - 1) / 2) / sig)^2)
    w / .morie_fsum(w)
  }
  g <- gauss(tlen)
  H <- gauss(flen)
  ht <- (length(g) - 1L) %/% 2L
  hf <- (length(H) - 1L) %/% 2L
  tmp <- matrix(0, nt, nf)
  for (i in seq_len(nt)) {
    idx <- pmin(nt, pmax(1L, i + (seq_along(g) - 1L) - ht))
    for (k in seq_len(nf)) tmp[i, k] <- .morie_fsum(g * tfd[idx, k])
  }
  out <- matrix(0, nt, nf)
  for (k in seq_len(nf)) {
    idx <- pmin(nf, pmax(1L, k + (seq_along(H) - 1L) - hf))
    for (i in seq_len(nt)) out[i, k] <- .morie_fsum(H * tmp[i, idx])
  }
  out
}

.tf_energy <- function(v) .morie_fsum(Mod(v)^2)

# --- deterministic 64-bit LCG for the EEMD noise ---------------------------
# R has no 64-bit integer, so the state is carried as four little-endian
# 16-bit limbs and multiply-add is done exactly in doubles.  Same stream as
# the Python arm's `state = (state * 6364136223846793005 + ...) mod 2^64`.

.TF_LCG_M <- c(0x7F2D, 0x4C95, 0xF42D, 0x5851)
.TF_LCG_C <- c(0x814F, 0xF767, 0x7B7E, 0x1405)

.tf_lcg_step <- function(st) {
  r <- c(0, 0, 0, 0)
  for (i in 1:4) {
    for (j in 1:(5 - i)) {
      k <- i + j - 1L
      r[k] <- r[k] + st[i] * .TF_LCG_M[j]
    }
  }
  r <- r + .TF_LCG_C
  carry <- 0
  for (k in 1:4) {
    t <- r[k] + carry
    carry <- floor(t / 65536)
    r[k] <- t - carry * 65536
  }
  r
}

.tf_lcg_unif <- function(st) {
  # (state >> 11) is exact in a double: it is at most 2^53 - 1.
  (floor(st[1] / 2048) + st[2] * 2^5 + st[3] * 2^21 + st[4] * 2^37 + 1) /
    (2^53 + 1)
}

.tf_lcg_seed <- function(seed) .tf_lcg_step(c(0, 0, 0, 0) + .tf_seed_limbs(seed))

.tf_seed_limbs <- function(seed) {
  s <- as.numeric(seed)
  if (s < 0) stop("seed must be non-negative")
  c(
    s %% 65536, floor(s / 65536) %% 65536,
    floor(s / 2^32) %% 65536, floor(s / 2^48) %% 65536
  )
}

# ===========================================================================
#  Public (internal-to-the-package) surface: the 45 mirrored functions.
# ===========================================================================

# -- Complex demodulation, Sec 5.5.1 eqs (5.16)-(5.19).
#' Complex demodulation, Sec 5.5.1 eqs (5.16)-(5.19)
#'
#' Part of the rangayyan_tf implementation; see the file header for the
#' source it follows.
#'
#' @param x See Usage.
#' @param fs Defaults to \code{1}.
#' @param f0 Defaults to \code{NULL}.
#' @param bandwidth Defaults to \code{NULL}.
#' @return A list with \code{amplitude}, \code{phase}, \code{demodulated}, \code{f0}, \code{bandwidth}, \code{mean_amplitude}, \code{method}.
#' @export
CDemod <- function(x, fs = 1, f0 = NULL, bandwidth = NULL) {
  v <- .tf_need(x, "x", 4L)
  fs <- as.numeric(fs)
  if (fs <= 0) stop("fs must be positive")
  n <- length(v)
  X <- .tf_dft(v)
  if (is.null(f0)) {
    half <- n %/% 2L + 1L
    kbest <- if (half > 1L) {
      k0 <- seq_len(half - 1L) # Python range(1, half), 0-based bins
      k0[which.max(Mod(X[k0 + 1L]))]
    } else {
      0L
    }
    f0 <- kbest * fs / n
  }
  f0 <- as.numeric(f0)
  if (f0 <= 0) stop("f0 must be positive")
  if (f0 >= fs / 2) {
    stop(sprintf("f0=%s must be below the Nyquist frequency %s", f0, fs / 2))
  }
  bw <- if (is.null(bandwidth)) fs / 16 else as.numeric(bandwidth)
  if (bw <= 0) stop("bandwidth must be positive")
  if (bw >= f0) {
    stop(sprintf(paste0(
      "bandwidth=%s Hz is not smaller than f0=%s Hz; the ",
      "image at 2*f0 (eq 5.18) would leak through the ",
      "lowpass filter"
    ), bw, f0))
  }
  i0 <- seq_len(n) - 1
  y <- 2 * v * exp(complex(imaginary = -2 * pi * f0 * i0 / fs))
  Y <- .tf_dft(y)
  kc <- max(1L, as.integer(trunc(bw * n / fs)))
  k0 <- i0
  kk <- ifelse(k0 <= n %/% 2L, k0, k0 - n)
  Z <- complex(length.out = n)
  keep <- abs(kk) <= kc
  Z[keep] <- Y[keep]
  y0 <- .tf_idft(Z)
  amp <- Mod(y0)
  ph <- atan2(Im(y0), Re(y0))
  unw <- numeric(n)
  unw[1L] <- ph[1L]
  for (i in 2L:n) {
    d <- ph[i] - ph[i - 1L]
    while (d > pi) d <- d - 2 * pi
    while (d < -pi) d <- d + 2 * pi
    unw[i] <- unw[i - 1L] + d
  }
  list(
    amplitude = amp, phase = unw, demodulated = y0, f0 = f0,
    bandwidth = bw, mean_amplitude = .morie_fsum(amp) / n,
    method = paste(
      "Complex demodulation, Rangayyan & Krishnan (2024)",
      "Sec 5.5.1 eqs (5.16)-(5.19)"
    )
  )
}

# -- Biorthogonal 5/3 (CDF) transform via lifting.
#' Biorthogonal 5/3 (CDF) transform via lifting
#'
#' Part of the rangayyan_tf implementation; see the file header for the
#' source it follows.
#'
#' @param x See Usage.
#' @param wavelet Defaults to \code{"bior2.2"}.
#' @param levels Defaults to \code{3}.
#' @return A list with \code{approx}, \code{details}, \code{coeffs}, \code{lengths}, \code{levels}, \code{reconstructed}, \code{max_reconstruction_error}, \code{symmetric}, \code{wavelet}, \code{method}.
#' @export
BiorDwt <- function(x, wavelet = "bior2.2", levels = 3) {
  v <- .tf_need(x, "x", 4L)
  w <- gsub("[-_]", "", tolower(trimws(as.character(wavelet))))
  if (!(w %in% c("bior2.2", "bior22", "5/3", "53", "cdf53", "legall"))) {
    stop(sprintf(paste0(
      "unsupported biorthogonal wavelet '%s'; only the 5/3 ",
      "(bior2.2) pair is implemented"
    ), as.character(wavelet)))
  }
  lv <- as.integer(levels)
  if (lv < 1L) stop("levels must be >= 1")

  fwd <- function(a) {
    n <- length(a)
    if (n < 2L) stop("signal too short for another level")
    if (n %% 2L == 1L) {
      a <- c(a, a[n])
      n <- n + 1L
    }
    half <- n %/% 2L
    s <- a[2L * seq_len(half) - 1L]
    d <- a[2L * seq_len(half)]
    k <- seq_len(half)
    d <- d - 0.5 * (s[k] + s[pmin(k + 1L, half)])
    s <- s + 0.25 * (d[pmax(k - 1L, 1L)] + d[k])
    list(s = s, d = d)
  }
  inv <- function(s, d, n) {
    k <- seq_along(s)
    s <- s - 0.25 * (d[pmax(k - 1L, 1L)] + d[k])
    d <- d + 0.5 * (s[k] + s[pmin(k + 1L, length(s))])
    out <- numeric(2L * length(s))
    out[2L * k - 1L] <- s
    out[2L * k] <- d
    out[seq_len(n)]
  }

  a <- v
  details <- vector("list", lv)
  lengths <- integer(lv)
  for (i in seq_len(lv)) {
    if (length(a) < 2L) {
      stop(sprintf("levels=%d is too many for a signal of length %d", lv, length(v)))
    }
    lengths[i] <- length(a)
    st <- fwd(a)
    a <- st$s
    details[[i]] <- st$d
  }
  details <- rev(details)
  lengths <- rev(lengths)
  rec <- a
  for (i in seq_along(details)) rec <- inv(rec, details[[i]], lengths[i])
  err <- max(abs(rec - v))
  list(
    approx = a, details = details, coeffs = c(list(a), details),
    lengths = lengths, levels = lv, reconstructed = rec,
    max_reconstruction_error = err, symmetric = TRUE,
    wavelet = "bior2.2 (CDF 5/3)",
    method = paste(
      "Biorthogonal 5/3 (CDF) wavelet transform via lifting;",
      "Cohen, Daubechies & Feauveau (1992) and Daubechies &",
      "Sweldens (1998) -- not defined in Rangayyan & Krishnan"
    )
  )
}

# -- Exponential-kernel (Choi-Williams) TFD.
#' Exponential-kernel (Choi-Williams) TFD
#'
#' Part of the rangayyan_tf implementation; see the file header for the
#' source it follows.
#'
#' @param x See Usage.
#' @param fs Defaults to \code{1}.
#' @param sigma Defaults to \code{1}.
#' @param nfreq Defaults to \code{NULL}.
#' @param maxlag Defaults to \code{NULL}.
#' @return A list with \code{tfd}, \code{times}, \code{freqs}, \code{sigma}, \code{maxlag}, \code{peak_freq}, \code{crossterm_ratio}, \code{method}.
#' @export
ExpKerTfd <- function(x, fs = 1, sigma = 1, nfreq = NULL, maxlag = NULL) {
  v <- .tf_need(x, "x", 8L)
  fs <- as.numeric(fs)
  sigma <- as.numeric(sigma)
  if (fs <= 0) stop("fs must be positive")
  if (sigma <= 0) stop("sigma must be positive")
  n <- length(v)
  nf <- as.integer(if (is.null(nfreq) || nfreq == 0) n else nfreq)
  ml <- if (is.null(maxlag)) max(1L, n %/% 4L) else as.integer(maxlag)
  if (ml < 1L) stop("maxlag must be >= 1")
  z <- .tf_analytic(v)
  freqs <- (seq_len(nf) - 1) * fs / (2 * nf)
  tfd <- matrix(0, n, nf)
  for (i in seq_len(n)) {
    kv <- complex(length.out = 0)
    lv <- numeric(0)
    for (tau in (-ml):ml) {
      if (i + tau < 1L || i - tau < 1L || i + tau > n || i - tau > n) next
      if (tau == 0L) {
        kv <- c(kv, z[i] * Conj(z[i]))
        lv <- c(lv, 0)
        next
      }
      sdv <- 2 * abs(tau) / sqrt(2 * sigma)
      span <- max(1L, as.integer(trunc(3 * sdv)))
      acc <- as.complex(0)
      wsum <- 0
      for (mu in (-span):span) {
        a <- i + mu + tau
        b <- i + mu - tau
        if (a < 1L || b < 1L || a > n || b > n) next
        wgt <- exp(-sigma * mu * mu / (4 * tau * tau))
        acc <- acc + wgt * z[a] * Conj(z[b])
        wsum <- wsum + wgt
      }
      if (wsum <= 0) next
      kv <- c(kv, acc / wsum)
      lv <- c(lv, tau)
    }
    for (k in seq_len(nf)) {
      w <- -2 * pi * (k - 1) / nf
      tfd[i, k] <- 2 * Re(sum(kv * exp(complex(imaginary = w * lv))))
    }
  }
  flat <- as.vector(t(tfd))
  tot <- .morie_fsum(abs(flat))
  neg <- .morie_fsum(-flat[flat < 0])
  col <- vapply(seq_len(nf), function(k) .morie_fsum(tfd[, k]), numeric(1))
  list(
    tfd = tfd, times = (seq_len(n) - 1) / fs, freqs = freqs, sigma = sigma,
    maxlag = ml, peak_freq = freqs[which.max(col)],
    crossterm_ratio = if (tot > 0) neg / tot else 0,
    method = paste(
      "Exponential-kernel (Choi-Williams) TFD, Choi & Williams",
      "(1989) IEEE TASSP 37(6):862-871; a member of Cohen's",
      "class, Rangayyan & Krishnan (2024) eq (8.124)"
    )
  )
}

# -- Wavelet scale distribution width of a fibrillation waveform, Sec 8.15.
#' Wavelet scale distribution width of a fibrillation waveform, Sec 8.15
#'
#' Part of the rangayyan_tf implementation; see the file header for the
#' source it follows.
#'
#' @param ecg See Usage.
#' @param fs Defaults to \code{250}.
#' @param scales Defaults to \code{NULL}.
#' @param w0 Defaults to \code{5}.
#' @param band Defaults to \code{c(3, 21)}.
#' @return A list with \code{sdw}, \code{scale_energy}, \code{scales}, \code{freqs}, \code{peak_scale}, \code{peak_freq}, \code{organised}, \code{band}, \code{method}.
#' @export
CprWt <- function(ecg, fs = 250, scales = NULL, w0 = 5, band = c(3, 21)) {
  v <- .tf_need(ecg, "ecg", 16L)
  fs <- as.numeric(fs)
  if (fs <= 0) stop("fs must be positive")
  lo <- as.numeric(band[1L])
  hi <- as.numeric(band[2L])
  if (!(0 < lo && lo < hi)) {
    stop(sprintf("band must satisfy 0 < low < high, got (%s, %s)", lo, hi))
  }
  if (hi > fs / 2) {
    stop(sprintf("band upper edge %s Hz exceeds Nyquist %s Hz", hi, fs / 2))
  }
  n <- length(v)
  X <- .tf_dft(v)
  k0 <- seq_len(n) - 1
  f <- ifelse(k0 <= n %/% 2L, k0, k0 - n) * fs / n
  Y <- complex(length.out = n)
  keep <- abs(f) >= lo & abs(f) <= hi
  Y[keep] <- X[keep]
  filt <- Re(.tf_idft(Y))
  if (is.null(scales)) {
    sc <- numeric(0)
    s <- max(1, w0 * fs / (2 * pi * hi))
    top <- w0 * fs / (2 * pi * lo)
    while (s <= top && length(sc) < 32L) {
      sc <- c(sc, s)
      s <- s * 2^0.25
    }
    scales <- if (length(sc)) sc else 1
  }
  sc <- as.numeric(scales)
  co <- .tf_cwt(filt, sc, "morlet", as.numeric(w0))
  ener <- vapply(co, function(r) .morie_fsum(Mod(r)^2), numeric(1))
  tot <- .morie_fsum(ener)
  if (tot <= 0) stop("no energy in the 3-21 Hz band; SDW is undefined")
  norm <- ener / tot
  pk <- which.max(norm)
  halfmax <- norm[pk] / 2
  lo_i <- pk
  while (lo_i > 1L && norm[lo_i - 1L] >= halfmax) lo_i <- lo_i - 1L
  hi_i <- pk
  while (hi_i < length(norm) && norm[hi_i + 1L] >= halfmax) hi_i <- hi_i + 1L
  # Widths are counted in ladder STEPS, so the 1-based positions cancel.
  left <- as.numeric(lo_i)
  if (lo_i > 1L && norm[lo_i] > norm[lo_i - 1L]) {
    left <- lo_i - (norm[lo_i] - halfmax) / (norm[lo_i] - norm[lo_i - 1L])
  }
  right <- as.numeric(hi_i)
  if (hi_i < length(norm) && norm[hi_i] > norm[hi_i + 1L]) {
    right <- hi_i + (norm[hi_i] - halfmax) / (norm[hi_i] - norm[hi_i + 1L])
  }
  sdw <- right - left
  list(
    sdw = sdw, scale_energy = norm, scales = sc,
    freqs = w0 * fs / (2 * pi * sc), peak_scale = sc[pk],
    peak_freq = w0 * fs / (2 * pi * sc[pk]),
    organised = sdw < length(sc) / 2, band = c(lo, hi),
    method = paste(
      "Wavelet scale distribution width (SDW) of a",
      "fibrillation waveform, Rangayyan & Krishnan (2024)",
      "Sec 8.15, Morlet CWT of eqs (8.107)/(8.116), 3-21 Hz",
      "band, FWHM of the normalised scale-energy distribution"
    )
  )
}

# -- Continuous wavelet transform, eq (8.107).
#' Continuous wavelet transform, eq (8.107)
#'
#' Part of the rangayyan_tf implementation; see the file header for the
#' source it follows.
#'
#' @param x See Usage.
#' @param fs Defaults to \code{1}.
#' @param wavelet Defaults to \code{"morlet"}.
#' @param scales Defaults to \code{NULL}.
#' @param w0 Defaults to \code{5}.
#' @return A list with \code{coeffs}, \code{scales}, \code{freqs}, \code{times}, \code{energy_per_scale}, \code{peak_scale}, \code{wavelet}, \code{method}.
#' @export
Cwt <- function(x, fs = 1, wavelet = "morlet", scales = NULL, w0 = 5) {
  v <- .tf_need(x, "x", 4L)
  fs <- as.numeric(fs)
  if (fs <= 0) stop("fs must be positive")
  if (is.null(scales)) {
    sc <- numeric(0)
    s <- 1
    while (s <= max(1, length(v) / 8)) {
      sc <- c(sc, s)
      s <- s * 2
    }
    scales <- if (length(sc)) sc else 1
  }
  sc <- as.numeric(scales)
  if (!length(sc)) stop("scales must not be empty")
  if (any(sc <= 0)) stop("all scales must be positive")
  co <- .tf_cwt(v, sc, wavelet, as.numeric(w0))
  nm <- tolower(trimws(as.character(wavelet)))
  fc <- switch(nm,
    morlet = as.numeric(w0) / (2 * pi),
    mexh = 0.25,
    mexicanhat = 0.25,
    sombrero = 0.25,
    ricker = 0.25,
    haar = 0.5,
    db1 = 0.5,
    stop(sprintf("unknown wavelet '%s'", nm))
  )
  epr <- vapply(co, function(r) .morie_fsum(Mod(r)^2), numeric(1))
  list(
    coeffs = co, scales = sc, freqs = fc * fs / sc,
    times = (seq_along(v) - 1) / fs, energy_per_scale = epr,
    peak_scale = sc[which.max(epr)], wavelet = as.character(wavelet),
    method = paste(
      "Continuous wavelet transform, Rangayyan & Krishnan",
      "(2024) eq (8.107); mother wavelets eqs (8.115)/(8.116)"
    )
  )
}

# -- Cohen's class generalised TFD, eqs (8.124)-(8.127).
#' Cohen\'s class generalised TFD, eqs (8.124)-(8.127)
#'
#' Part of the rangayyan_tf implementation; see the file header for the
#' source it follows.
#'
#' @param x See Usage.
#' @param fs Defaults to \code{1}.
#' @param kernel Defaults to \code{"spwvd"}.
#' @param nfreq Defaults to \code{NULL}.
#' @param tsmooth Defaults to \code{NULL}.
#' @param fsmooth Defaults to \code{NULL}.
#' @return A list with \code{tfd}, \code{times}, \code{freqs}, \code{kernel}, \code{tsmooth}, \code{fsmooth}, \code{peak_freq}, \code{crossterm_ratio}, \code{method}.
#' @export
Gtfd <- function(x, fs = 1, kernel = "spwvd", nfreq = NULL,
                 tsmooth = NULL, fsmooth = NULL) {
  v <- .tf_need(x, "x", 4L)
  fs <- as.numeric(fs)
  if (fs <= 0) stop("fs must be positive")
  k <- tolower(trimws(as.character(kernel)))
  if (!(k %in% c("wvd", "pwvd", "swvd", "spwvd"))) {
    stop(sprintf("unknown kernel '%s'; use 'wvd', 'pwvd', 'swvd' or 'spwvd'", k))
  }
  nf <- as.integer(if (is.null(nfreq) || nfreq == 0) length(v) else nfreq)
  w <- .tf_wvd(v, fs, nf)
  tfd <- w$tfd
  freqs <- w$freqs
  tl <- if (is.null(tsmooth)) max(3L, length(v) %/% 8L) else as.integer(tsmooth)
  fl <- if (is.null(fsmooth)) max(3L, nf %/% 8L) else as.integer(fsmooth)
  if (tl < 1L || fl < 1L) stop("tsmooth and fsmooth must be >= 1")
  if (k == "wvd") {
    tl <- 1L
    fl <- 1L
  } else if (k == "pwvd") {
    tl <- 1L
  } else if (k == "swvd") {
    fl <- 1L
  }
  if (tl > 1L || fl > 1L) tfd <- .tf_smooth2d(tfd, tl, fl)
  flat <- as.vector(t(tfd))
  tot <- .morie_fsum(abs(flat))
  neg <- .morie_fsum(-flat[flat < 0])
  col <- vapply(seq_len(nf), function(j) .morie_fsum(tfd[, j]), numeric(1))
  list(
    tfd = tfd, times = (seq_along(v) - 1) / fs, freqs = freqs, kernel = k,
    tsmooth = tl, fsmooth = fl, peak_freq = freqs[which.max(col)],
    crossterm_ratio = if (tot > 0) neg / tot else 0,
    method = paste(
      "Cohen's class generalised TFD, Rangayyan & Krishnan",
      "(2024) eq (8.124), evaluated as the smoothed WVD of",
      "eqs (8.125)-(8.127) with separable Gaussian kernels"
    )
  )
}

# -- Daubechies filter bank taps and their orthonormality identities.
#' Daubechies filter bank taps and their orthonormality identities
#'
#' Part of the rangayyan_tf implementation; see the file header for the
#' source it follows.
#'
#' @param order Defaults to \code{4}.
#' @return A list with \code{dec_lo}, \code{dec_hi}, \code{rec_lo}, \code{rec_hi}, \code{order}, \code{length}, \code{vanishing_moments}, \code{sum_lo}, \code{norm_lo}, \code{max_shift_inner_product}, \code{method}.
#' @export
OrthFilt <- function(order = 4) {
  k <- as.integer(order)
  if (is.null(.TF_DBTAPS[[as.character(k)]])) {
    stop(sprintf("order must be an integer in 1..10, got %s", as.character(order)))
  }
  f <- .tf_filters(paste0("db", k))
  h <- f$h
  L <- length(h)
  worst <- 0
  if (L %/% 2L >= 2L) {
    for (m in seq_len(L %/% 2L - 1L)) {
      worst <- max(worst, abs(.morie_fsum(h[seq_len(L - 2L * m)] *
        h[seq_len(L - 2L * m) + 2L * m])))
    }
  }
  list(
    dec_lo = h, dec_hi = f$g, rec_lo = f$rec_lo, rec_hi = f$rec_hi,
    order = k, length = L, vanishing_moments = k,
    sum_lo = .morie_fsum(h), norm_lo = .morie_fsum(h * h),
    max_shift_inner_product = worst,
    method = paste(
      "Daubechies orthogonal scaling/wavelet filters,",
      "Daubechies (1992) Ten Lectures on Wavelets Table 6.1;",
      "family cited by Rangayyan & Krishnan (2024) Sec 8.8",
      "but not tabulated there"
    )
  )
}

# -- Matching-pursuit TFD, eq (9.15) over the Gabor dictionary.
#' Matching-pursuit TFD, eq (9.15) over the Gabor dictionary
#'
#' Part of the rangayyan_tf implementation; see the file header for the
#' source it follows.
#'
#' @param x See Usage.
#' @param fs Defaults to \code{1}.
#' @param dictionary Defaults to \code{"gabor"}.
#' @param max_atoms Defaults to \code{8}.
#' @param nfreq Defaults to \code{NULL}.
#' @param min_decay Defaults to \code{0.001}.
#' @return A list with \code{tfd}, \code{times}, \code{freqs}, \code{atoms}, \code{n_atoms}, \code{decay}, \code{residual_energy}, \code{explained}, \code{peak_freq}, \code{method}.
#' @export
AtomTfd <- function(x, fs = 1, dictionary = "gabor", max_atoms = 8,
                    nfreq = NULL, min_decay = 1e-3) {
  v <- .tf_need(x, "x", 8L)
  fs <- as.numeric(fs)
  if (fs <= 0) stop("fs must be positive")
  dic <- tolower(trimws(as.character(dictionary)))
  if (!(dic %in% c("gabor", "fourier"))) {
    stop(sprintf("unknown dictionary '%s'; use 'gabor' or 'fourier'", dic))
  }
  ma <- as.integer(max_atoms)
  if (ma < 1L) stop("max_atoms must be >= 1")
  n <- length(v)
  nf <- as.integer(if (is.null(nfreq) || nfreq == 0) n else nfreq)
  # 0-based sample and bin arithmetic throughout, exactly as in Python: tau
  # here is a TIME OFFSET, not an index into a vector.
  i0 <- seq_len(n) - 1
  scales <- c(n / 2, n / 4, n / 8, n / 16)
  scales <- scales[scales >= 2]
  if (dic == "fourier") scales <- as.numeric(n)
  shifts <- seq(0, n - 1, by = max(1L, n %/% 8L))
  kmax <- nf

  atom <- function(s, tau, k) {
    f <- k * fs / (2 * nf)
    if (dic == "gabor") {
      u <- (i0 - tau) / s
      env <- (2^0.25) * exp(-pi * u * u) / sqrt(s)
    } else {
      env <- rep(1 / sqrt(n), n)
    }
    out <- env * exp(complex(imaginary = 2 * pi * f * i0 / fs))
    nrm <- sqrt(.morie_fsum(Mod(out)^2))
    if (nrm <= 0) stop("degenerate atom (zero norm)")
    out / nrm
  }

  res <- .tf_analytic(v)
  e0 <- .tf_energy(res)
  decay <- numeric(0)
  prev <- e0
  atoms <- list()
  for (rep_i in seq_len(ma)) {
    bestval <- -1
    best <- NULL
    for (s in scales) {
      for (tau in shifts) {
        for (k in seq_len(kmax - 1L)) {
          g <- atom(s, tau, k)
          ip <- sum(res * Conj(g))
          if (Mod(ip) > bestval) {
            bestval <- Mod(ip)
            best <- list(s = s, tau = tau, k = k, ip = ip, g = g)
          }
        }
      }
    }
    if (is.null(best)) break
    res <- res - best$ip * best$g
    cur <- .tf_energy(res)
    lam <- if (prev > 0) sqrt(max(0, 1 - cur / prev)) else 0
    decay <- c(decay, lam)
    atoms[[length(atoms) + 1L]] <- list(
      scale = best$s, translation = best$tau,
      freq = best$k * fs / (2 * nf),
      coeff = Mod(best$ip)
    )
    prev <- cur
    if (lam < as.numeric(min_decay)) break
  }

  k0 <- seq_len(nf) - 1
  freqs <- k0 * fs / (2 * nf)
  tfd <- matrix(0, n, nf)
  df <- fs / (2 * nf)
  for (a in atoms) {
    te <- exp(-2 * pi * ((i0 - a$translation) / a$scale)^2)
    fe <- exp(-2 * pi * ((k0 * df - a$freq) * a$scale / fs)^2)
    ok <- te >= 1e-12
    if (any(ok)) {
      tfd[ok, ] <- tfd[ok, , drop = FALSE] +
        (a$coeff * a$coeff) * outer(te[ok], fe)
    }
  }
  col <- vapply(seq_len(nf), function(k) .morie_fsum(tfd[, k]), numeric(1))
  list(
    tfd = tfd, times = i0 / fs, freqs = freqs, atoms = atoms,
    n_atoms = length(atoms), decay = decay, residual_energy = prev,
    explained = if (e0 > 0) 1 - prev / e0 else 0,
    peak_freq = if (nf > 0L) freqs[which.max(col)] else 0,
    method = paste(
      "Matching-pursuit TFD (MPTFD), Rangayyan & Krishnan (2024)",
      "eq (9.15), over the Gabor dictionary of eqs (9.2)-(9.3)",
      "with the eq (9.6) decay stopping rule"
    )
  )
}

# -- Dyadic DWT via the decimated filter bank, eqs (8.111)-(8.113).
#' Dyadic DWT via the decimated filter bank, eqs (8.111)-(8.113)
#'
#' Part of the rangayyan_tf implementation; see the file header for the
#' source it follows.
#'
#' @param x See Usage.
#' @param wavelet Defaults to \code{"db4"}.
#' @param levels Defaults to \code{3}.
#' @return A list with \code{approx}, \code{details}, \code{coeffs}, \code{lengths}, \code{levels}, \code{wavelet}, \code{energy}, \code{method}.
#' @export
Dwt <- function(x, wavelet = "db4", levels = 3) {
  v <- .tf_need(x, "x", 2L)
  lv <- as.integer(levels)
  r <- .tf_dwt(v, wavelet, lv)
  coeffs <- c(list(r$approx), r$details)
  list(
    approx = r$approx, details = r$details, coeffs = coeffs,
    lengths = r$lengths, levels = lv, wavelet = as.character(wavelet),
    energy = .morie_fsum(vapply(
      coeffs, function(c) .morie_fsum(c * c),
      numeric(1)
    )),
    method = paste(
      "Dyadic DWT via the decimated filter bank, Rangayyan &",
      "Krishnan (2024) eqs (8.111)-(8.113); Mallat (1989)",
      "algorithm, periodic extension"
    )
  )
}

# -- Ensemble EMD, Sec 9.4.1 eq (9.13).
#' Ensemble EMD, Sec 9.4.1 eq (9.13)
#'
#' Part of the rangayyan_tf implementation; see the file header for the
#' source it follows.
#'
#' @param x See Usage.
#' @param n_ensembles Defaults to \code{20}.
#' @param noise_std Defaults to \code{0.2}.
#' @param max_imfs Defaults to \code{8}.
#' @param seed Defaults to \code{0}.
#' @return A list with \code{imfs}, \code{residual}, \code{n_imfs}, \code{n_ensembles}, \code{noise_std}, \code{seed}, \code{reconstruction_error}, \code{energy_per_imf}, \code{method}.
#' @export
EmdEns <- function(x, n_ensembles = 20, noise_std = 0.2, max_imfs = 8, seed = 0) {
  v <- .tf_need(x, "x", 8L)
  ne <- as.integer(n_ensembles)
  if (ne < 1L) stop("n_ensembles must be >= 1")
  ns <- as.numeric(noise_std)
  if (ns < 0) stop("noise_std must be non-negative")
  n <- length(v)
  mu <- .morie_fsum(v) / n
  sdv <- sqrt(.morie_fsum((v - mu)^2) / n)
  amp <- ns * (if (sdv > 0) sdv else 1)
  st <- .tf_lcg_seed(seed)
  mi <- as.integer(max_imfs)
  acc <- matrix(0, mi, n)
  cnt <- integer(mi)
  resacc <- numeric(n)
  for (kk in seq_len(ne)) {
    noise <- numeric(0)
    while (length(noise) < n) {
      st <- .tf_lcg_step(st)
      u1 <- .tf_lcg_unif(st)
      st <- .tf_lcg_step(st)
      u2 <- .tf_lcg_unif(st)
      r <- sqrt(-2 * log(u1))
      noise <- c(noise, amp * r * cos(2 * pi * u2), amp * r * sin(2 * pi * u2))
    }
    xk <- v + noise[seq_len(n)]
    e <- .tf_emd(xk, mi, 0.05)
    nk <- min(length(e$imfs), mi)
    if (nk > 0L) {
      for (j in seq_len(nk)) {
        acc[j, ] <- acc[j, ] + e$imfs[[j]]
        cnt[j] <- cnt[j] + 1L
      }
    }
    resacc <- resacc + e$residual
  }
  out <- list()
  for (j in seq_len(mi)) {
    if (cnt[j] == 0L) break
    out[[length(out) + 1L]] <- acc[j, ] / ne
  }
  resid <- resacc / ne
  tot <- vapply(
    seq_len(n), function(i) {
      .morie_fsum(c(resid[i], vapply(out, function(c) c[i], numeric(1))))
    },
    numeric(1)
  )
  err <- max(abs(tot - v))
  list(
    imfs = out, residual = resid, n_imfs = length(out), n_ensembles = ne,
    noise_std = ns, seed = as.integer(seed), reconstruction_error = err,
    energy_per_imf = vapply(out, function(c) .morie_fsum(c^2), numeric(1)),
    method = paste(
      "Ensemble EMD, Rangayyan & Krishnan (2024) Sec 9.4.1",
      "eq (9.13) and steps 1-4; method of Wu & Huang (2009),",
      "the book's reference [17]"
    )
  )
}

# -- Empirical mode decomposition by sifting, Sec 9.4 steps 1-6.
#' Empirical mode decomposition by sifting, Sec 9.4 steps 1-6
#'
#' Part of the rangayyan_tf implementation; see the file header for the
#' source it follows.
#'
#' @param x See Usage.
#' @param max_imfs Defaults to \code{10}.
#' @param tol Defaults to \code{0.05}.
#' @return A list with \code{imfs}, \code{residual}, \code{n_imfs}, \code{reconstruction_error}, \code{energy_per_imf}, \code{tol}, \code{method}.
#' @export
Sift <- function(x, max_imfs = 10, tol = 0.05) {
  v <- .tf_need(x, "x", 8L)
  mi <- as.integer(max_imfs)
  if (mi < 1L) stop("max_imfs must be >= 1")
  t <- as.numeric(tol)
  if (t <= 0) stop("tol must be positive")
  e <- .tf_emd(v, mi, t)
  n <- length(v)
  tot <- vapply(
    seq_len(n), function(i) {
      .morie_fsum(c(e$residual[i], vapply(e$imfs, function(c) c[i], numeric(1))))
    },
    numeric(1)
  )
  list(
    imfs = e$imfs, residual = e$residual, n_imfs = length(e$imfs),
    reconstruction_error = max(abs(tot - v)),
    energy_per_imf = vapply(e$imfs, function(c) .morie_fsum(c^2), numeric(1)),
    tol = t,
    method = paste(
      "Empirical mode decomposition by sifting, Rangayyan &",
      "Krishnan (2024) Sec 9.4 algorithm steps 1-6; SD stopping",
      "rule from Huang et al. (1998) Proc. R. Soc. A 454"
    )
  )
}

# -- One IMF plus its admissibility evidence, Sec 9.4 and eqs (9.8)-(9.11).
#' One IMF plus its admissibility evidence, Sec 9.4 and eqs (9.8)-(9.11)
#'
#' Part of the rangayyan_tf implementation; see the file header for the
#' source it follows.
#'
#' @param x See Usage.
#' @param max_iter Defaults to \code{50}.
#' @param tol Defaults to \code{0.05}.
#' @return A list with \code{imf}, \code{residual}, \code{n_extrema}, \code{n_zero_crossings}, \code{extrema_zerox_ok}, \code{mean_envelope}, \code{max_envelope_mean}, \code{envelope_mean_ok}, \code{is_imf}, \code{iterations}, \code{converged}, \code{amplitude}, \code{phase}, \code{method}.
#' @export
Imf <- function(x, max_iter = 50, tol = 0.05) {
  v <- .tf_need(x, "x", 8L)
  mi <- as.integer(max_iter)
  if (mi < 1L) stop("max_iter must be >= 1")
  t <- as.numeric(tol)
  if (t <= 0) stop("tol must be positive")
  e0 <- .tf_extrema(v)
  if (length(e0$mx) < 2L || length(e0$mn) < 2L) {
    stop(sprintf(paste0(
      "the signal has %d maxima and %d minima; at least two ",
      "of each are needed to build the spline envelopes of ",
      "Sec 9.4 step 2"
    ), length(e0$mx), length(e0$mn)))
  }
  s <- .tf_sift(v, mi, t)
  c <- s$imf
  n <- length(c)
  ex <- .tf_extrema(c)
  nex <- length(ex$mx) + length(ex$mn)
  nzx <- .tf_zerox(c)
  cond1 <- abs(nex - nzx) <= 1L
  if (length(ex$mx) >= 2L && length(ex$mn) >= 2L) {
    tt <- seq_len(n)
    up <- .tf_spline(c(1L, ex$mx, n), c(c[1L], c[ex$mx], c[n]), tt)
    lo <- .tf_spline(c(1L, ex$mn, n), c(c[1L], c[ex$mn], c[n]), tt)
    menv <- (up + lo) / 2
  } else {
    menv <- numeric(n)
  }
  amp <- max(abs(c))
  if (amp == 0) amp <- 1
  mem <- max(abs(menv))
  cond2 <- mem <= 0.05 * amp
  za <- .tf_analytic(c)
  list(
    imf = c, residual = v - c, n_extrema = nex, n_zero_crossings = nzx,
    extrema_zerox_ok = cond1, mean_envelope = menv, max_envelope_mean = mem,
    envelope_mean_ok = cond2, is_imf = cond1 && cond2,
    iterations = s$iterations, converged = s$converged,
    amplitude = Mod(za), phase = atan2(Im(za), Re(za)),
    method = paste(
      "IMF extraction and admissibility test, Rangayyan &",
      "Krishnan (2024) Sec 9.4 (IMF properties) with the",
      "analytic-signal quantities of eqs (9.8)-(9.11)"
    )
  )
}

# -- Odd/even T-wave alternans after EMD detrending, Sec 9.2.3 + Sec 9.4.
#' Odd/even T-wave alternans after EMD detrending, Sec 9.2.3 + Sec 9.4
#'
#' Part of the rangayyan_tf implementation; see the file header for the
#' source it follows.
#'
#' @param ecg See Usage.
#' @param fs Defaults to \code{250}.
#' @param r_peaks Defaults to \code{NULL}.
#' @param twa_window Defaults to \code{c(0.15, 0.4)}.
#' @param max_imfs Defaults to \code{6}.
#' @return A list with \code{twa_amplitude}, \code{twa_rms}, \code{odd_mean}, \code{even_mean}, \code{difference}, \code{n_beats}, \code{n_odd}, \code{n_even}, \code{r_peaks}, \code{rpeaks_supplied}, \code{method}.
#' @export
TwaEmd <- function(ecg, fs = 250, r_peaks = NULL, twa_window = c(0.15, 0.40),
                   max_imfs = 6) {
  v <- .tf_need(ecg, "ecg", 32L)
  fs <- as.numeric(fs)
  if (fs <= 0) stop("fs must be positive")
  t0 <- as.numeric(twa_window[1L])
  t1 <- as.numeric(twa_window[2L])
  if (!(0 <= t0 && t0 < t1)) {
    stop(sprintf("twa_window must satisfy 0 <= start < end, got (%s, %s)", t0, t1))
  }
  n <- length(v)
  e <- .tf_emd(v, as.integer(max_imfs), 0.05)
  det <- if (length(e$imfs)) v - e$residual else v
  supplied <- !is.null(r_peaks)
  if (supplied) {
    rp <- as.integer(r_peaks)
  } else {
    mx <- max(abs(v))
    if (mx == 0) mx <- 1
    thr <- 0.5 * mx
    refr <- max(1L, as.integer(trunc(0.2 * fs)))
    rp <- integer(0)
    i <- 1L
    while (i <= n) {
      if (abs(v[i]) >= thr) {
        j <- i
        while (j <= n && abs(v[j]) >= thr) j <- j + 1L
        run <- i:(j - 1L)
        rp <- c(rp, run[which.max(abs(v[run]))])
        i <- j + refr
      } else {
        i <- i + 1L
      }
    }
  }
  rp <- rp[rp >= 1L & rp <= n]
  if (length(rp) < 4L) {
    stop(sprintf(paste0(
      "only %d R peaks available; at least 4 are needed for ",
      "an odd/even T-wave comparison"
    ), length(rp)))
  }
  a <- as.integer(trunc(t0 * fs))
  b <- as.integer(trunc(t1 * fs))
  if (b <= a) stop("the T-wave window is empty at this sampling rate")
  odd <- list()
  even <- list()
  for (bi in seq_along(rp)) {
    p <- rp[bi]
    if (p + b - 1L > n) next
    seg <- det[(p + a):(p + b - 1L)]
    if ((bi - 1L) %% 2L == 0L) {
      even[[length(even) + 1L]] <- seg
    } else {
      odd[[length(odd) + 1L]] <- seg
    }
  }
  if (length(odd) < 2L || length(even) < 2L) {
    stop(sprintf(paste0(
      "only %d even and %d odd complete T windows; at least ",
      "two of each are needed"
    ), length(even), length(odd)))
  }
  m <- b - a
  om <- vapply(seq_len(m), function(i) {
    .morie_fsum(vapply(odd, function(s) s[i], numeric(1))) / length(odd)
  }, numeric(1))
  em <- vapply(seq_len(m), function(i) {
    .morie_fsum(vapply(even, function(s) s[i], numeric(1))) / length(even)
  }, numeric(1))
  diff <- om - em
  list(
    twa_amplitude = max(abs(diff)),
    twa_rms = sqrt(.morie_fsum(diff * diff) / m),
    odd_mean = om, even_mean = em, difference = diff, n_beats = length(rp),
    n_odd = length(odd), n_even = length(even), r_peaks = rp,
    rpeaks_supplied = supplied,
    method = paste(
      "Odd/even T-wave alternans amplitude after EMD",
      "detrending; Rangayyan & Krishnan (2024) Sec 9.2.3 (TWA",
      "definition) and Sec 9.4 (EMD).  The book gives no",
      "alternans threshold, so none is applied"
    )
  )
}

# -- IMF-based characterisation of ventricular fibrillation, Sec 8.16.
#' IMF-based characterisation of ventricular fibrillation, Sec 8.16
#'
#' Part of the rangayyan_tf implementation; see the file header for the
#' source it follows.
#'
#' @param ecg See Usage.
#' @param fs Defaults to \code{250}.
#' @param n_imfs Defaults to \code{6}.
#' @param tol Defaults to \code{0.05}.
#' @return A list with \code{imfs}, \code{residual}, \code{n_imfs}, \code{features}, \code{dominant_imf}, \code{dominant_freq}, \code{method}.
#' @export
VfEmd <- function(ecg, fs = 250, n_imfs = 6, tol = 0.05) {
  v <- .tf_need(ecg, "ecg", 16L)
  fs <- as.numeric(fs)
  if (fs <= 0) stop("fs must be positive")
  ni <- as.integer(n_imfs)
  if (ni < 1L) stop("n_imfs must be >= 1")
  n <- length(v)
  e <- .tf_emd(v, ni, as.numeric(tol))
  tot <- .morie_fsum(vapply(e$imfs, function(c) .morie_fsum(c * c), numeric(1)))
  if (!length(e$imfs) || tot == 0) tot <- 1
  feats <- vector("list", length(e$imfs))
  for (q in seq_along(e$imfs)) {
    c <- e$imfs[[q]]
    za <- .tf_analytic(c)
    a <- Mod(za)
    ph <- atan2(Im(za), Re(za))
    fi <- numeric(0)
    if (n >= 2L) {
      fi <- numeric(n - 1L)
      for (i in 2L:n) {
        d <- ph[i] - ph[i - 1L]
        while (d > pi) d <- d - 2 * pi
        while (d < -pi) d <- d + 2 * pi
        fi[i - 1L] <- abs(d) * fs / (2 * pi)
      }
    }
    mf <- if (length(fi)) .morie_fsum(fi) / length(fi) else 0
    sdv <- if (length(fi)) sqrt(.morie_fsum((fi - mf)^2) / length(fi)) else 0
    en <- .morie_fsum(c * c)
    feats[[q]] <- list(
      energy = en, relative_energy = en / tot, mean_freq = mf,
      freq_std = sdv, mean_amplitude = .morie_fsum(a) / n
    )
  }
  dom <- if (length(feats)) {
    which.max(vapply(feats, function(f) f$energy, numeric(1)))
  } else {
    NULL
  }
  list(
    imfs = e$imfs, residual = e$residual, n_imfs = length(e$imfs),
    features = feats, dominant_imf = dom,
    dominant_freq = if (!is.null(dom)) feats[[dom]]$mean_freq else NULL,
    method = paste(
      "IMF-based characterisation of ventricular fibrillation,",
      "Rangayyan & Krishnan (2024) Sec 8.16 with the EMD of",
      "Sec 9.4 and eqs (9.8)-(9.11)"
    )
  )
}

# -- Wavelet (relative-energy Shannon) entropy, Rosso et al. (2001).
#' Wavelet (relative-energy Shannon) entropy, Rosso et al. (2001)
#'
#' Part of the rangayyan_tf implementation; see the file header for the
#' source it follows.
#'
#' @param x See Usage.
#' @param wavelet Defaults to \code{"db4"}.
#' @param levels Defaults to \code{3}.
#' @param base Defaults to \code{"e"}.
#' @return A list with \code{entropy}, \code{max_entropy}, \code{normalized_entropy}, \code{relative_energy}, \code{labels}, \code{levels}, \code{base}, \code{wavelet}, \code{method}.
#' @export
WtEntropy <- function(x, wavelet = "db4", levels = 3, base = "e") {
  b <- tolower(trimws(as.character(base)))
  if (!(b %in% c("e", "2"))) stop("base must be 'e' or '2'")
  r <- WtEnergy(x, wavelet = wavelet, levels = levels)
  p <- r$relative
  if (r$total_energy <= 0) {
    stop("the signal has zero energy; wavelet entropy is undefined")
  }
  ent <- -.morie_fsum(p[p > 0] * log(p[p > 0]))
  mx <- log(length(p))
  if (b == "2") {
    ent <- ent / log(2)
    mx <- mx / log(2)
  }
  list(
    entropy = ent, max_entropy = mx,
    normalized_entropy = if (mx > 0) ent / mx else 0,
    relative_energy = p, labels = r$labels, levels = as.integer(levels),
    base = b, wavelet = as.character(wavelet),
    method = paste(
      "Wavelet (relative-energy Shannon) entropy, Rosso et al.",
      "(2001) J. Neurosci. Methods 105(1):65-75, over the",
      "Rangayyan & Krishnan (2024) eq (8.111)-(8.113) DWT"
    )
  )
}

# -- Two-tap (Haar / db1) orthogonal DWT.
#' Two-tap (Haar / db1) orthogonal DWT
#'
#' Part of the rangayyan_tf implementation; see the file header for the
#' source it follows.
#'
#' @param x See Usage.
#' @param levels Defaults to \code{3}.
#' @return A list with \code{approx}, \code{details}, \code{coeffs}, \code{lengths}, \code{levels}, \code{energy}, \code{input_energy}, \code{method}.
#' @export
Dwt2Tap <- function(x, levels = 3) {
  v <- .tf_need(x, "x", 2L)
  lv <- as.integer(levels)
  r <- .tf_dwt(v, "db1", lv)
  coeffs <- c(list(r$approx), r$details)
  list(
    approx = r$approx, details = r$details, coeffs = coeffs,
    lengths = r$lengths, levels = lv,
    energy = .morie_fsum(vapply(
      coeffs, function(c) .morie_fsum(c * c),
      numeric(1)
    )),
    input_energy = .morie_fsum(v * v),
    method = paste(
      "Two-tap (Haar / db1) orthogonal DWT; the L=2 case of",
      "Daubechies (1992) Table 6.1, dyadic grid per",
      "Rangayyan & Krishnan (2024) eq (8.113)"
    )
  )
}

# -- EMD-based instantaneous-frequency spectrum, Sec 9.4 eqs (9.8)-(9.12).
#' EMD-based instantaneous-frequency spectrum, Sec 9.4 eqs (9.8)-(9.12)
#'
#' Part of the rangayyan_tf implementation; see the file header for the
#' source it follows.
#'
#' @param x See Usage.
#' @param fs Defaults to \code{1}.
#' @param max_imfs Defaults to \code{8}.
#' @param nfreq Defaults to \code{32}.
#' @param tol Defaults to \code{0.05}.
#' @return A list with \code{spectrum}, \code{times}, \code{freqs}, \code{imfs}, \code{amplitude}, \code{inst_freq}, \code{marginal}, \code{n_imfs}, \code{peak_freq}, \code{method}.
#' @export
EmdSpec <- function(x, fs = 1, max_imfs = 8, nfreq = 32, tol = 0.05) {
  v <- .tf_need(x, "x", 8L)
  fs <- as.numeric(fs)
  if (fs <= 0) stop("fs must be positive")
  nf <- as.integer(nfreq)
  if (nf < 2L) stop("nfreq must be >= 2")
  n <- length(v)
  e <- .tf_emd(v, as.integer(max_imfs), as.numeric(tol))
  freqs <- (seq_len(nf) - 1) * fs / (2 * nf)
  df <- fs / (2 * nf)
  spec <- matrix(0, n, nf)
  amps <- vector("list", length(e$imfs))
  ifs <- vector("list", length(e$imfs))
  for (q in seq_along(e$imfs)) {
    c <- e$imfs[[q]]
    za <- .tf_analytic(c)
    a <- Mod(za)
    ph <- atan2(Im(za), Re(za))
    unw <- numeric(n)
    unw[1L] <- ph[1L]
    for (i in 2L:n) {
      d <- ph[i] - ph[i - 1L]
      while (d > pi) d <- d - 2 * pi
      while (d < -pi) d <- d + 2 * pi
      unw[i] <- unw[i - 1L] + d
    }
    fi <- numeric(n)
    for (i in seq_len(n)) {
      dth <- if (i == 1L) {
        unw[2L] - unw[1L]
      } else if (i == n) {
        unw[n] - unw[n - 1L]
      } else {
        (unw[i + 1L] - unw[i - 1L]) / 2
      }
      fi[i] <- abs(dth) * fs / (2 * pi)
    }
    amps[[q]] <- a
    ifs[[q]] <- fi
    for (i in seq_len(n)) {
      k <- as.integer(trunc(fi[i] / df)) # 0-based bin, as in Python
      if (k >= 0L && k < nf) spec[i, k + 1L] <- spec[i, k + 1L] + a[i] * a[i]
    }
  }
  marg <- vapply(seq_len(nf), function(k) .morie_fsum(spec[, k]), numeric(1))
  list(
    spectrum = spec, times = (seq_len(n) - 1) / fs, freqs = freqs,
    imfs = e$imfs, amplitude = amps, inst_freq = ifs, marginal = marg,
    n_imfs = length(e$imfs),
    peak_freq = if (nf > 0L) freqs[which.max(marg)] else 0,
    method = paste(
      "EMD-based instantaneous-frequency spectrum, Rangayyan &",
      "Krishnan (2024) Sec 9.4 eqs (9.8)-(9.12)"
    )
  )
}

# -- Time-varying HRV band powers, Sec 8.12.
#' Time-varying HRV band powers, Sec 8.12
#'
#' Part of the rangayyan_tf implementation; see the file header for the
#' source it follows.
#'
#' @param rr_intervals See Usage.
#' @param fs_resamp Defaults to \code{4}.
#' @param window_len Defaults to \code{64}.
#' @param noverlap Defaults to \code{NULL}.
#' @param standard Defaults to \code{"taskforce"}.
#' @return A list with \code{times}, \code{vlf}, \code{lf}, \code{hf}, \code{total_power}, \code{lf_hf_ratio}, \code{lf_percent}, \code{hf_percent}, \code{bands}, \code{standard}, \code{mean_rr}, \code{mean_hr}, \code{resampled}, \code{fs_resamp}, \code{method}.
#' @export
HrvTv <- function(rr_intervals, fs_resamp = 4, window_len = 64,
                  noverlap = NULL, standard = "taskforce") {
  rr <- .tf_need(rr_intervals, "rr_intervals", 4L)
  if (any(rr <= 0)) stop("all RR intervals must be positive (seconds)")
  st <- tolower(trimws(as.character(standard)))
  bands <- if (st == "taskforce") {
    list(vlf = c(0, 0.04), lf = c(0.04, 0.15), hf = c(0.15, 0.40))
  } else if (st == "bianchi") {
    list(vlf = c(0, 0.03), lf = c(0.03, 0.15), hf = c(0.18, 0.40))
  } else {
    stop(sprintf("standard must be 'taskforce' or 'bianchi', got '%s'", st))
  }
  fsr <- as.numeric(fs_resamp)
  if (fsr <= 0) stop("fs_resamp must be positive")
  # NOT cumsum(): R accumulates that in long double, Python in plain double.
  # The two then disagree in the last bits of the total duration, which is
  # enough to move int(dur * fs_resamp) by a whole sample.
  beat_t <- numeric(length(rr))
  acc <- 0
  for (i in seq_along(rr)) {
    acc <- acc + rr[i]
    beat_t[i] <- acc
  }
  dur <- beat_t[length(beat_t)]
  m <- as.integer(trunc(dur * fsr))
  if (m < 4L) stop("the RR series is too short to resample at this rate")
  grid <- (seq_len(m) - 1) / fsr
  resamp <- numeric(m)
  j <- 1L
  nb <- length(beat_t)
  for (q in seq_len(m)) {
    g <- grid[q]
    while (j <= nb - 2L && beat_t[j + 1L] < g) j <- j + 1L
    tt0 <- beat_t[j]
    tt1 <- beat_t[j + 1L]
    y0 <- rr[j]
    y1 <- rr[j + 1L]
    w <- if (tt1 == tt0) 0 else (g - tt0) / (tt1 - tt0)
    w <- max(0, min(1, w))
    resamp[q] <- y0 + (y1 - y0) * w
  }
  mu <- .morie_fsum(resamp) / m
  resamp <- resamp - mu
  wl <- as.integer(window_len)
  if (wl < 4L) stop("window_len must be >= 4")
  if (wl > m) {
    stop(sprintf(paste0(
      "window_len=%d exceeds the resampled length %d; use a ",
      "shorter window or a higher fs_resamp"
    ), wl, m))
  }
  sp <- Spectrogram(resamp,
    fs = fsr, nperseg = wl, noverlap = noverlap,
    window = "hann"
  )
  fr <- sp$freqs
  nfr <- length(fr)
  out <- list(vlf = numeric(0), lf = numeric(0), hf = numeric(0))
  tot <- numeric(0)
  for (ri in seq_len(nrow(sp$spectrogram))) {
    row <- sp$spectrogram[ri, ]
    s <- numeric(3)
    names(s) <- c("vlf", "lf", "hf")
    for (nmb in c("vlf", "lf", "hf")) {
      ab <- bands[[nmb]]
      sel <- fr >= ab[1L] & fr < ab[2L]
      s[[nmb]] <- .morie_fsum(row[sel])
      out[[nmb]] <- c(out[[nmb]], s[[nmb]])
    }
    tot <- c(tot, .morie_fsum(s))
  }
  list(
    times = sp$times, vlf = out$vlf, lf = out$lf, hf = out$hf,
    total_power = tot,
    lf_hf_ratio = ifelse(out$hf > 0, out$lf / out$hf, Inf),
    lf_percent = ifelse(tot > 0, 100 * out$lf / tot, 0),
    hf_percent = ifelse(tot > 0, 100 * out$hf / tot, 0),
    bands = bands, standard = st,
    mean_rr = .morie_fsum(rr) / length(rr),
    mean_hr = 60 / (.morie_fsum(rr) / length(rr)),
    resampled = resamp, fs_resamp = fsr,
    method = paste(
      "Time-varying HRV band powers from the short-time",
      "spectrum of the RR tachogram, Rangayyan & Krishnan",
      "(2024) Sec 8.12; bands as given there (Task Force",
      "standard or Bianchi et al.); STFT per eq (8.8)"
    )
  )
}

# -- Weighted overlap-add inverse STFT, Griffin & Lim (1984) eq (6).
#' Weighted overlap-add inverse STFT, Griffin & Lim (1984) eq (6)
#'
#' Part of the rangayyan_tf implementation; see the file header for the
#' source it follows.
#'
#' @param stft See Usage.
#' @param window Defaults to \code{"hann"}.
#' @param hop Defaults to \code{NULL}.
#' @return A list with \code{signal}, \code{n}, \code{hop}, \code{valid_start}, \code{valid_end}, \code{n_frames}, \code{method}.
#' @export
IStft <- function(stft, window = "hann", hop = NULL) {
  frames <- if (is.matrix(stft)) {
    lapply(seq_len(nrow(stft)), function(i) stft[i, ])
  } else {
    lapply(stft, as.complex)
  }
  if (!length(frames)) stop("stft must contain at least one frame")
  m <- length(frames[[1L]])
  if (m < 2L) stop("each STFT frame must have at least 2 bins")
  if (any(vapply(frames, length, integer(1)) != m)) {
    stop("all STFT frames must have the same length")
  }
  h <- if (is.null(hop)) m %/% 2L else as.integer(hop)
  if (!(h >= 1L && h <= m)) {
    stop(sprintf("hop must satisfy 1 <= hop <= %d, got %d", m, h))
  }
  w <- if (is.character(window)) .tf_win(window, m) else as.numeric(window)
  if (length(w) != m) {
    stop("explicit window length must equal the frame length")
  }
  n <- (length(frames) - 1L) * h + m
  num <- numeric(n)
  den <- numeric(n)
  for (fi in seq_along(frames)) {
    seg <- .tf_idft(frames[[fi]])
    off <- (fi - 1L) * h
    idx <- off + seq_len(m)
    num[idx] <- num[idx] + Re(seg) * w
    den[idx] <- den[idx] + w * w
  }
  tol <- if (max(den) > 0) 1e-10 * max(den) else 0
  # Python checks the closed interior range(m-1, n-m+1) -- 1-based, m..n-m+1.
  if (n - m + 1L >= m) {
    for (i in m:(n - m + 1L)) {
      if (den[i] <= tol) {
        stop(sprintf(
          paste0(
            "sample %d lies in a gap between analysis windows ",
            "(hop=%d is too large for a window of length %d)"
          ),
          i, h, m
        ))
      }
    }
  }
  out <- ifelse(den > tol, num / den, 0)
  ok <- which(den > tol)
  lo <- if (length(ok)) ok[1L] else 1L
  hi <- if (length(ok)) ok[length(ok)] else n
  list(
    signal = out, n = n, hop = h, valid_start = lo, valid_end = hi,
    n_frames = length(frames),
    method = paste(
      "Weighted overlap-add inverse STFT, Griffin & Lim (1984)",
      "eq (6); forward transform is Rangayyan eq (8.8)"
    )
  )
}

# -- Multiresolution analysis, eqs (8.111)-(8.114).
#' Multiresolution analysis, eqs (8.111)-(8.114)
#'
#' Part of the rangayyan_tf implementation; see the file header for the
#' source it follows.
#'
#' @param x See Usage.
#' @param wavelet Defaults to \code{"db4"}.
#' @param levels Defaults to \code{3}.
#' @return A list with \code{approximation}, \code{details}, \code{bands}, \code{reconstruction_error}, \code{energy_per_band}, \code{levels}, \code{wavelet}, \code{method}.
#' @export
Mra <- function(x, wavelet = "db4", levels = 3) {
  v <- .tf_need(x, "x", 2L)
  lv <- as.integer(levels)
  r <- .tf_dwt(v, wavelet, lv)
  n <- length(v)
  rebuild <- function(which) {
    aa <- numeric(length(r$approx))
    dd <- lapply(r$details, function(c) numeric(length(c)))
    if (is.null(which)) aa <- r$approx else dd[[which]] <- r$details[[which]]
    .tf_idwt(aa, dd, r$lengths, wavelet)[seq_len(n)]
  }
  approx <- rebuild(NULL)
  details <- lapply(seq_along(r$details), rebuild)
  total <- vapply(
    seq_len(n), function(i) {
      approx[i] + .morie_fsum(vapply(details, function(b) b[i], numeric(1)))
    },
    numeric(1)
  )
  bands <- c(list(approx), details)
  list(
    approximation = approx, details = details, bands = bands,
    reconstruction_error = max(abs(total - v)),
    energy_per_band = vapply(bands, function(b) .morie_fsum(b * b), numeric(1)),
    levels = lv, wavelet = as.character(wavelet),
    method = paste(
      "Multiresolution analysis, Rangayyan & Krishnan (2024)",
      "eqs (8.111)-(8.114); Mallat (1989) IEEE PAMI 11(7)"
    )
  )
}

# -- ECG-triggered synchronised averaging of PCG envelopes, Sec 3.5 + 5.5.3.
#' ECG-triggered synchronised averaging of PCG envelopes, Sec 3.5 +
#' 5.5.3
#'
#' Part of the rangayyan_tf implementation; see the file header for the
#' source it follows.
#'
#' @param pcg See Usage.
#' @param ecg See Usage.
#' @param fs Defaults to \code{1000}.
#' @param cycle_len Defaults to \code{NULL}.
#' @param envelope_smoothing Defaults to \code{NULL}.
#' @return A list with \code{average_envelope}, \code{n_cycles}, \code{cycle_len}, \code{triggers}, \code{s1_index}, \code{s1_time}, \code{s1_amplitude}, \code{s2_index}, \code{s2_time}, \code{s2_amplitude}, \code{s2_s1_ratio}, \code{snr_gain_db}, \code{method}.
#' @export
PcgEnvAvg <- function(pcg, ecg, fs = 1000, cycle_len = NULL,
                      envelope_smoothing = NULL) {
  p <- .tf_need(pcg, "pcg", 16L)
  e <- .tf_need(ecg, "ecg", 16L)
  if (length(p) != length(e)) {
    stop(sprintf(
      "pcg and ecg must have the same length, got %d and %d",
      length(p), length(e)
    ))
  }
  fs <- as.numeric(fs)
  if (fs <= 0) stop("fs must be positive")
  n <- length(p)
  env <- Mod(.tf_analytic(p))
  w <- if (is.null(envelope_smoothing)) {
    max(1L, as.integer(trunc(0.02 * fs)))
  } else {
    as.integer(envelope_smoothing)
  }
  if (w < 1L) stop("envelope_smoothing must be >= 1")
  hw <- w %/% 2L
  sm <- vapply(seq_len(n), function(i) {
    a <- max(1L, i - hw)
    b <- min(n, i + hw)
    .morie_fsum(env[a:b]) / (b - a + 1L)
  }, numeric(1))
  mx <- max(abs(e))
  if (mx == 0) mx <- 1
  thr <- 0.6 * mx
  refr <- max(1L, as.integer(trunc(0.25 * fs)))
  trig <- integer(0)
  i <- 1L
  while (i <= n) {
    if (abs(e[i]) >= thr) {
      j <- i
      while (j <= n && abs(e[j]) >= thr) j <- j + 1L
      run <- i:(j - 1L)
      trig <- c(trig, run[which.max(abs(e[run]))])
      i <- j + refr
    } else {
      i <- i + 1L
    }
  }
  if (length(trig) < 2L) {
    stop(sprintf(paste0(
      "only %d QRS triggers found in the ECG; synchronised ",
      "averaging needs at least two cycles"
    ), length(trig)))
  }
  gaps <- sort(diff(trig))
  cl <- if (is.null(cycle_len)) gaps[length(gaps) %/% 2L + 1L] else as.integer(cycle_len)
  if (cl < 2L) stop("cycle_len must be >= 2 samples")
  keep <- trig[trig + cl - 1L <= n]
  cycles <- lapply(keep, function(t) sm[t:(t + cl - 1L)])
  if (length(cycles) < 2L) {
    stop(sprintf(paste0(
      "only %d complete cycle(s) of %d samples fit in the ",
      "record; at least two are needed"
    ), length(cycles), cl))
  }
  avg <- vapply(
    seq_len(cl), function(i) {
      .morie_fsum(vapply(cycles, function(c) c[i], numeric(1))) / length(cycles)
    },
    numeric(1)
  )
  half <- max(1L, cl %/% 3L)
  s1 <- which.max(avg[seq_len(half)])
  s2 <- if (cl > half) half + which.max(avg[(half + 1L):cl]) else s1
  list(
    average_envelope = avg, n_cycles = length(cycles), cycle_len = cl,
    triggers = trig, s1_index = s1, s1_time = (s1 - 1L) / fs,
    s1_amplitude = avg[s1], s2_index = s2, s2_time = (s2 - 1L) / fs,
    s2_amplitude = avg[s2],
    s2_s1_ratio = if (avg[s1] > 0) avg[s2] / avg[s1] else Inf,
    snr_gain_db = 10 * log(length(cycles)) / log(10),
    method = paste(
      "ECG-triggered synchronised averaging of PCG envelopes,",
      "Rangayyan & Krishnan (2024) Sec 3.5 (synchronised",
      "averaging) with the analytic-signal envelope of",
      "Sec 5.5.3"
    )
  )
}

# -- Wavelet-shrinkage denoising of PPG, Sec 8.14 with eqs (8.103)-(8.105).
#' Wavelet-shrinkage denoising of PPG, Sec 8.14 with eqs (8.103)-(8.105)
#'
#' Part of the rangayyan_tf implementation; see the file header for the
#' source it follows.
#'
#' @param ppg See Usage.
#' @param fs Defaults to \code{100}.
#' @param wavelet Defaults to \code{"db4"}.
#' @param levels Defaults to \code{4}.
#' @param threshold_type Defaults to \code{"soft"}.
#' @return A list with \code{denoised}, \code{artifact}, \code{threshold}, \code{sigma}, \code{artifact_energy}, \code{snr_improvement_db}, \code{approx_energy}, \code{levels}, \code{wavelet}, \code{fs}, \code{method}.
#' @export
PpgWtDen <- function(ppg, fs = 100, wavelet = "db4", levels = 4,
                     threshold_type = "soft") {
  v <- .tf_need(ppg, "ppg", 8L)
  fs <- as.numeric(fs)
  if (fs <= 0) stop("fs must be positive")
  r <- WtThresh(v,
    wavelet = wavelet, levels = as.integer(levels),
    threshold_type = threshold_type
  )
  den <- r$denoised
  art <- v - den
  ein <- .morie_fsum(v * v)
  ea <- .morie_fsum(art * art)
  a <- .tf_dwt(v, wavelet, as.integer(levels))$approx
  list(
    denoised = den, artifact = art, threshold = r$threshold, sigma = r$sigma,
    artifact_energy = ea,
    snr_improvement_db = if (ea > 0) 10 * log(ein / ea) / log(10) else Inf,
    approx_energy = .morie_fsum(a * a), levels = as.integer(levels),
    wavelet = as.character(wavelet), fs = fs,
    method = paste(
      "Wavelet-shrinkage denoising of PPG, Rangayyan & Krishnan",
      "(2024) Sec 8.14 (Daubechies wavelets best, per its",
      "reference [91]) with eqs (8.103)-(8.105)"
    )
  )
}

# -- Scalogram (|CWT|^2), eq (8.107) and Figure 8.29.
#' Scalogram (|CWT|^2), eq (8.107) and Figure 8.29
#'
#' Part of the rangayyan_tf implementation; see the file header for the
#' source it follows.
#'
#' @param x See Usage.
#' @param fs Defaults to \code{1}.
#' @param scales Defaults to \code{NULL}.
#' @param wavelet Defaults to \code{"morlet"}.
#' @param w0 Defaults to \code{5}.
#' @return A list with \code{scalogram}, \code{scales}, \code{freqs}, \code{times}, \code{energy_per_scale}, \code{total_energy}, \code{ridge}, \code{method}.
#' @export
Scalogram <- function(x, fs = 1, scales = NULL, wavelet = "morlet", w0 = 5) {
  r <- Cwt(x, fs = fs, wavelet = wavelet, scales = scales, w0 = w0)
  sg <- do.call(rbind, lapply(r$coeffs, function(row) Mod(row)^2))
  n <- ncol(sg)
  ridge <- vapply(seq_len(n), function(i) which.max(sg[, i]), integer(1))
  list(
    scalogram = sg, scales = r$scales, freqs = r$freqs, times = r$times,
    energy_per_scale = vapply(
      seq_len(nrow(sg)),
      function(s) .morie_fsum(sg[s, ]), numeric(1)
    ),
    total_energy = .morie_fsum(vapply(
      seq_len(nrow(sg)),
      function(s) .morie_fsum(sg[s, ]),
      numeric(1)
    )),
    ridge = ridge,
    method = paste(
      "Scalogram (|CWT|^2), Rangayyan & Krishnan (2024)",
      "eq (8.107) and Figure 8.29"
    )
  )
}

# -- Fluctuation intensity of DWT coefficients, Sec 8.17 eq (8.132).
#' Fluctuation intensity of DWT coefficients, Sec 8.17 eq (8.132)
#'
#' Part of the rangayyan_tf implementation; see the file header for the
#' source it follows.
#'
#' @param eeg See Usage.
#' @param fs Defaults to \code{1}.
#' @param wavelet Defaults to \code{"db4"}.
#' @param levels Defaults to \code{5}.
#' @param scales Defaults to \code{c(3, 4, 5)}.
#' @param threshold Defaults to \code{NULL}.
#' @return A list with \code{fi}, \code{fi_total}, \code{scales}, \code{bands}, \code{energies}, \code{seizure_detected}, \code{threshold}, \code{wavelet}, \code{levels}, \code{method}.
#' @export
SeizWt <- function(eeg, fs = 1, wavelet = "db4", levels = 5,
                   scales = c(3, 4, 5), threshold = NULL) {
  v <- .tf_need(eeg, "eeg", 8L)
  fs <- as.numeric(fs)
  if (fs <= 0) stop("fs must be positive")
  lv <- as.integer(levels)
  sc <- as.integer(scales)
  for (s in sc) {
    if (!(s >= 1L && s <= lv)) {
      stop(sprintf("scale %d is outside 1..levels=%d", s, lv))
    }
  }
  r <- .tf_dwt(v, wavelet, lv)
  fine_to_coarse <- rev(r$details)
  fi <- numeric(length(sc))
  ener <- numeric(length(sc))
  bands <- vector("list", length(sc))
  for (q in seq_along(sc)) {
    s <- sc[q]
    c <- fine_to_coarse[[s]]
    n <- length(c)
    if (n < 2L) {
      stop(sprintf("scale %d has only %d coefficient(s); FI needs >= 2", s, n))
    }
    fi[q] <- .morie_fsum(abs(diff(c))) / n
    ener[q] <- .morie_fsum(c * c)
    bands[[q]] <- c(fs / 2^(s + 1L), fs / 2^s)
  }
  tot <- .morie_fsum(fi)
  det <- if (is.null(threshold)) NULL else tot > as.numeric(threshold)
  list(
    fi = fi, fi_total = tot, scales = sc, bands = bands, energies = ener,
    seizure_detected = det, threshold = threshold,
    wavelet = as.character(wavelet), levels = lv,
    method = paste(
      "Fluctuation intensity of DWT coefficients, Rangayyan &",
      "Krishnan (2024) Sec 8.17 eq (8.132), db4 with five",
      "scales and scales 3-5 selected as specified there"
    )
  )
}

# -- STFT window selection under the time-bandwidth limit, eq (8.10).
#' STFT window selection under the time-bandwidth limit, eq (8.10)
#'
#' Part of the rangayyan_tf implementation; see the file header for the
#' source it follows.
#'
#' @param fs See Usage.
#' @param desired_t_res See Usage.
#' @param desired_f_res See Usage.
#' @return A list with \code{nperseg_time}, \code{nperseg_freq}, \code{nperseg}, \code{achieved_t_res}, \code{achieved_f_res}, \code{tf_product}, \code{heisenberg_bound}, \code{feasible}, \code{method}.
#' @export
StftParam <- function(fs, desired_t_res, desired_f_res) {
  fs <- as.numeric(fs)
  dt <- as.numeric(desired_t_res)
  df <- as.numeric(desired_f_res)
  if (fs <= 0) stop("fs must be positive")
  if (dt <= 0) stop("desired_t_res must be positive")
  if (df <= 0) stop("desired_f_res must be positive")
  m_t <- max(2L, as.integer(trunc(dt * fs)))
  m_f <- max(2L, as.integer(trunc(fs / df + 0.5)))
  feasible <- dt * df >= 1
  m <- if (!feasible) m_f else m_t
  list(
    nperseg_time = m_t, nperseg_freq = m_f, nperseg = m,
    achieved_t_res = m / fs, achieved_f_res = fs / m, tf_product = dt * df,
    heisenberg_bound = 1 / (4 * pi), feasible = feasible,
    method = paste(
      "STFT window selection under the time-bandwidth limit,",
      "Rangayyan & Krishnan (2024) eq (8.10)"
    )
  )
}

# -- STFT spectrogram, eq (8.8); |STFT|^2 per the text after eq (8.9).
#' STFT spectrogram, eq (8.8); |STFT|^2 per the text after eq (8.9)
#'
#' Part of the rangayyan_tf implementation; see the file header for the
#' source it follows.
#'
#' @param x See Usage.
#' @param fs Defaults to \code{1}.
#' @param nperseg Defaults to \code{64}.
#' @param noverlap Defaults to \code{NULL}.
#' @param window Defaults to \code{"hann"}.
#' @return A list with \code{spectrogram}, \code{stft}, \code{times}, \code{freqs}, \code{nperseg}, \code{hop}, \code{window}, \code{n_frames}, \code{total_energy}, \code{peak_freq}, \code{method}.
#' @export
Spectrogram <- function(x, fs = 1, nperseg = 64, noverlap = NULL,
                        window = "hann") {
  v <- .tf_need(x, "x", 2L)
  fs <- as.numeric(fs)
  if (fs <= 0) stop("fs must be positive")
  m <- as.integer(nperseg)
  if (m < 2L) stop("nperseg must be >= 2")
  if (m > length(v)) {
    stop(sprintf("nperseg=%d exceeds the signal length %d", m, length(v)))
  }
  ov <- if (is.null(noverlap)) m %/% 2L else as.integer(noverlap)
  if (!(ov >= 0L && ov < m)) {
    stop(sprintf("noverlap must satisfy 0 <= noverlap < nperseg, got %d", ov))
  }
  hop <- m - ov
  w <- .tf_win(window, m)
  nf <- m %/% 2L + 1L
  freqs <- (seq_len(nf) - 1) * fs / m
  frames <- list()
  times <- numeric(0)
  spec <- list()
  start <- 1L
  while (start + m - 1L <= length(v)) {
    X <- .tf_dft(v[start:(start + m - 1L)] * w)
    frames[[length(frames) + 1L]] <- X
    spec[[length(spec) + 1L]] <- Mod(X[seq_len(nf)])^2
    times <- c(times, ((start - 1L) + (m - 1) / 2) / fs)
    start <- start + hop
  }
  if (!length(frames)) stop("no complete analysis window fits in the signal")
  smat <- do.call(rbind, spec)
  total <- .morie_fsum(vapply(spec, .morie_fsum, numeric(1)))
  colsum <- vapply(seq_len(nf), function(k) .morie_fsum(smat[, k]), numeric(1))
  list(
    spectrogram = smat, stft = frames, times = times, freqs = freqs,
    nperseg = m, hop = hop, window = as.character(window),
    n_frames = length(frames), total_energy = total,
    peak_freq = freqs[which.max(colsum)],
    method = paste(
      "STFT spectrogram, Rangayyan & Krishnan (2024) eq (8.8);",
      "|STFT|^2 per the definition following eq (8.9)"
    )
  )
}

# -- Stationary (undecimated, a-trous) wavelet transform.
#' Stationary (undecimated, a-trous) wavelet transform
#'
#' Part of the rangayyan_tf implementation; see the file header for the
#' source it follows.
#'
#' @param x See Usage.
#' @param wavelet Defaults to \code{"db4"}.
#' @param levels Defaults to \code{3}.
#' @return A list with \code{approx}, \code{details}, \code{levels}, \code{wavelet}, \code{redundancy}, \code{energy_per_level}, \code{method}.
#' @export
Swt <- function(x, wavelet = "db4", levels = 3) {
  v <- .tf_need(x, "x", 4L)
  lv <- as.integer(levels)
  if (lv < 1L) stop("levels must be >= 1")
  if (2^lv > length(v)) {
    stop(sprintf(
      "levels=%d needs a signal of at least %d samples, got %d",
      lv, as.integer(2^lv), length(v)
    ))
  }
  r <- .tf_swt(v, wavelet, lv)
  list(
    approx = r$approx, details = r$details, levels = lv,
    wavelet = as.character(wavelet), redundancy = lv + 1L,
    energy_per_level = vapply(
      r$details, function(c) .morie_fsum(c * c),
      numeric(1)
    ),
    method = paste(
      "Stationary (undecimated, a-trous) wavelet transform,",
      "Nason & Silverman (1995); shift variance of the",
      "decimated DWT noted by Rangayyan & Krishnan (2024)",
      "after eq (8.113)"
    )
  )
}

# -- SWT (cycle-spinning) denoising, eqs (8.103)-(8.104) + Coifman & Donoho.
#' SWT (cycle-spinning) denoising, eqs (8.103)-(8.104) + Coifman &
#' Donoho
#'
#' Part of the rangayyan_tf implementation; see the file header for the
#' source it follows.
#'
#' @param x See Usage.
#' @param wavelet Defaults to \code{"db4"}.
#' @param levels Defaults to \code{3}.
#' @param threshold Defaults to \code{NULL}.
#' @param threshold_type Defaults to \code{"soft"}.
#' @return A list with \code{denoised}, \code{threshold}, \code{sigma}, \code{n_zeroed}, \code{n_coeffs}, \code{n_shifts}, \code{residual_energy}, \code{levels}, \code{wavelet}, \code{threshold_type}, \code{method}.
#' @export
SwtDen <- function(x, wavelet = "db4", levels = 3, threshold = NULL,
                   threshold_type = "soft") {
  v <- .tf_need(x, "x", 4L)
  tt <- tolower(trimws(as.character(threshold_type)))
  if (!(tt %in% c("soft", "hard"))) {
    stop(sprintf("threshold_type must be 'soft' or 'hard', got '%s'", tt))
  }
  lv <- as.integer(levels)
  if (lv < 1L) stop("levels must be >= 1")
  if (2^lv > length(v)) {
    stop(sprintf("levels=%d needs at least %d samples", lv, as.integer(2^lv)))
  }
  n <- length(v)
  nshift <- as.integer(2^lv)
  r0 <- .tf_dwt(v, wavelet, lv)
  med <- sort(abs(r0$details[[length(r0$details)]]))
  sigma <- med[length(med) %/% 2L + 1L] / 0.6745
  if (is.null(threshold)) {
    T <- sigma * sqrt(2 * log(n))
  } else {
    T <- as.numeric(threshold)
    if (T < 0) stop("threshold must be non-negative")
  }
  shrink <- function(w) {
    ifelse(abs(w) < T, 0,
      if (tt == "hard") w else sign(w) * (abs(w) - T)
    )
  }
  acc <- numeric(n)
  zeroed <- 0L
  ncoef <- 0L
  for (sh in 0:(nshift - 1L)) {
    rolled <- v[((seq_len(n) - 1L + sh) %% n) + 1L]
    r <- .tf_dwt(rolled, wavelet, lv)
    nd <- lapply(r$details, function(c) {
      row <- shrink(c)
      zeroed <<- zeroed + sum(row == 0)
      ncoef <<- ncoef + length(row)
      row
    })
    rec <- .tf_idwt(r$approx, nd, r$lengths, wavelet)[seq_len(n)]
    idx <- ((seq_len(n) - 1L + sh) %% n) + 1L
    acc[idx] <- acc[idx] + rec
  }
  cur <- acc / nshift
  list(
    denoised = cur, threshold = T, sigma = sigma, n_zeroed = zeroed,
    n_coeffs = ncoef, n_shifts = nshift,
    residual_energy = .morie_fsum((v - cur)^2), levels = lv,
    wavelet = as.character(wavelet), threshold_type = tt,
    method = paste(
      "SWT (cycle-spinning) denoising: thresholds of",
      "Rangayyan & Krishnan (2024) eqs (8.103)-(8.104) applied",
      "to the Nason & Silverman (1995) undecimated transform;",
      "translation invariance per Coifman & Donoho (1995)"
    )
  )
}

# -- Variational mode decomposition, Dragomiretskiy & Zosso (2014).
#' Variational mode decomposition, Dragomiretskiy & Zosso (2014)
#'
#' Part of the rangayyan_tf implementation; see the file header for the
#' source it follows.
#'
#' @param x See Usage.
#' @param K Defaults to \code{3}.
#' @param alpha Defaults to \code{2000}.
#' @param tau Defaults to \code{0}.
#' @param init Defaults to \code{"uniform"}.
#' @param tol Defaults to \code{1e-07}.
#' @param max_iter Defaults to \code{300}.
#' @return A list with \code{modes}, \code{center_freqs}, \code{K}, \code{alpha}, \code{tau}, \code{iterations}, \code{converged}, \code{reconstruction_error}, \code{residual_energy}, \code{method}.
#' @export
VModes <- function(x, K = 3, alpha = 2000, tau = 0, init = "uniform",
                   tol = 1e-7, max_iter = 300) {
  v <- .tf_need(x, "x", 8L)
  k <- as.integer(K)
  if (k < 1L) stop("K must be >= 1")
  al <- as.numeric(alpha)
  if (al <= 0) stop("alpha must be positive")
  ta <- as.numeric(tau)
  if (ta < 0) stop("tau must be non-negative")
  mi <- as.integer(max_iter)
  if (mi < 1L) stop("max_iter must be >= 1")
  ini <- tolower(trimws(as.character(init)))
  if (!(ini %in% c("uniform", "zero"))) {
    stop(sprintf("init must be 'uniform' or 'zero', got '%s'", ini))
  }
  n <- length(v)
  F <- .tf_dft(v)
  half <- n %/% 2L + 1L
  fh <- F[seq_len(half)]
  om <- (seq_len(half) - 1) / n
  wk <- if (ini == "uniform") 0.5 * ((seq_len(k) - 1) + 0.5) / k else numeric(k)
  uk <- lapply(seq_len(k), function(j) complex(length.out = half))
  lam <- complex(length.out = half)
  it <- 0L
  conv <- FALSE
  for (itr in seq_len(mi)) {
    it <- itr
    change <- 0
    for (j in seq_len(k)) {
      others <- complex(length.out = half)
      for (q in seq_len(k)) if (q != j) others <- others + uk[[q]]
      den <- 1 + 2 * al * (om - wk[j])^2
      new <- (fh - others + lam / 2) / den
      num2 <- .morie_fsum(om * Mod(new)^2)
      den2 <- .morie_fsum(Mod(new)^2)
      if (den2 > 0) wk[j] <- num2 / den2
      prev <- .morie_fsum(Mod(uk[[j]])^2)
      change <- change + .morie_fsum(Mod(new - uk[[j]])^2) /
        (if (prev > 0) prev else 1)
      uk[[j]] <- new
    }
    if (ta > 0) {
      tot <- complex(length.out = half)
      for (q in seq_len(k)) tot <- tot + uk[[q]]
      lam <- lam + ta * (fh - tot)
    }
    if (change < as.numeric(tol)) {
      conv <- TRUE
      break
    }
  }
  modes <- vector("list", k)
  for (j in seq_len(k)) {
    full <- complex(length.out = n)
    full[seq_len(half)] <- uk[[j]]
    top <- (n + 1L) %/% 2L - 1L
    if (top >= 1L) {
      for (i in seq_len(top)) full[n - i + 1L] <- Conj(uk[[j]][i + 1L])
    }
    modes[[j]] <- Re(.tf_idft(full))
  }
  ord <- order(wk)
  modes <- modes[ord]
  wk <- wk[ord]
  tot <- vapply(seq_len(n), function(i) {
    .morie_fsum(vapply(modes, function(m) m[i], numeric(1)))
  }, numeric(1))
  list(
    modes = modes, center_freqs = wk, K = k, alpha = al, tau = ta,
    iterations = it, converged = conv,
    reconstruction_error = max(abs(tot - v)),
    residual_energy = .morie_fsum((tot - v)^2),
    method = paste(
      "Variational mode decomposition, Dragomiretskiy & Zosso",
      "(2014) IEEE TSP 62(3):531-544 eqs (13), (15), (16) --",
      "not covered by Rangayyan & Krishnan (2024)"
    )
  )
}

# -- CWT ridge detection of transient structures, Sec 8.8.
#' CWT ridge detection of transient structures, Sec 8.8
#'
#' Part of the rangayyan_tf implementation; see the file header for the
#' source it follows.
#'
#' @param x See Usage.
#' @param fs Defaults to \code{1}.
#' @param scales Defaults to \code{NULL}.
#' @param wavelet Defaults to \code{"mexh"}.
#' @param w0 Defaults to \code{5}.
#' @param min_prominence Defaults to \code{0.1}.
#' @return A list with \code{structures}, \code{n_structures}, \code{scalogram}, \code{scales}, \code{times}, \code{min_prominence}, \code{method}.
#' @export
CwtRidge <- function(x, fs = 1, scales = NULL, wavelet = "mexh", w0 = 5,
                     min_prominence = 0.1) {
  p <- as.numeric(min_prominence)
  if (!(p > 0 && p <= 1)) stop("min_prominence must lie in (0, 1]")
  r <- Scalogram(x, fs = fs, scales = scales, wavelet = wavelet, w0 = w0)
  sg <- r$scalogram
  ns <- nrow(sg)
  n <- ncol(sg)
  gmax <- max(sg)
  if (gmax <= 0) stop("the signal has no wavelet energy at any scale")
  found <- list()
  for (si in seq_len(ns)) {
    row <- sg[si, ]
    if (n >= 3L) {
      for (i in 2L:(n - 1L)) {
        if (row[i] > row[i - 1L] && row[i] >= row[i + 1L] && row[i] >= p * gmax) {
          if (si > 1L && sg[si - 1L, i] > row[i]) next
          if (si < ns && sg[si + 1L, i] > row[i]) next
          found[[length(found) + 1L]] <- list(
            sample = i, time = r$times[i],
            scale = r$scales[si],
            freq = r$freqs[si], energy = row[i]
          )
        }
      }
    }
  }
  if (length(found)) {
    found <- found[order(-vapply(found, function(d) d$energy, numeric(1)))]
  }
  list(
    structures = found, n_structures = length(found), scalogram = sg,
    scales = r$scales, times = r$times, min_prominence = p,
    method = paste(
      "CWT ridge detection of transient structures,",
      "Rangayyan & Krishnan (2024) Sec 8.8 (eqs 8.107, 8.115,",
      "8.116); ridge rule is local-maximum-with-prominence,",
      "not specified by the book"
    )
  )
}

# -- Scale-by-scale wavelet cross-correlation, Whitcher et al. (2000).
#' Scale-by-scale wavelet cross-correlation, Whitcher et al. (2000)
#'
#' Part of the rangayyan_tf implementation; see the file header for the
#' source it follows.
#'
#' @param x See Usage.
#' @param y See Usage.
#' @param wavelet Defaults to \code{"db4"}.
#' @param levels Defaults to \code{3}.
#' @param max_lag Defaults to \code{0}.
#' @return A list with \code{correlations}, \code{best_lags}, \code{covariances}, \code{scales}, \code{overall_correlation}, \code{levels}, \code{max_lag}, \code{wavelet}, \code{method}.
#' @export
WtXcor <- function(x, y, wavelet = "db4", levels = 3, max_lag = 0) {
  a <- .tf_need(x, "x", 4L)
  b <- .tf_need(y, "y", 4L)
  if (length(a) != length(b)) {
    stop(sprintf(
      "x and y must have the same length, got %d and %d",
      length(a), length(b)
    ))
  }
  lv <- as.integer(levels)
  if (lv < 1L) stop("levels must be >= 1")
  ml <- as.integer(max_lag)
  n <- length(a)
  if (ml < 0L || ml >= n) {
    stop(sprintf("max_lag must satisfy 0 <= max_lag < %d, got %d", n, ml))
  }
  if (2^lv > n) {
    stop(sprintf("levels=%d needs at least %d samples", lv, as.integer(2^lv)))
  }
  da <- .tf_swt(a, wavelet, lv)$details
  db <- .tf_swt(b, wavelet, lv)$details

  corr <- function(u, w, lag) {
    m <- length(u)
    idx <- seq_len(m)
    idx <- idx[idx + lag >= 1L & idx + lag <= m]
    if (length(idx) < 2L) {
      return(c(0, 0))
    }
    mu <- .morie_fsum(u[idx]) / length(idx)
    mw <- .morie_fsum(w[idx + lag]) / length(idx)
    cov <- .morie_fsum((u[idx] - mu) * (w[idx + lag] - mw)) / length(idx)
    su <- sqrt(.morie_fsum((u[idx] - mu)^2) / length(idx))
    sw <- sqrt(.morie_fsum((w[idx + lag] - mw)^2) / length(idx))
    c(if (su > 0 && sw > 0) cov / (su * sw) else 0, cov)
  }

  cors <- numeric(lv)
  lags <- integer(lv)
  covs <- numeric(lv)
  for (j in seq_len(lv)) {
    best <- -2
    bl <- 0L
    bc <- 0
    for (lag in (-ml):ml) {
      cv <- corr(da[[j]], db[[j]], lag)
      if (abs(cv[1L]) > abs(best) || best == -2) {
        best <- cv[1L]
        bl <- lag
        bc <- cv[2L]
      }
    }
    cors[j] <- best
    lags[j] <- bl
    covs[j] <- bc
  }
  mu <- .morie_fsum(a) / n
  mw <- .morie_fsum(b) / n
  sa <- sqrt(.morie_fsum((a - mu)^2) / n)
  sb <- sqrt(.morie_fsum((b - mw)^2) / n)
  ov <- if (sa > 0 && sb > 0) {
    .morie_fsum((a - mu) * (b - mw)) / n / (sa * sb)
  } else {
    0
  }
  list(
    correlations = cors, best_lags = lags, covariances = covs,
    scales = 2^(seq_len(lv) - 1L), overall_correlation = ov, levels = lv,
    max_lag = ml, wavelet = as.character(wavelet),
    method = paste(
      "Scale-by-scale wavelet cross-correlation, Whitcher,",
      "Guttorp & Percival (2000) JGR 105(D11), on the",
      "undecimated transform of Nason & Silverman (1995);",
      "wavelet basis per Rangayyan & Krishnan (2024) eq (8.113)"
    )
  )
}

# -- Wigner-Ville distribution, eq (8.123).
#' Wigner-Ville distribution, eq (8.123)
#'
#' Part of the rangayyan_tf implementation; see the file header for the
#' source it follows.
#'
#' @param x See Usage.
#' @param fs Defaults to \code{1}.
#' @param nfreq Defaults to \code{NULL}.
#' @return A list with \code{tfd}, \code{times}, \code{freqs}, \code{peak_freq}, \code{total_energy}, \code{method}.
#' @export
WvDist <- function(x, fs = 1, nfreq = NULL) {
  v <- .tf_need(x, "x", 4L)
  fs <- as.numeric(fs)
  if (fs <= 0) stop("fs must be positive")
  nf <- as.integer(if (is.null(nfreq) || nfreq == 0) length(v) else nfreq)
  if (nf < 2L) stop("nfreq must be >= 2")
  r <- .tf_wvd(v, fs, nf)
  col <- vapply(seq_len(nf), function(k) .morie_fsum(r$tfd[, k]), numeric(1))
  list(
    tfd = r$tfd, times = (seq_along(v) - 1) / fs, freqs = r$freqs,
    peak_freq = r$freqs[which.max(col)],
    total_energy = .morie_fsum(vapply(
      seq_len(nrow(r$tfd)),
      function(i) .morie_fsum(r$tfd[i, ]),
      numeric(1)
    )),
    method = paste(
      "Wigner-Ville distribution, Rangayyan & Krishnan (2024)",
      "eq (8.123), analytic-signal (Claasen-Mecklenbrauker) form"
    )
  )
}

# -- Wavelet subband energy, Sec 8.15.
#' Wavelet subband energy, Sec 8.15
#'
#' Part of the rangayyan_tf implementation; see the file header for the
#' source it follows.
#'
#' @param x See Usage.
#' @param wavelet Defaults to \code{"db4"}.
#' @param levels Defaults to \code{3}.
#' @return A list with \code{energies}, \code{relative}, \code{labels}, \code{total_energy}, \code{input_energy}, \code{energy_balance}, \code{dominant_band}, \code{levels}, \code{wavelet}, \code{method}.
#' @export
WtEnergy <- function(x, wavelet = "db4", levels = 3) {
  v <- .tf_need(x, "x", 2L)
  lv <- as.integer(levels)
  r <- .tf_dwt(v, wavelet, lv)
  bands <- c(list(r$approx), r$details)
  labels <- c(sprintf("A%d", lv), sprintf("D%d", lv - (seq_len(lv) - 1L)))
  ener <- vapply(bands, function(c) .morie_fsum(c * c), numeric(1))
  tot <- .morie_fsum(ener)
  ein <- .morie_fsum(v * v)
  list(
    energies = ener, relative = if (tot > 0) ener / tot else rep(0, length(ener)),
    labels = labels, total_energy = tot, input_energy = ein,
    energy_balance = abs(tot - ein),
    dominant_band = labels[which.max(ener)], levels = lv,
    wavelet = as.character(wavelet),
    method = paste(
      "Wavelet subband energy, Rangayyan & Krishnan (2024)",
      "Sec 8.15 (Ex = Es1 + Es2 + ... + EsN) over the",
      "eq (8.111)-(8.113) orthonormal DWT"
    )
  )
}

# -- Per-subband sample moments of the DWT coefficients.
#' Per-subband sample moments of the DWT coefficients
#'
#' Part of the rangayyan_tf implementation; see the file header for the
#' source it follows.
#'
#' @param x See Usage.
#' @param wavelet Defaults to \code{"db4"}.
#' @param levels Defaults to \code{3}.
#' @return A list with \code{moments}, \code{labels}, \code{levels}, \code{wavelet}, \code{method}.
#' @export
WtMoment <- function(x, wavelet = "db4", levels = 3) {
  v <- .tf_need(x, "x", 2L)
  lv <- as.integer(levels)
  r <- .tf_dwt(v, wavelet, lv)
  bands <- c(list(r$approx), r$details)
  labels <- c(sprintf("A%d", lv), sprintf("D%d", lv - (seq_len(lv) - 1L)))
  out <- vector("list", length(bands))
  for (q in seq_along(bands)) {
    c <- bands[[q]]
    n <- length(c)
    mu <- .morie_fsum(c) / n
    var <- .morie_fsum((c - mu)^2) / n
    sdv <- sqrt(var)
    sk <- NULL
    ku <- NULL
    if (n >= 3L && sdv > 0) {
      sk <- .morie_fsum(((c - mu) / sdv)^3) / n
      ku <- .morie_fsum(((c - mu) / sdv)^4) / n
    }
    out[[q]] <- list(
      label = labels[q], n = n, mean = mu, variance = var,
      energy = .morie_fsum(c * c), skewness = sk, kurtosis = ku
    )
  }
  list(
    moments = out, labels = labels, levels = lv,
    wavelet = as.character(wavelet),
    method = paste(
      "Per-subband sample moments of the Rangayyan & Krishnan",
      "(2024) eq (8.111)-(8.113) DWT coefficients; band energy",
      "per Sec 8.15"
    )
  )
}

# -- Wavelet packet decomposition (full binary tree, natural order).
#' Wavelet packet decomposition (full binary tree, natural order)
#'
#' Part of the rangayyan_tf implementation; see the file header for the
#' source it follows.
#'
#' @param x See Usage.
#' @param wavelet Defaults to \code{"db4"}.
#' @param levels Defaults to \code{3}.
#' @return A list with \code{nodes}, \code{leaves}, \code{n_leaves}, \code{energy_per_leaf}, \code{dominant_leaf}, \code{entropy}, \code{levels}, \code{wavelet}, \code{method}.
#' @export
Wpt <- function(x, wavelet = "db4", levels = 3) {
  v <- .tf_need(x, "x", 4L)
  lv <- as.integer(levels)
  if (lv < 1L) stop("levels must be >= 1")
  f <- .tf_filters(wavelet)
  if (length(v) < length(f$h) * (2^(lv - 1L))) {
    stop(sprintf(paste0(
      "signal of length %d is too short for %d packet levels ",
      "with a length-%d filter"
    ), length(v), lv, length(f$h)))
  }
  nodes <- list(list(v))
  names(nodes) <- "0"
  for (lev in seq_len(lv)) {
    cur <- list()
    for (node in nodes[[as.character(lev - 1L)]]) {
      st <- .tf_dwtstep(node, f$h, f$g)
      cur[[length(cur) + 1L]] <- st$lo
      cur[[length(cur) + 1L]] <- st$hi
    }
    nodes[[as.character(lev)]] <- cur
  }
  leaves <- nodes[[as.character(lv)]]
  ener <- vapply(leaves, function(c) .morie_fsum(c * c), numeric(1))
  tot <- .morie_fsum(ener)
  ent <- 0
  if (tot > 0) {
    p <- ener / tot
    ent <- -.morie_fsum(p[p > 0] * log(p[p > 0]))
  }
  list(
    nodes = nodes, leaves = leaves, n_leaves = length(leaves),
    energy_per_leaf = ener, dominant_leaf = which.max(ener), entropy = ent,
    levels = lv, wavelet = as.character(wavelet),
    method = paste(
      "Wavelet packet decomposition (full binary tree, natural",
      "order), Rangayyan & Krishnan (2024) Sec 8.8.1 and its",
      "reference [81], Wickerhauser (1994)"
    )
  )
}

# -- Wavelet shrinkage, eqs (8.103)-(8.105) with the universal threshold.
#' Wavelet shrinkage, eqs (8.103)-(8.105) with the universal threshold
#'
#' Part of the rangayyan_tf implementation; see the file header for the
#' source it follows.
#'
#' @param x See Usage.
#' @param wavelet Defaults to \code{"db4"}.
#' @param levels Defaults to \code{3}.
#' @param threshold_type Defaults to \code{"soft"}.
#' @param threshold Defaults to \code{NULL}.
#' @return A list with \code{denoised}, \code{threshold}, \code{sigma}, \code{n_zeroed}, \code{n_coeffs}, \code{sparsity}, \code{noise_removed}, \code{threshold_type}, \code{wavelet}, \code{method}.
#' @export
WtThresh <- function(x, wavelet = "db4", levels = 3, threshold_type = "soft",
                     threshold = NULL) {
  v <- .tf_need(x, "x", 4L)
  tt <- tolower(trimws(as.character(threshold_type)))
  if (!(tt %in% c("soft", "hard"))) {
    stop(sprintf("threshold_type must be 'soft' or 'hard', got '%s'", tt))
  }
  r <- .tf_dwt(v, wavelet, as.integer(levels))
  finest <- r$details[[length(r$details)]]
  med <- sort(abs(finest))
  m <- if (length(med)) med[length(med) %/% 2L + 1L] else 0
  sigma <- m / 0.6745
  if (is.null(threshold)) {
    T <- if (length(v) > 1L) sigma * sqrt(2 * log(length(v))) else 0
  } else {
    T <- as.numeric(threshold)
    if (T < 0) stop("threshold must be non-negative")
  }
  zeroed <- 0L
  newd <- lapply(r$details, function(c) {
    small <- abs(c) < T
    zeroed <<- zeroed + sum(small)
    row <- if (tt == "hard") c else sign(c) * (abs(c) - T)
    row[small] <- 0
    row
  })
  den <- .tf_idwt(r$approx, newd, r$lengths, wavelet)[seq_along(v)]
  ncoef <- sum(vapply(r$details, length, integer(1)))
  list(
    denoised = den, threshold = T, sigma = sigma, n_zeroed = zeroed,
    n_coeffs = ncoef, sparsity = if (ncoef) zeroed / ncoef else 0,
    noise_removed = .morie_fsum((v - den)^2), threshold_type = tt,
    wavelet = as.character(wavelet),
    method = paste(
      "Wavelet shrinkage, Rangayyan & Krishnan (2024) eqs",
      "(8.103)-(8.105); universal threshold from Donoho &",
      "Johnstone (1994) when T is not supplied"
    )
  )
}

# -- Unbiased wavelet variance by scale, Percival (1995).
#' Unbiased wavelet variance by scale, Percival (1995)
#'
#' Part of the rangayyan_tf implementation; see the file header for the
#' source it follows.
#'
#' @param x See Usage.
#' @param wavelet Defaults to \code{"db1"}.
#' @param levels Defaults to \code{3}.
#' @return A list with \code{variances}, \code{scales}, \code{n_used}, \code{total_variance}, \code{sample_variance}, \code{dominant_scale}, \code{is_allan}, \code{wavelet}, \code{method}.
#' @export
WtVar <- function(x, wavelet = "db1", levels = 3) {
  v <- .tf_need(x, "x", 4L)
  lv <- as.integer(levels)
  if (lv < 1L) stop("levels must be >= 1")
  if (2^lv > length(v)) {
    stop(sprintf("levels=%d needs at least %d samples", lv, as.integer(2^lv)))
  }
  L <- length(.tf_filters(wavelet)$h)
  n <- length(v)
  det <- .tf_swt(v, wavelet, lv)$details
  variances <- numeric(lv)
  used <- integer(lv)
  for (j in seq_len(lv)) {
    span <- (L - 1L) * (2^(j - 1L)) # 0-based level j-1 in Python
    keep <- if (span < n) det[[j]][(span + 1L):n] else numeric(0)
    if (!length(keep)) {
      stop(sprintf(
        paste0(
          "level %d has no interior coefficients for a signal ",
          "of length %d; reduce levels or use a shorter filter"
        ),
        j, n
      ))
    }
    variances[j] <- .morie_fsum(keep * keep) / (2^j * length(keep))
    used[j] <- length(keep)
  }
  mu <- .morie_fsum(v) / n
  list(
    variances = variances, scales = 2^(seq_len(lv) - 1L), n_used = used,
    total_variance = .morie_fsum(variances),
    sample_variance = .morie_fsum((v - mu)^2) / n,
    dominant_scale = 2^(which.max(variances) - 1L),
    is_allan = .tf_dbname(wavelet) == 1L, wavelet = as.character(wavelet),
    method = paste(
      "Unbiased wavelet variance by scale, Percival (1995)",
      "Biometrika 82(3):619-631, on the Nason & Silverman",
      "(1995) undecimated transform; db1 gives the Allan",
      "variance of Allan (1966)"
    )
  )
}

# ---------------------------------------------------------------------------
#  Chapter 4.8: the wavelet-plus-echo family, eqs (4.74)-(4.85).
#  Here `n` is the BOOK'S sample-index variable and stays 0-based.
# ---------------------------------------------------------------------------

.tf_echo_idx <- function(n, default = NULL) {
  if (is.null(n)) {
    return(default)
  }
  if (length(n) == 1L) {
    k <- as.integer(n)
    if (k < 1L) stop("n must be a positive length or a sequence of indices")
    return(seq_len(k) - 1L)
  }
  idx <- as.integer(n)
  if (!length(idx)) stop("n must not be empty")
  idx
}

# -- Two-impulse echo excitation, eq (4.74).
#' Two-impulse echo excitation, eq (4.74)
#'
#' Part of the rangayyan_tf implementation; see the file header for the
#' source it follows.
#'
#' @param a See Usage.
#' @param n_0 See Usage.
#' @param n See Usage.
#' @return A list with \code{x}, \code{n}, \code{a}, \code{n_0}, \code{method}.
#' @export
EchoImp <- function(a, n_0, n) {
  a <- as.numeric(a)
  n0 <- as.integer(n_0)
  if (n0 <= 0L) {
    stop(sprintf(
      "n_0 must be a positive delay in samples, got %s",
      as.character(n_0)
    ))
  }
  idx <- .tf_echo_idx(n)
  x <- as.numeric(idx == 0L) + a * as.numeric(idx == n0)
  list(
    x = x, n = idx, a = a, n_0 = n0,
    method = paste(
      "Two-impulse echo excitation, Rangayyan & Krishnan",
      "(2024) eq (4.74)"
    )
  )
}

# -- Wavelet plus echo in the time domain, eq (4.75).
#' Wavelet plus echo in the time domain, eq (4.75)
#'
#' Part of the rangayyan_tf implementation; see the file header for the
#' source it follows.
#'
#' @param h See Usage.
#' @param a See Usage.
#' @param n_0 See Usage.
#' @param n Defaults to \code{NULL}.
#' @return A list with \code{y}, \code{n}, \code{h}, \code{a}, \code{n_0}, \code{echo_visible}, \code{method}.
#' @export
EchoSig <- function(h, a, n_0, n = NULL) {
  hh <- as.numeric(h)
  if (!length(hh)) stop("h must contain at least one sample")
  a <- as.numeric(a)
  n0 <- as.integer(n_0)
  if (n0 <= 0L) {
    stop(sprintf(
      "n_0 must be a positive delay in samples, got %s",
      as.character(n_0)
    ))
  }
  idx <- .tf_echo_idx(n, seq_len(length(hh) + n0) - 1L)
  hv <- function(i) ifelse(i >= 0L & i < length(hh), hh[pmin(pmax(i, 0L), length(hh) - 1L) + 1L], 0)
  y <- hv(idx) + a * hv(idx - n0)
  list(
    y = y, n = idx, h = hh, a = a, n_0 = n0,
    echo_visible = n0 >= length(hh),
    method = paste(
      "Wavelet plus echo in the time domain, Rangayyan &",
      "Krishnan (2024) eq (4.75)"
    )
  )
}

# -- z-transform of a wavelet with an echo, eq (4.76).
#' Z-transform of a wavelet with an echo, eq (4.76)
#'
#' Part of the rangayyan_tf implementation; see the file header for the
#' source it follows.
#'
#' @param a See Usage.
#' @param n_0 See Usage.
#' @param z See Usage.
#' @param H Defaults to \code{NULL}.
#' @return A list with \code{Y}, \code{echo_factor}, \code{z}, \code{H}, \code{a}, \code{n_0}, \code{method}.
#' @export
EchoZ <- function(a, n_0, z, H = NULL) {
  a <- as.numeric(a)
  n0 <- as.integer(n_0)
  if (n0 <= 0L) {
    stop(sprintf(
      "n_0 must be a positive delay in samples, got %s",
      as.character(n_0)
    ))
  }
  zs <- as.complex(z)
  if (!length(zs)) stop("z must not be empty")
  if (any(zs == 0)) {
    stop("z = 0 is outside the region of convergence of z^(-n_0)")
  }
  hs <- if (is.null(H)) {
    rep(as.complex(1), length(zs))
  } else if (length(H) == 1L) {
    rep(as.complex(H), length(zs))
  } else {
    as.complex(H)
  }
  if (length(hs) != length(zs)) {
    stop(sprintf("H has length %d but z has length %d", length(hs), length(zs)))
  }
  fac <- 1 + a * zs^(-n0)
  list(
    Y = fac * hs, echo_factor = fac, z = zs, H = hs, a = a, n_0 = n0,
    method = paste(
      "z-transform of a wavelet with an echo, Rangayyan &",
      "Krishnan (2024) eq (4.76)"
    )
  )
}

# -- Fourier spectrum of a wavelet with an echo, eq (4.77).
#' Fourier spectrum of a wavelet with an echo, eq (4.77)
#'
#' Part of the rangayyan_tf implementation; see the file header for the
#' source it follows.
#'
#' @param a See Usage.
#' @param n_0 See Usage.
#' @param omega See Usage.
#' @param H Defaults to \code{NULL}.
#' @return A list with \code{Y}, \code{echo_factor}, \code{magnitude}, \code{phase}, \code{omega}, \code{ripple_period}, \code{a}, \code{n_0}, \code{method}.
#' @export
EchoSpec <- function(a, n_0, omega, H = NULL) {
  a <- as.numeric(a)
  n0 <- as.integer(n_0)
  if (n0 <= 0L) {
    stop(sprintf(
      "n_0 must be a positive delay in samples, got %s",
      as.character(n_0)
    ))
  }
  ws <- as.numeric(omega)
  if (!length(ws)) stop("omega must not be empty")
  hs <- if (is.null(H)) {
    rep(as.complex(1), length(ws))
  } else if (length(H) == 1L) {
    rep(as.complex(H), length(ws))
  } else {
    as.complex(H)
  }
  if (length(hs) != length(ws)) {
    stop(sprintf(
      "H has length %d but omega has length %d",
      length(hs), length(ws)
    ))
  }
  fac <- 1 + a * exp(complex(imaginary = -ws * n0))
  Y <- fac * hs
  list(
    Y = Y, echo_factor = fac, magnitude = Mod(Y),
    phase = atan2(Im(Y), Re(Y)), omega = ws, ripple_period = 2 * pi / n0,
    a = a, n_0 = n0,
    method = paste(
      "Fourier spectrum of a wavelet with an echo, Rangayyan",
      "& Krishnan (2024) eq (4.77)"
    )
  )
}

# -- Complex log spectrum of a wavelet with an echo, eqs (4.78)-(4.79).
#' Complex log spectrum of a wavelet with an echo, eqs (4.78)-(4.79)
#'
#' Part of the rangayyan_tf implementation; see the file header for the
#' source it follows.
#'
#' @param a See Usage.
#' @param n_0 See Usage.
#' @param omega See Usage.
#' @param H_hat Defaults to \code{NULL}.
#' @param n_terms Defaults to \code{NULL}.
#' @return A list with \code{Y_hat}, \code{echo_log}, \code{series}, \code{series_valid}, \code{series_error}, \code{omega}, \code{a}, \code{n_0}, \code{n_terms}, \code{method}.
#' @export
EchoLogSp <- function(a, n_0, omega, H_hat = NULL, n_terms = NULL) {
  a <- as.numeric(a)
  n0 <- as.integer(n_0)
  if (n0 <= 0L) {
    stop(sprintf(
      "n_0 must be a positive delay in samples, got %s",
      as.character(n_0)
    ))
  }
  ws <- as.numeric(omega)
  if (!length(ws)) stop("omega must not be empty")
  nt <- if (is.null(n_terms)) 10L else as.integer(n_terms)
  if (nt < 1L) stop("n_terms must be >= 1")
  hs <- if (is.null(H_hat)) {
    rep(as.complex(0), length(ws))
  } else if (length(H_hat) == 1L) {
    rep(as.complex(H_hat), length(ws))
  } else {
    as.complex(H_hat)
  }
  if (length(hs) != length(ws)) {
    stop(sprintf(
      "H_hat has length %d but omega has length %d",
      length(hs), length(ws)
    ))
  }
  t <- 1 + a * exp(complex(imaginary = -ws * n0))
  if (any(Mod(t) < 1e-12)) {
    stop(sprintf(
      paste0(
        "1 + a exp(-j w n_0) vanishes at omega=%s; the complex ",
        "logarithm of eq (4.78) is undefined there"
      ),
      ws[which(Mod(t) < 1e-12)[1L]]
    ))
  }
  elog <- log(t)
  Yh <- hs + elog
  valid <- abs(a) < 1
  ser <- NULL
  err <- NULL
  if (valid) {
    ser <- complex(length.out = length(ws))
    for (i in seq_along(ws)) {
      acc <- as.complex(0)
      for (k in seq_len(nt)) {
        acc <- acc + ((-1)^(k + 1)) * (a^k) / k *
          exp(complex(imaginary = -k * ws[i] * n0))
      }
      ser[i] <- hs[i] + acc
    }
    err <- max(Mod(Yh - ser))
  }
  list(
    Y_hat = Yh, echo_log = elog, series = ser, series_valid = valid,
    series_error = err, omega = ws, a = a, n_0 = n0, n_terms = nt,
    method = paste(
      "Complex log spectrum of a wavelet with an echo,",
      "Rangayyan & Krishnan (2024) eq (4.78), with the",
      "eq (4.79) power-series expansion"
    )
  )
}

# -- Complex cepstrum of a wavelet with an echo, eq (4.80).
#' Complex cepstrum of a wavelet with an echo, eq (4.80)
#'
#' Part of the rangayyan_tf implementation; see the file header for the
#' source it follows.
#'
#' @param h_hat See Usage.
#' @param a See Usage.
#' @param n_0 See Usage.
#' @param n Defaults to \code{NULL}.
#' @param n_terms Defaults to \code{NULL}.
#' @return A list with \code{y_hat}, \code{n}, \code{impulses}, \code{n_impulses}, \code{echo_delay}, \code{a}, \code{method}.
#' @export
EchoCep <- function(h_hat, a, n_0, n = NULL, n_terms = NULL) {
  hh <- as.numeric(h_hat)
  a <- as.numeric(a)
  if (abs(a) >= 1) {
    stop(sprintf(paste0(
      "|a| = %s >= 1; the power series of eq (4.79) that ",
      "gives eq (4.80) requires a < 1"
    ), abs(a)))
  }
  n0 <- as.integer(n_0)
  if (n0 <= 0L) {
    stop(sprintf(
      "n_0 must be a positive delay in samples, got %s",
      as.character(n_0)
    ))
  }
  if (is.null(n)) {
    nt0 <- if (is.null(n_terms)) 4L else as.integer(n_terms)
    idx <- seq_len(max(length(hh), n0 * nt0 + 1L)) - 1L
  } else {
    idx <- .tf_echo_idx(n)
  }
  top <- max(idx)
  nt <- if (is.null(n_terms)) top %/% n0 else as.integer(n_terms)
  if (nt < 1L) stop("n_terms must be >= 1")
  k <- seq_len(nt)
  amp <- ((-1)^(k + 1)) * (a^k) / k
  pos <- k * n0
  y <- ifelse(idx >= 0L & idx < length(hh),
    hh[pmin(pmax(idx, 0L), max(length(hh) - 1L, 0L)) + 1L], 0
  )
  hit <- match(idx, pos)
  y <- y + ifelse(is.na(hit), 0, amp[ifelse(is.na(hit), 1L, hit)])
  list(
    y_hat = y, n = idx,
    impulses = lapply(seq_len(nt), function(i) c(pos[i], amp[i])),
    n_impulses = nt, echo_delay = n0, a = a,
    method = paste(
      "Complex cepstrum of a wavelet with an echo, Rangayyan",
      "& Krishnan (2024) eq (4.80)"
    )
  )
}

# -- Power spectrum of a wavelet with an echo, eq (4.84).
#' Power spectrum of a wavelet with an echo, eq (4.84)
#'
#' Part of the rangayyan_tf implementation; see the file header for the
#' source it follows.
#'
#' @param H See Usage.
#' @param a See Usage.
#' @param n_0 See Usage.
#' @param z See Usage.
#' @return A list with \code{power}, \code{wavelet_power}, \code{echo_power}, \code{z}, \code{a}, \code{n_0}, \code{method}.
#' @export
EchoPsd <- function(H, a, n_0, z) {
  a <- as.numeric(a)
  n0 <- as.integer(n_0)
  if (n0 <= 0L) {
    stop(sprintf(
      "n_0 must be a positive delay in samples, got %s",
      as.character(n_0)
    ))
  }
  zs <- as.complex(z)
  if (!length(zs)) stop("z must not be empty")
  if (any(zs == 0)) {
    stop("z = 0 is outside the region of convergence of z^(-n_0)")
  }
  hs <- if (length(H) == 1L) rep(as.complex(H), length(zs)) else as.complex(H)
  if (length(hs) != length(zs)) {
    stop(sprintf("H has length %d but z has length %d", length(hs), length(zs)))
  }
  hp <- Mod(hs)^2
  ep <- Mod(1 + a * zs^(-n0))^2
  list(
    power = hp * ep, wavelet_power = hp, echo_power = ep, z = zs, a = a,
    n_0 = n0,
    method = paste(
      "Power spectrum of a wavelet with an echo, Rangayyan &",
      "Krishnan (2024) eq (4.84)"
    )
  )
}

# -- Log power spectrum of a wavelet with an echo, eq (4.85).
#' Log power spectrum of a wavelet with an echo, eq (4.85)
#'
#' Part of the rangayyan_tf implementation; see the file header for the
#' source it follows.
#'
#' @param H See Usage.
#' @param a See Usage.
#' @param n_0 See Usage.
#' @param omega See Usage.
#' @return A list with \code{log_power}, \code{wavelet_log_power}, \code{echo_log_power}, \code{dc_term}, \code{ripple}, \code{modulation_index}, \code{ripple_period}, \code{decomposition_error}, \code{omega}, \code{a}, \code{n_0}, \code{method}.
#' @export
EchoLogPsd <- function(H, a, n_0, omega) {
  a <- as.numeric(a)
  n0 <- as.integer(n_0)
  if (n0 <= 0L) {
    stop(sprintf(
      "n_0 must be a positive delay in samples, got %s",
      as.character(n_0)
    ))
  }
  ws <- as.numeric(omega)
  if (!length(ws)) stop("omega must not be empty")
  hs <- if (length(H) == 1L) rep(as.complex(H), length(ws)) else as.complex(H)
  if (length(hs) != length(ws)) {
    stop(sprintf(
      "H has length %d but omega has length %d",
      length(hs), length(ws)
    ))
  }
  if (any(Mod(hs) == 0)) {
    stop("|H(w)| = 0 at some frequency; log|H|^2 is undefined there")
  }
  hl <- log(Mod(hs)^2)
  dc <- log(1 + a * a)
  mi <- 2 * a / (1 + a * a)
  vv <- 1 + a * a + 2 * a * cos(ws * n0)
  if (any(vv <= 0)) {
    bad <- which(vv <= 0)[1L]
    stop(sprintf(paste0(
      "1 + a^2 + 2 a cos(w n_0) = %s at omega=%s; the two ",
      "wavelet copies cancel exactly there and log|Y|^2 is ",
      "-infinity"
    ), vv[bad], ws[bad]))
  }
  el <- log(vv)
  r <- 1 + mi * cos(ws * n0)
  rip <- ifelse(r > 0, log(ifelse(r > 0, r, 1)), -Inf)
  lp <- hl + el
  list(
    log_power = lp, wavelet_log_power = hl, echo_log_power = el,
    dc_term = dc, ripple = rip, modulation_index = mi,
    ripple_period = 2 * pi / n0,
    decomposition_error = max(abs(el - (dc + rip))), omega = ws, a = a,
    n_0 = n0,
    method = paste(
      "Log power spectrum of a wavelet with an echo,",
      "Rangayyan & Krishnan (2024) eq (4.85)"
    )
  )
}

# pre-policy spellings
rangayyan_amplitude_demod <- CDemod
rangayyan_biorthogonal_wvlt <- BiorDwt
rangayyan_choi_williams <- ExpKerTfd
rangayyan_cpr_analysis <- CprWt
rangayyan_cwt <- Cwt
rangayyan_cohen_class <- Gtfd
rangayyan_daubechies <- OrthFilt
rangayyan_decomp_tfd <- AtomTfd
rangayyan_dwt <- Dwt
rangayyan_eemd <- EmdEns
rangayyan_emd <- Sift
rangayyan_emd_imf <- Imf
rangayyan_emd_twa <- TwaEmd
rangayyan_emd_vf_detect <- VfEmd
rangayyan_wavelet_entropy <- WtEntropy
rangayyan_haar_wavelet <- Dwt2Tap
rangayyan_hht_spectrum <- EmdSpec
rangayyan_hrv_time_varying <- HrvTv
rangayyan_istft <- IStft
rangayyan_mra <- Mra
rangayyan_pcg_envelope_avg <- PcgEnvAvg
rangayyan_ppg_wavelet <- PpgWtDen
rangayyan_scalogram <- Scalogram
rangayyan_seizure_wavelet <- SeizWt
rangayyan_stft_params <- StftParam
rangayyan_stft_spectrogram <- Spectrogram
rangayyan_swt <- Swt
rangayyan_swt_denoise <- SwtDen
rangayyan_vmd <- VModes
rangayyan_wavelet_struct <- CwtRidge
rangayyan_wavelet_corr <- WtXcor
rangayyan_wigner_ville <- WvDist
rangayyan_wavelet_energy <- WtEnergy
rangayyan_wavelet_moments <- WtMoment
rangayyan_wavelet_packet <- Wpt
rangayyan_wavelet_threshold <- WtThresh
rangayyan_wavelet_variance <- WtVar
rangayyan_ch4_signal_with_echo_input <- EchoImp
rangayyan_ch4_signal_with_echo_output <- EchoSig
rangayyan_ch4_z_transform_signal_echo <- EchoZ
rangayyan_ch4_fourier_signal_echo <- EchoSpec
rangayyan_ch4_log_signal_echo <- EchoLogSp
rangayyan_ch4_complex_cepstrum_signal_with_echo <- EchoCep
rangayyan_ch4_power_spectrum_signal_echo <- EchoPsd
rangayyan_ch4_log_power_spectrum_signal_echo <- EchoLogPsd
