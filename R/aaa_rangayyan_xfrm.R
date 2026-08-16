# Rangayyan transforms -- Chapter 3.4 (Laplace, z, Fourier, DFT) and the
# homomorphic equations of Chapter 4.7.  Mirror of the Python bsaxfrm
# module.  Equation numbers were read off the PDF: the placeholder
# docstrings numbered these from the extraction sequence, not the book,
# and were wrong by up to eleven.

#' Eq (3.54): X(z) = sum_n x(n) z^-n; eq (3.55) is the causal FIR case
#'
#' Part of the rangayyan_xfrm implementation; see the file header for
#' the source it follows.
#'
#' @param x See Usage.
#' @param z Defaults to \code{NULL}.
#' @param n0 Defaults to \code{0}.
#' @return The value of \code{out}, as built in the body.
#' @export
Ztrans <- function(x, z = NULL, n0 = 0) {
  # eq (3.54): X(z) = sum_n x(n) z^-n; eq (3.55) is the causal FIR case.
  xs <- as.numeric(x)
  if (!length(xs)) stop("need at least one sample")
  idx <- n0 + seq_along(xs) - 1L
  out <- list(
    coefficients = xs, n = idx, causal = n0 >= 0,
    degree = length(xs) - 1L,
    method = "Rangayyan (2024) eqs. (3.54)-(3.55)"
  )
  if (is.null(z)) {
    out$X <- NULL
    out$z <- NULL
    return(out)
  }
  zs <- as.complex(z)
  if (any(zs == 0) && any(idx > 0)) stop("z = 0 is a pole of this sequence")
  vals <- vapply(
    zs, function(zv) sum(as.complex(xs) * zv^(-idx)),
    complex(1)
  )
  out$X <- if (length(vals) == 1L) vals[[1]] else vals
  out$z <- if (length(zs) == 1L) zs[[1]] else zs
  out
}

#' .morie_rg_conv
#'
#' Part of the rangayyan_xfrm implementation; see the file header for
#' the source it follows.
#'
#' @param xs See Usage.
#' @param hs See Usage.
#' @return A vector, from \code{vapply}.
#' @export
.morie_rg_conv <- function(xs, hs) {
  n <- length(xs)
  m <- length(hs)
  vapply(seq_len(n + m - 1L), function(k) {
    lo <- max(1L, k - m + 1L)
    hi <- min(k, n)
    idx <- lo:hi
    .morie_fsum(xs[idx] * hs[k - idx + 1L])
  }, numeric(1))
}

#' Eq (3.56): y = x * h => Y(z) = X(z) H(z).  Both sides computed
#'
#' separately so the property is demonstrated, not assumed.
#'
#' @param x See Usage.
#' @param h See Usage.
#' @param z See Usage.
#' @return A list with \code{y}, \code{Y}, \code{XH}, \code{z}, \code{max_difference}, \code{holds}, \code{method}.
#' @export
ZtConv <- function(x, h, z) {
  # eq (3.56): y = x * h  =>  Y(z) = X(z) H(z).  Both sides computed
  # separately so the property is demonstrated, not assumed.
  xs <- as.numeric(x)
  hs <- as.numeric(h)
  if (!length(xs) || !length(hs)) {
    stop("both sequences need at least one sample")
  }
  zs <- as.complex(z)
  if (any(zs == 0)) stop("z = 0 is a pole of a causal sequence")
  y <- .morie_rg_conv(xs, hs)
  zt <- function(s, zv) sum(as.complex(s) * zv^(-(seq_along(s) - 1L)))
  lhs <- vapply(zs, function(zv) zt(y, zv), complex(1))
  rhs <- vapply(zs, function(zv) zt(xs, zv) * zt(hs, zv), complex(1))
  gap <- max(Mod(lhs - rhs))
  scale <- max(Mod(rhs))
  if (scale == 0) scale <- 1
  list(
    y = y, Y = if (length(lhs) == 1L) lhs[[1]] else lhs,
    XH = if (length(rhs) == 1L) rhs[[1]] else rhs,
    z = if (length(zs) == 1L) zs[[1]] else zs,
    max_difference = gap, holds = gap <= 1e-9 * scale,
    method = "Rangayyan (2024) eq. (3.56)"
  )
}

#' Eq (3.66): the Fourier transform is the z-transform on the unit
#'
#' circle, z = exp(j omega T).  fs = NULL reads omega as normalized.
#'
#' @param x See Usage.
#' @param omega See Usage.
#' @param fs Defaults to \code{NULL}.
#' @return A list with \code{X}, \code{z}, \code{omega}, \code{T}, \code{n}, \code{on_unit_circle}, \code{method}.
#' @export
DtftZ <- function(x, omega, fs = NULL) {
  # eq (3.66): the Fourier transform is the z-transform on the unit
  # circle, z = exp(j omega T).  fs = NULL reads omega as normalized.
  xs <- as.numeric(x)
  if (!length(xs)) stop("need at least one sample")
  t_s <- if (is.null(fs)) 1 else 1 / as.numeric(fs)
  ws <- as.numeric(omega)
  zs <- complex(real = cos(ws * t_s), imaginary = sin(ws * t_s))
  idx <- seq_along(xs) - 1L
  vals <- vapply(zs, function(zv) sum(as.complex(xs) * zv^(-idx)), complex(1))
  one <- length(ws) == 1L
  list(
    X = if (one) vals[[1]] else vals, z = if (one) zs[[1]] else zs,
    omega = if (one) ws[[1]] else ws, T = t_s, n = length(xs),
    on_unit_circle = all(abs(Mod(zs) - 1) < 1e-12),
    method = "Rangayyan (2024) eq. (3.66)"
  )
}

#' Eq (3.74): exp(j omega t) = cos(omega t) + j sin(omega t)
#'
#' Part of the rangayyan_xfrm implementation; see the file header for
#' the source it follows.
#'
#' @param omega See Usage.
#' @param t Defaults to \code{0}.
#' @return A list with \code{value}, \code{real}, \code{imag}, \code{angle}, \code{unit_modulus}, \code{method}.
#' @export
Euler <- function(omega, t = 0) {
  # eq (3.74): exp(j omega t) = cos(omega t) + j sin(omega t)
  ws <- as.numeric(omega)
  ts <- as.numeric(t)
  if (length(ws) > 1L && length(ts) > 1L && length(ws) != length(ts)) {
    stop("omega and t must broadcast: equal lengths or one of them scalar")
  }
  ang <- ws * ts
  re <- cos(ang)
  im <- sin(ang)
  vals <- complex(real = re, imaginary = im)
  one <- length(vals) == 1L
  list(
    value = if (one) vals[[1]] else vals,
    real = if (one) re[[1]] else re, imag = if (one) im[[1]] else im,
    angle = if (one) ang[[1]] else ang,
    unit_modulus = all(abs(Mod(vals) - 1) < 1e-15),
    method = "Rangayyan (2024) eq. (3.74)"
  )
}

#' Eqs (3.75)-(3.76): one transform in two frequency variables,
#'
#' omega = 2 pi f.  Integrated over the supplied samples, so the limits
#' are the duration of the signal.
#'
#' @param x See Usage.
#' @param t Defaults to \code{NULL}.
#' @param omega Defaults to \code{NULL}.
#' @param f Defaults to \code{NULL}.
#' @param dt Defaults to \code{NULL}.
#' @return A list with \code{X}, \code{omega}, \code{f}, \code{variable}, \code{duration}, \code{method}.
#' @export
Ctft <- function(x, t = NULL, omega = NULL, f = NULL, dt = NULL) {
  # eqs (3.75)-(3.76): one transform in two frequency variables,
  # omega = 2 pi f.  Integrated over the supplied samples, so the limits
  # are the duration of the signal.
  xs <- as.numeric(x)
  if (length(xs) < 2L) stop("need at least two samples to integrate")
  if (is.null(omega) == is.null(f)) stop("give exactly one of omega, f")
  step <- if (is.null(dt)) 1 else as.numeric(dt)
  ts <- if (is.null(t)) (seq_along(xs) - 1) * step else as.numeric(t)
  if (length(ts) != length(xs)) stop("t and x must have the same length")
  if (!is.null(omega)) {
    ws <- as.numeric(omega)
    fs_ <- ws / (2 * pi)
    variable <- "omega"
  } else {
    fs_ <- as.numeric(f)
    ws <- 2 * pi * fs_
    variable <- "f"
  }
  vals <- vapply(ws, function(w) {
    complex(
      real = .morie_rg_gridint(xs * cos(-w * ts), ts),
      imaginary = .morie_rg_gridint(xs * sin(-w * ts), ts)
    )
  }, complex(1))
  one <- length(ws) == 1L
  list(
    X = if (one) vals[[1]] else vals,
    omega = if (one) ws[[1]] else ws, f = if (one) fs_[[1]] else fs_,
    variable = variable, duration = ts[length(ts)] - ts[1],
    method = "Rangayyan (2024) eqs. (3.75)-(3.76)"
  )
}

#' Eq (3.76), the Hz spelling of eq (3.75); one implementation so the
#'
#' two can never drift apart.
#'
#' @param x See Usage.
#' @param f See Usage.
#' @param t Defaults to \code{NULL}.
#' @param dt Defaults to \code{NULL}.
#' @return The value of \code{Ctft}.
#' @export
CtftF <- function(x, f, t = NULL, dt = NULL) {
  # eq (3.76), the Hz spelling of eq (3.75); one implementation so the
  # two can never drift apart.
  Ctft(x, t = t, f = f, dt = dt)
}

#' Eqs (3.75)-(3.76); the name Section 3.4.4 uses
#'
#' Part of the rangayyan_xfrm implementation; see the file header for
#' the source it follows.
#'
#' @param x See Usage.
#' @param t Defaults to \code{NULL}.
#' @param omega Defaults to \code{NULL}.
#' @param f Defaults to \code{NULL}.
#' @param dt Defaults to \code{NULL}.
#' @return The value of \code{Ctft}.
#' @export
Fourier <- function(x, t = NULL, omega = NULL, f = NULL, dt = NULL) {
  # eqs (3.75)-(3.76); the name Section 3.4.4 uses.
  Ctft(x, t = t, omega = omega, f = f, dt = dt)
}

#' Eq (3.77): the 1/(2 pi) belongs to the omega form only.  Getting that
#'
#' factor wrong scales the synthesis by 6.28, so the branch is explicit.
#'
#' @param X See Usage.
#' @param t See Usage.
#' @param omega Defaults to \code{NULL}.
#' @param f Defaults to \code{NULL}.
#' @return A list with \code{x}, \code{t}, \code{variable}, \code{scale}, \code{method}.
#' @export
Ictft <- function(X, t, omega = NULL, f = NULL) {
  # eq (3.77): the 1/(2 pi) belongs to the omega form only.  Getting that
  # factor wrong scales the synthesis by 6.28, so the branch is explicit.
  Xs <- as.complex(X)
  if (is.null(omega) == is.null(f)) stop("give exactly one of omega, f")
  if (!is.null(omega)) {
    grid <- as.numeric(omega)
    scale <- 1 / (2 * pi)
    k <- 1
    variable <- "omega"
  } else {
    grid <- as.numeric(f)
    scale <- 1
    k <- 2 * pi
    variable <- "f"
  }
  if (length(grid) != length(Xs)) {
    stop("X and the frequency grid must have equal length")
  }
  if (length(grid) < 2L) {
    stop("need at least two frequency points to integrate")
  }
  ts <- as.numeric(t)
  out <- vapply(ts, function(tv) {
    ang <- k * grid * tv
    re <- Re(Xs) * cos(ang) - Im(Xs) * sin(ang)
    im <- Re(Xs) * sin(ang) + Im(Xs) * cos(ang)
    complex(
      real = scale * .morie_rg_gridint(re, grid),
      imaginary = scale * .morie_rg_gridint(im, grid)
    )
  }, complex(1))
  one <- length(ts) == 1L
  list(
    x = if (one) out[[1]] else out, t = if (one) ts[[1]] else ts,
    variable = variable, scale = scale,
    method = "Rangayyan (2024) eq. (3.77)"
  )
}

#' Eq (3.78): discrete signal, CONTINUOUS frequency -- that is the whole
#'
#' distinction from the DFT of eq (3.80), which samples this at N
#' points.
#'
#' @param x See Usage.
#' @param omega See Usage.
#' @param n0 Defaults to \code{0}.
#' @return A list with \code{X}, \code{omega}, \code{n0}, \code{n}, \code{method}.
#' @export
Dtft <- function(x, omega, n0 = 0) {
  # eq (3.78): discrete signal, CONTINUOUS frequency -- that is the whole
  # distinction from the DFT of eq (3.80), which samples this at N points.
  xs <- as.numeric(x)
  if (!length(xs)) stop("need at least one sample")
  ws <- as.numeric(omega)
  idx <- n0 + seq_along(xs) - 1L
  vals <- vapply(ws, function(w) {
    complex(
      real = .morie_fsum(xs * cos(-w * idx)),
      imaginary = .morie_fsum(xs * sin(-w * idx))
    )
  }, complex(1))
  one <- length(ws) == 1L
  list(
    X = if (one) vals[[1]] else vals, omega = if (one) ws[[1]] else ws,
    n0 = as.integer(n0), n = length(xs),
    method = "Rangayyan (2024) eq. (3.78)"
  )
}

#' Eq (3.79): K need not equal N.  K > N samples the same DTFT more
#'
#' finely; K < N folds and the signal cannot be recovered.
#'
#' @param x See Usage.
#' @param k_points See Usage.
#' @return A list with \code{X}, \code{K}, \code{n}, \code{aliased}, \code{method}.
#' @export
DftK <- function(x, k_points) {
  # eq (3.79): K need not equal N.  K > N samples the same DTFT more
  # finely; K < N folds and the signal cannot be recovered.
  xs <- as.numeric(x)
  if (!length(xs)) stop("need at least one sample")
  kk <- as.integer(k_points)
  if (kk < 1L) stop("K must be positive")
  step <- 2 * pi / kk
  idx <- seq_along(xs) - 1L
  X <- vapply(0:(kk - 1L), function(k) {
    complex(
      real = .morie_fsum(xs * cos(-step * idx * k)),
      imaginary = .morie_fsum(xs * sin(-step * idx * k))
    )
  }, complex(1))
  list(
    X = X, K = kk, n = length(xs), aliased = kk < length(xs),
    method = "Rangayyan (2024) eq. (3.79)"
  )
}

#' Eq (3.80), evaluated straight from the definition: exact at any N,
#'
#' with no power-of-two requirement.  eq (3.85) is the same sum split
#' into cos and sin parts, returned here so the two cannot disagree.
#'
#' @param x See Usage.
#' @return A list with \code{X}, \code{real}, \code{imag}, \code{n}, \code{magnitude}, \code{conjugate_symmetric}, \code{method}.
#' @export
Dft <- function(x) {
  # eq (3.80), evaluated straight from the definition: exact at any N,
  # with no power-of-two requirement.  eq (3.85) is the same sum split
  # into cos and sin parts, returned here so the two cannot disagree.
  xs <- as.numeric(x)
  n <- length(xs)
  if (!n) stop("need at least one sample")
  step <- 2 * pi / n
  idx <- seq_len(n) - 1L
  re <- vapply(
    idx, function(k) .morie_fsum(xs * cos(-step * idx * k)),
    numeric(1)
  )
  im <- vapply(
    idx, function(k) .morie_fsum(xs * sin(-step * idx * k)),
    numeric(1)
  )
  X <- complex(real = re, imaginary = im)
  mirror <- X[((n - idx) %% n) + 1L]
  sym <- all(Mod(X - Conj(mirror)) < 1e-9 * (1 + Mod(X)))
  list(
    X = X, real = re, imag = im, n = n, magnitude = Mod(X),
    conjugate_symmetric = sym, method = "Rangayyan (2024) eq. (3.80)"
  )
}

#' Eq (3.80) with bin k at k fs / N.  Figure 3.38: for even N, DC and
#'
#' the folding frequency fs/2 are the two real-valued bins.
#'
#' @param x See Usage.
#' @param fs Defaults to \code{1}.
#' @return The value of \code{r}, as built in the body.
#' @export
DftX <- function(x, fs = 1) {
  # eq (3.80) with bin k at k fs / N.  Figure 3.38: for even N, DC and
  # the folding frequency fs/2 are the two real-valued bins.
  r <- Dft(x)
  n <- r$n
  r$freqs <- (seq_len(n) - 1) * as.numeric(fs) / n
  r$fs <- as.numeric(fs)
  r$folding_frequency <- as.numeric(fs) / 2
  r$unique_bins <- n %/% 2L + 1L
  r
}

#' Eq (3.82): W_N = exp(-j 2 pi / N), the N-th root of unity
#'
#' Part of the rangayyan_xfrm implementation; see the file header for
#' the source it follows.
#'
#' @param npoints See Usage.
#' @param power Defaults to \code{1}.
#' @return A list with \code{W}, \code{N}, \code{power}, \code{root_of_unity}, \code{method}.
#' @export
Twiddle <- function(npoints, power = 1) {
  # eq (3.82): W_N = exp(-j 2 pi / N), the N-th root of unity.
  n <- as.integer(npoints)
  if (n < 1L) stop("N must be positive")
  ps <- as.integer(power)
  vals <- complex(
    real = cos(-2 * pi * ps / n),
    imaginary = sin(-2 * pi * ps / n)
  )
  one <- length(ps) == 1L
  list(
    W = if (one) vals[[1]] else vals, N = n,
    power = if (one) ps[[1]] else ps,
    root_of_unity = if (one) Mod(vals[[1]]^n - 1) < 1e-9 else NULL,
    method = "Rangayyan (2024) eq. (3.82)"
  )
}

#' Eq (3.83): the same transform written with twiddle factors, which is
#'
#' the structure the FFT exploits via eqs (3.88)-(3.89).  Checked
#' against Dft() rather than assumed equal.
#'
#' @param x See Usage.
#' @return A list with \code{X}, \code{W}, \code{n}, \code{max_difference}, \code{agrees_with_definition}, \code{method}.
#' @export
DftTw <- function(x) {
  # eq (3.83): the same transform written with twiddle factors, which is
  # the structure the FFT exploits via eqs (3.88)-(3.89).  Checked
  # against Dft() rather than assumed equal.
  xs <- as.numeric(x)
  n <- length(xs)
  if (!n) stop("need at least one sample")
  w <- complex(real = cos(-2 * pi / n), imaginary = sin(-2 * pi / n))
  X <- vapply(seq_len(n) - 1L, function(k) {
    acc <- complex(real = 0, imaginary = 0)
    wk <- complex(real = 1, imaginary = 0)
    step <- w^k
    for (v in xs) {
      acc <- acc + v * wk
      wk <- wk * step
    }
    acc
  }, complex(1))
  direct <- Dft(xs)$X
  gap <- max(Mod(X - direct))
  list(
    X = X, W = w, n = n, max_difference = gap,
    agrees_with_definition = gap <= 1e-8 * (1 + max(Mod(direct))),
    method = "Rangayyan (2024) eq. (3.83)"
  )
}

#' Eq (3.84): W_N^(nk) = cos(.) - j sin(.).  Note the MINUS on the sine:
#'
#' the DFT projects onto the conjugated exponential, and that sign is
#' the commonest transcription error in a hand-written DFT.
#'
#' @param npoints See Usage.
#' @param n See Usage.
#' @param k See Usage.
#' @return A list with \code{W}, \code{cos}, \code{sin}, \code{angle}, \code{N}, \code{n}, \code{k}, \code{method}.
#' @export
TwidCS <- function(npoints, n, k) {
  # eq (3.84): W_N^(nk) = cos(.) - j sin(.).  Note the MINUS on the sine:
  # the DFT projects onto the conjugated exponential, and that sign is
  # the commonest transcription error in a hand-written DFT.
  nn <- as.integer(npoints)
  if (nn < 1L) stop("N must be positive")
  ang <- 2 * pi * as.integer(n) * as.integer(k) / nn
  cc <- cos(ang)
  ss <- sin(ang)
  list(
    W = complex(real = cc, imaginary = -ss), cos = cc, sin = ss,
    angle = ang, N = nn, n = as.integer(n), k = as.integer(k),
    method = "Rangayyan (2024) eq. (3.84)"
  )
}

#' Eq (3.85): the real part is the projection onto the k-th cosine, the
#'
#' imaginary part is MINUS the projection onto the corresponding sine.
#'
#' @param x See Usage.
#' @return A list with \code{X}, \code{cos_projection}, \code{sin_projection}, \code{real}, \code{imag}, \code{n}, \code{method}.
#' @export
DftRI <- function(x) {
  # eq (3.85): the real part is the projection onto the k-th cosine, the
  # imaginary part is MINUS the projection onto the corresponding sine.
  xs <- as.numeric(x)
  n <- length(xs)
  if (!n) stop("need at least one sample")
  step <- 2 * pi / n
  idx <- seq_len(n) - 1L
  cp <- vapply(
    idx, function(k) .morie_fsum(xs * cos(step * idx * k)),
    numeric(1)
  )
  sp <- vapply(
    idx, function(k) .morie_fsum(xs * sin(step * idx * k)),
    numeric(1)
  )
  list(
    X = complex(real = cp, imaginary = -sp), cos_projection = cp,
    sin_projection = sp, real = cp, imag = -sp, n = n,
    method = "Rangayyan (2024) eq. (3.85)"
  )
}

#' Eq (3.86): synthesis as a weighted sum of sinusoids.  The imaginary
#'
#' residue is reported, not discarded -- a large one means the spectrum
#' was not conjugate-symmetric and the "real signal" reading is wrong.
#'
#' @param X See Usage.
#' @return A list with \code{x}, \code{complex}, \code{n}, \code{max_imaginary}, \code{method}.
#' @export
IdftRI <- function(X) {
  # eq (3.86): synthesis as a weighted sum of sinusoids.  The imaginary
  # residue is reported, not discarded -- a large one means the spectrum
  # was not conjugate-symmetric and the "real signal" reading is wrong.
  Xs <- as.complex(X)
  n <- length(Xs)
  if (!n) stop("need at least one coefficient")
  step <- 2 * pi / n
  idx <- seq_len(n) - 1L
  out <- vapply(idx, function(i) {
    ang <- step * i * idx
    sum(Xs * complex(real = cos(ang), imaginary = sin(ang))) / n
  }, complex(1))
  list(
    x = Re(out), complex = out, n = n, max_imaginary = max(abs(Im(out))),
    method = "Rangayyan (2024) eq. (3.86)"
  )
}

#' Eq (3.87).  The book is explicit that the convolution here is
#'
#' PERIODIC: multiplying N-point DFTs gives the circular convolution of
#' eq (3.90), and the linear one needs L >= Nx + Nh - 1 with both
#' sequences zero-padded.  Both are returned so the wrap is visible.
#'
#' @param x See Usage.
#' @param h See Usage.
#' @return A list with \code{linear}, \code{circular}, \code{from_dft}, \code{padded_length}, \code{n_linear}, \code{n_circular}, \code{max_difference}, \code{holds}, \code{wraps_if_unpadded}, \code{method}.
#' @export
DftConv <- function(x, h) {
  # eq (3.87).  The book is explicit that the convolution here is
  # PERIODIC: multiplying N-point DFTs gives the circular convolution of
  # eq (3.90), and the linear one needs L >= Nx + Nh - 1 with both
  # sequences zero-padded.  Both are returned so the wrap is visible.
  xs <- as.numeric(x)
  hs <- as.numeric(h)
  if (!length(xs) || !length(hs)) {
    stop("both sequences need at least one sample")
  }
  nx <- length(xs)
  nh <- length(hs)
  lin <- .morie_rg_conv(xs, hs)
  L <- nx + nh - 1L
  xp <- c(xs, numeric(L - nx))
  hp <- c(hs, numeric(L - nh))
  rec <- IdftRI(Dft(xp)$X * Dft(hp)$X)$x
  n <- max(nx, nh)
  xc <- c(xs, numeric(n - nx))
  hc <- c(hs, numeric(n - nh))
  circ <- vapply(seq_len(n) - 1L, function(k) {
    .morie_fsum(xc * hc[((k - (seq_len(n) - 1L)) %% n) + 1L])
  }, numeric(1))
  gap <- max(abs(rec - lin))
  list(
    linear = lin, circular = circ, from_dft = rec, padded_length = L,
    n_linear = L, n_circular = n, max_difference = gap,
    holds = gap <= 1e-8 * (1 + max(abs(lin))),
    wraps_if_unpadded = n < L,
    method = "Rangayyan (2024) eq. (3.87)"
  )
}

#' Eq (3.88): W_N^(-nk) = conj(W_N^(nk)) -- a negative power costs only
#'
#' a sign flip, one of the two properties the FFT is built on.
#'
#' @param npoints See Usage.
#' @param n See Usage.
#' @param k See Usage.
#' @return A list with \code{negative_power}, \code{conjugate}, \code{difference}, \code{holds}, \code{N}, \code{n}, \code{k}, \code{method}.
#' @export
TwidConj <- function(npoints, n, k) {
  # eq (3.88): W_N^(-nk) = conj(W_N^(nk)) -- a negative power costs only
  # a sign flip, one of the two properties the FFT is built on.
  nn <- as.integer(npoints)
  if (nn < 1L) stop("N must be positive")
  p <- as.integer(n) * as.integer(k)
  lhs <- complex(real = cos(2 * pi * p / nn), imaginary = sin(2 * pi * p / nn))
  rhs <- Conj(complex(
    real = cos(-2 * pi * p / nn),
    imaginary = sin(-2 * pi * p / nn)
  ))
  list(
    negative_power = lhs, conjugate = rhs, difference = Mod(lhs - rhs),
    holds = Mod(lhs - rhs) < 1e-12, N = nn, n = as.integer(n),
    k = as.integer(k), method = "Rangayyan (2024) eq. (3.88)"
  )
}

#' Eq (3.89): indices reduce modulo N -- why the same roots of unity are
#'
#' reused at every FFT stage, and why every DFT relation is periodic.
#'
#' @param npoints See Usage.
#' @param n See Usage.
#' @param k See Usage.
#' @return A list with \code{base}, \code{shift_k}, \code{shift_n}, \code{max_difference}, \code{holds}, \code{N}, \code{n}, \code{k}, \code{method}.
#' @export
TwidPer <- function(npoints, n, k) {
  # eq (3.89): indices reduce modulo N -- why the same roots of unity are
  # reused at every FFT stage, and why every DFT relation is periodic.
  nn <- as.integer(npoints)
  if (nn < 1L) stop("N must be positive")
  ni <- as.integer(n)
  ki <- as.integer(k)
  w <- function(p) {
    complex(
      real = cos(-2 * pi * p / nn),
      imaginary = sin(-2 * pi * p / nn)
    )
  }
  base <- w(ni * ki)
  sk <- w(ni * (ki + nn))
  sn <- w((ni + nn) * ki)
  gap <- max(Mod(base - sk), Mod(base - sn))
  list(
    base = base, shift_k = sk, shift_n = sn, max_difference = gap,
    holds = gap < 1e-9, N = nn, n = ni, k = ki,
    method = "Rangayyan (2024) eq. (3.89)"
  )
}

#' Eq (3.90): y_p(n) = sum_k x_p(k) h_p[(n-k) mod N], defined only for
#'
#' equal periods.  Both routes -- the modular sum and the inverse DFT of
#' X(k)H(k) -- are computed; their agreement is eq (3.87) at equal N.
#'
#' @param x See Usage.
#' @param h See Usage.
#' @param npoints Defaults to \code{NULL}.
#' @return A list with \code{y}, \code{via_dft}, \code{N}, \code{max_difference}, \code{agrees}, \code{equals_linear}, \code{linear_length}, \code{method}.
#' @export
CircConv <- function(x, h, npoints = NULL) {
  # eq (3.90): y_p(n) = sum_k x_p(k) h_p[(n-k) mod N], defined only for
  # equal periods.  Both routes -- the modular sum and the inverse DFT of
  # X(k)H(k) -- are computed; their agreement is eq (3.87) at equal N.
  xs <- as.numeric(x)
  hs <- as.numeric(h)
  if (!length(xs) || !length(hs)) {
    stop("both signals need at least one sample")
  }
  n <- if (is.null(npoints)) {
    max(length(xs), length(hs))
  } else {
    as.integer(npoints)
  }
  if (n < max(length(xs), length(hs))) {
    stop("N must be at least the length of both signals")
  }
  xp <- c(xs, numeric(n - length(xs)))
  hp <- c(hs, numeric(n - length(hs)))
  k0 <- seq_len(n) - 1L
  direct <- vapply(k0, function(i) {
    .morie_fsum(xp * hp[((i - k0) %% n) + 1L])
  }, numeric(1))
  via <- IdftRI(Dft(xp)$X * Dft(hp)$X)$x
  gap <- max(abs(direct - via))
  lin_len <- length(xs) + length(hs) - 1L
  list(
    y = direct, via_dft = via, N = n, max_difference = gap,
    agrees = gap <= 1e-8 * (1 + max(abs(direct))),
    equals_linear = n >= lin_len, linear_length = lin_len,
    method = "Rangayyan (2024) eq. (3.90)"
  )
}

#' .morie_rg_evenodd
#'
#' Part of the rangayyan_xfrm implementation; see the file header for
#' the source it follows.
#'
#' @param x See Usage.
#' @param n Defaults to \code{NULL}.
#' @return A list with \code{n}, \code{even}, \code{odd}, \code{x}, \code{reconstruction_error}.
#' @export
.morie_rg_evenodd <- function(x, n = NULL) {
  xs <- as.numeric(x)
  m <- length(xs)
  if (!m) stop("need at least one sample")
  if (is.null(n)) {
    if (m %% 2L == 0L) {
      stop(
        "with no index grid the sequence must have an odd length so ",
        "that n = 0 is a sample; pass n="
      )
    }
    idx <- seq_len(m) - 1L - (m %/% 2L)
  } else {
    idx <- as.integer(n)
    if (length(idx) != m) stop("n and x must have the same length")
  }
  pos <- match(-idx, idx)
  if (anyNA(pos)) {
    stop(
      "index grid is not symmetric: x(-n) is unavailable for n = ",
      paste(idx[is.na(pos)][1:min(5, sum(is.na(pos)))], collapse = ", ")
    )
  }
  ev <- 0.5 * (xs + xs[pos])
  od <- 0.5 * (xs - xs[pos])
  list(
    n = idx, even = ev, odd = od, x = xs,
    reconstruction_error = max(abs(ev + od - xs))
  )
}

#' Eq (3.92): x_e(n) = 0.5 [x(n) + x(-n)].  x(-n) must exist, so the
#'
#' index grid has to be symmetric; reflecting a causal sequence about 0
#' instead computes x/2, which is something else entirely.
#'
#' @param x See Usage.
#' @param n Defaults to \code{NULL}.
#' @return A vector, from \code{c}.
#' @export
EvenPart <- function(x, n = NULL) {
  # eq (3.92): x_e(n) = 0.5 [x(n) + x(-n)].  x(-n) must exist, so the
  # index grid has to be symmetric; reflecting a causal sequence about 0
  # instead computes x/2, which is something else entirely.
  c(.morie_rg_evenodd(x, n), method = "Rangayyan (2024) eq. (3.92)")
}

#' Eq (3.93): x_o(n) = 0.5 [x(n) - x(-n)]; forced to 0 at the origin
#'
#' Part of the rangayyan_xfrm implementation; see the file header for
#' the source it follows.
#'
#' @param x See Usage.
#' @param n Defaults to \code{NULL}.
#' @return A vector, from \code{c}.
#' @export
OddPart <- function(x, n = NULL) {
  # eq (3.93): x_o(n) = 0.5 [x(n) - x(-n)]; forced to 0 at the origin.
  c(.morie_rg_evenodd(x, n), method = "Rangayyan (2024) eq. (3.93)")
}

#' Eqs (3.92)-(3.94).  Eq (3.94) is an identity, so the reconstruction
#'
#' error checks the index bookkeeping, not the arithmetic.
#'
#' @param x See Usage.
#' @param n Defaults to \code{NULL}.
#' @return A vector, from \code{c}.
#' @export
EvenOdd <- function(x, n = NULL) {
  # eqs (3.92)-(3.94).  Eq (3.94) is an identity, so the reconstruction
  # error checks the index bookkeeping, not the arithmetic.
  c(.morie_rg_evenodd(x, n),
    method = "Rangayyan (2024) eqs. (3.92)-(3.94)"
  )
}

#' Eqs (4.58)-(4.60): y = x p, log y = log x + log p, and so
#'
#' Y_l(omega) = X_l(omega) + P_l(omega).  Eq (4.59) needs both factors
#' nonzero, so zeros are rejected rather than yielding -Inf.
#'
#' @param x See Usage.
#' @param p See Usage.
#' @param omega See Usage.
#' @param t Defaults to \code{NULL}.
#' @param dt Defaults to \code{NULL}.
#' @return A list with \code{y}, \code{Yl}, \code{Xl}, \code{Pl}, \code{max_difference}, \code{additive}, \code{method}.
#' @export
LogFT <- function(x, p, omega, t = NULL, dt = NULL) {
  # eqs (4.58)-(4.60): y = x p, log y = log x + log p, and so
  # Y_l(omega) = X_l(omega) + P_l(omega).  Eq (4.59) needs both factors
  # nonzero, so zeros are rejected rather than yielding -Inf.
  xs <- as.numeric(x)
  ps <- as.numeric(p)
  if (length(xs) != length(ps)) stop("x and p must have the same length")
  if (any(xs == 0) || any(ps == 0)) {
    stop("eq. (4.59) needs x(t) != 0 and p(t) != 0 for all t")
  }
  if (any(xs < 0) || any(ps < 0)) {
    stop(
      "real logarithm needs positive signals; take the complex ",
      "cepstrum route for signed data"
    )
  }
  y <- xs * ps
  Yl <- Ctft(log(y), t = t, omega = omega, dt = dt)$X
  Xl <- Ctft(log(xs), t = t, omega = omega, dt = dt)$X
  Pl <- Ctft(log(ps), t = t, omega = omega, dt = dt)$X
  gap <- max(Mod(Yl - (Xl + Pl)))
  list(
    y = y, Yl = Yl, Xl = Xl, Pl = Pl, max_difference = gap,
    additive = gap <= 1e-8 * (1 + max(Mod(Yl))),
    method = "Rangayyan (2024) eqs. (4.58)-(4.60)"
  )
}

#' Eqs (4.61)-(4.62): the Fourier transform turns the convolution into a
#'
#' product, which eq (4.63) then turns into a sum.  The convolution is
#' scaled by dt as in eq (3.30), so the identity holds in the
#' continuous-time sense rather than up to a sampling-interval factor.
#'
#' @param x See Usage.
#' @param h See Usage.
#' @param omega See Usage.
#' @param dt Defaults to \code{1}.
#' @return A list with \code{y}, \code{Y}, \code{X}, \code{H}, \code{XH}, \code{max_difference}, \code{holds}, \code{method}.
#' @export
FtConv <- function(x, h, omega, dt = 1) {
  # eqs (4.61)-(4.62): the Fourier transform turns the convolution into a
  # product, which eq (4.63) then turns into a sum.  The convolution is
  # scaled by dt as in eq (3.30), so the identity holds in the
  # continuous-time sense rather than up to a sampling-interval factor.
  xs <- as.numeric(x)
  hs <- as.numeric(h)
  if (!length(xs) || !length(hs)) {
    stop("both signals need at least one sample")
  }
  step <- as.numeric(dt)
  if (step <= 0) stop("dt must be positive")
  y <- .morie_rg_conv(xs, hs) * step
  ws <- as.numeric(omega)
  tf <- function(sig) {
    idx <- seq_along(sig) - 1L
    vapply(
      ws, function(w) {
        complex(
          real = .morie_fsum(sig * cos(-w * idx * step)),
          imaginary = .morie_fsum(sig * sin(-w * idx * step))
        ) * step
      },
      complex(1)
    )
  }
  Y <- tf(y)
  X <- tf(xs)
  H <- tf(hs)
  prod <- X * H
  gap <- max(Mod(Y - prod))
  one <- length(ws) == 1L
  list(
    y = y, Y = if (one) Y[[1]] else Y, X = if (one) X[[1]] else X,
    H = if (one) H[[1]] else H, XH = if (one) prod[[1]] else prod,
    max_difference = gap, holds = gap <= 1e-8 * (1 + max(Mod(prod))),
    method = "Rangayyan (2024) eqs. (4.61)-(4.62)"
  )
}

#' Eqs (4.63), (4.65): complex logs of the z-transforms add.  Arg() is a
#'
#' principal value in (-pi, pi], so the two sides can differ by an
#' integer multiple of 2 pi j; that is reported as branch_offset rather
#' than papered over -- it is the phase-unwrapping problem itself.
#'
#' @param x See Usage.
#' @param h See Usage.
#' @param z See Usage.
#' @return A list with \code{y}, \code{Y_hat}, \code{X_hat}, \code{H_hat}, \code{magnitude_difference}, \code{branch_offset}, \code{holds_up_to_branch}, \code{method}.
#' @export
ClogSum <- function(x, h, z) {
  # eqs (4.63), (4.65): complex logs of the z-transforms add.  Arg() is a
  # principal value in (-pi, pi], so the two sides can differ by an
  # integer multiple of 2 pi j; that is reported as branch_offset rather
  # than papered over -- it is the phase-unwrapping problem itself.
  xs <- as.numeric(x)
  hs <- as.numeric(h)
  if (!length(xs) || !length(hs)) {
    stop("both sequences need at least one sample")
  }
  y <- .morie_rg_conv(xs, hs)
  zs <- as.complex(z)
  if (any(zs == 0)) stop("z = 0 is a pole of a causal sequence")
  zt <- function(s, zv) sum(as.complex(s) * zv^(-(seq_along(s) - 1L)))
  clog <- function(v) complex(real = log(Mod(v)), imaginary = Arg(v))
  Yh <- Xh <- Hh <- complex(length(zs))
  off <- numeric(length(zs))
  for (i in seq_along(zs)) {
    Y <- zt(y, zs[i])
    X <- zt(xs, zs[i])
    H <- zt(hs, zs[i])
    if (Y == 0 || X == 0 || H == 0) {
      stop("the complex log needs X(z) != 0 and H(z) != 0")
    }
    Yh[i] <- clog(Y)
    Xh[i] <- clog(X)
    Hh[i] <- clog(H)
    off[i] <- (Im(Yh[i]) - Im(Xh[i]) - Im(Hh[i])) / (2 * pi)
  }
  mag_gap <- max(abs(Re(Yh) - Re(Xh) - Re(Hh)))
  wrap <- max(abs(off - round(off)))
  one <- length(zs) == 1L
  list(
    y = y, Y_hat = if (one) Yh[[1]] else Yh,
    X_hat = if (one) Xh[[1]] else Xh, H_hat = if (one) Hh[[1]] else Hh,
    magnitude_difference = mag_gap,
    branch_offset = if (one) off[[1]] else off,
    holds_up_to_branch = mag_gap < 1e-9 && wrap < 1e-9,
    method = "Rangayyan (2024) eqs. (4.63), (4.65)"
  )
}

#' Eq (4.69): log(1 + x) = x - x^2/2 + x^3/3 - ..., |x| < 1.  The radius
#'
#' is exactly 1, so |x| >= 1 is refused instead of diverging quietly.
#'
#' @param x See Usage.
#' @param terms Defaults to \code{20}.
#' @return A list with \code{value}, \code{exact}, \code{error}, \code{error_bound}, \code{terms}, \code{method}.
#' @export
LogSeries <- function(x, terms = 20) {
  # eq (4.69): log(1 + x) = x - x^2/2 + x^3/3 - ..., |x| < 1.  The radius
  # is exactly 1, so |x| >= 1 is refused instead of diverging quietly.
  xs <- as.complex(x)
  k <- as.integer(terms)
  if (k < 1L) stop("terms must be positive")
  if (any(Mod(xs) >= 1)) {
    stop("the series converges only for |x| < 1")
  }
  res <- vapply(xs, function(v) {
    s <- complex(real = 0, imaginary = 0)
    p <- complex(real = 1)
    for (n in seq_len(k)) {
      p <- p * v
      s <- s + (-1)^(n + 1) * p / n
    }
    s
  }, complex(1))
  bound <- vapply(xs, function(v) Mod(v)^(k + 1) / (k + 1), numeric(1))
  exact <- log(1 + xs)
  one <- length(xs) == 1L
  list(
    value = if (one) res[[1]] else res,
    exact = if (one) exact[[1]] else exact,
    error = max(Mod(res - exact)),
    error_bound = if (one) bound[[1]] else bound, terms = k,
    method = "Rangayyan (2024) eq. (4.69)"
  )
}

#' Eq (4.70): log(1 - alpha z^-1) = -sum alpha^n/n z^-n, |z| > |alpha|
#'
#' The coefficients sit at POSITIVE quefrency and decay at least as fast
#' as 1/n: the minimum-phase cepstrum is causal.
#'
#' @param alpha See Usage.
#' @param terms Defaults to \code{20}.
#' @param z Defaults to \code{NULL}.
#' @return The value of \code{out}, as built in the body.
#' @export
LogMinPh <- function(alpha, terms = 20, z = NULL) {
  # eq (4.70): log(1 - alpha z^-1) = -sum alpha^n/n z^-n, |z| > |alpha|.
  # The coefficients sit at POSITIVE quefrency and decay at least as fast
  # as 1/n: the minimum-phase cepstrum is causal.
  a <- as.complex(alpha)
  k <- as.integer(terms)
  if (k < 1L) stop("terms must be positive")
  ns <- seq_len(k)
  coeffs <- -(a^ns) / ns
  out <- list(
    coefficients = coeffs, quefrency = ns, causal = TRUE,
    alpha = a, method = "Rangayyan (2024) eq. (4.70)"
  )
  if (!is.null(z)) {
    zv <- as.complex(z)
    if (Mod(zv) <= Mod(a)) stop("the expansion needs |z| > |alpha|")
    s <- sum(coeffs * zv^(-ns))
    exact <- log(1 - a / zv)
    out$value <- s
    out$exact <- exact
    out$error <- Mod(s - exact)
    out$z <- zv
  }
  out
}

#' Eq (4.71): log(1 - beta z) = -sum beta^n/n z^n, |z| < 1/|beta|.  The
#'
#' mirror of eq (4.70): positive powers of z, so the maximum-phase part
#' of the cepstrum is anticausal -- which is why liftering windows for
#' homomorphic deconvolution are two-sided.
#'
#' @param beta See Usage.
#' @param terms Defaults to \code{20}.
#' @param z Defaults to \code{NULL}.
#' @return The value of \code{out}, as built in the body.
#' @export
LogMaxPh <- function(beta, terms = 20, z = NULL) {
  # eq (4.71): log(1 - beta z) = -sum beta^n/n z^n, |z| < 1/|beta|.  The
  # mirror of eq (4.70): positive powers of z, so the maximum-phase part
  # of the cepstrum is anticausal -- which is why liftering windows for
  # homomorphic deconvolution are two-sided.
  b <- as.complex(beta)
  k <- as.integer(terms)
  if (k < 1L) stop("terms must be positive")
  ns <- seq_len(k)
  coeffs <- -(b^ns) / ns
  out <- list(
    coefficients = coeffs, quefrency = -ns, causal = FALSE,
    beta = b, method = "Rangayyan (2024) eq. (4.71)"
  )
  if (!is.null(z)) {
    zv <- as.complex(z)
    if (Mod(b) != 0 && Mod(zv) >= 1 / Mod(b)) {
      stop("the expansion needs |z| < 1/|beta|")
    }
    s <- sum(coeffs * zv^ns)
    exact <- log(1 - b * zv)
    out$value <- s
    out$exact <- exact
    out$error <- Mod(s - exact)
    out$z <- zv
  }
  out
}

# pre-policy spellings
morie_ch3_z_transform <- Ztrans
morie_ch3_z_convolution <- ZtConv
morie_ch3_dtft_via_z <- DtftZ
morie_ch3_complex_exponential <- Euler
morie_ch3_fourier_transform <- Ctft
morie_ch3_inverse_fourier_transform <- Ictft
morie_ch3_dtft <- Dtft
morie_ch3_dft_k_samples <- DftK
morie_ch3_dft <- Dft
morie_ch3_twiddle_factor <- Twiddle
morie_ch3_dft_via_twiddle <- DftTw
morie_ch3_twiddle_cos_sin <- TwidCS
morie_ch3_dft_real_imag <- DftRI
morie_ch3_idft_real_imag <- IdftRI
morie_ch3_dft_convolution <- DftConv
morie_ch3_twiddle_conjugate <- TwidConj
morie_ch3_twiddle_periodicity <- TwidPer
morie_circular_conv_dft <- CircConv
morie_ch3_even_part <- EvenPart
morie_ch3_odd_part <- OddPart
morie_ch3_even_odd <- EvenOdd
morie_ch4_homomorphic_log_fourier <- LogFT
morie_ch4_fourier_convolution <- FtConv
morie_ch4_log_of_convolved <- ClogSum
morie_ch4_log_power_series <- LogSeries
morie_ch4_log_min_phase <- LogMinPh
morie_ch4_log_max_phase <- LogMaxPh
