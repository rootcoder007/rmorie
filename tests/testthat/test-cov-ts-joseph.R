# Coverage for the Joseph & Tackes forecasting shelf. Metrics and
# transforms are recomputed from their printed formulas, the diagnostics
# against stats::acf / stats::pacf / urca::ur.df, the multi-step
# strategies against lm.fit refits, and the deep architectures against
# plain matrix algebra of the papers' equations.

.y <- c(12, 15.5, 14, 18, 21, 19.5, 23, 26, 24.5, 28, 31, 29.5, 33, 36.5, 35, 38)
.yh <- c(11, 16, 15, 17.5, 22, 18, 24, 25, 25.5, 27, 32, 30, 31.5, 37, 34, 39)

.lnorm <- function(v, eps = 1e-5) (v - mean(v)) / sqrt(mean((v - mean(v))^2) + eps)
.smax <- function(v) exp(v - max(v)) / sum(exp(v - max(v)))

test_that("point-forecast error metrics match their definitions", {
  e <- .y - .yh
  r <- morie_rmse(.y, .yh)
  expect_equal(c(r$rmse, r$mse, r$mae, r$bias), c(sqrt(mean(e^2)), mean(e^2), mean(abs(e)), mean(e)), tolerance = 1e-12)
  pe <- 100 * abs(e) / abs(.y)
  m <- morie_mapets(.y, .yh)
  expect_equal(c(m$mape, m$mdape, m$maxape), c(mean(pe), median(pe), max(pe)), tolerance = 1e-12)
  expect_error(morie_mapets(c(0, 1), c(1, 1)), "zero")
  st <- 200 * abs(e) / (abs(.y) + abs(.yh))
  s <- morie_smape(.y, .yh)
  expect_equal(c(s$smape, s$smdape), c(mean(st), median(st)), tolerance = 1e-12)
  expect_error(morie_smape(0, 0), "undefined")
  ins <- c(3, 5, 4, 8, 6, 9, 7, 10)
  for (sn in 1:2) {
    rs <- morie_rmsse(.y, .yh, ins, season = sn)
    nd <- diff(ins, lag = sn)
    expect_equal(rs$rmsse, sqrt(mean(e^2) / mean(nd^2)), tolerance = 1e-12)
    expect_equal(rs$mase, mean(abs(e)) / mean(abs(nd)), tolerance = 1e-12)
  }
  expect_error(morie_rmsse(.y, .yh, c(1, 2), season = 2), "longer than season")
  expect_error(morie_rmsse(.y, .yh, rep(4, 5)), "naive error is zero")
  bench <- c(.y[1], .y[-length(.y)])
  rm <- morie_relmae(.y, .yh, bench)
  expect_equal(rm$relmae, mean(abs(e)) / mean(abs(.y - bench)), tolerance = 1e-12)
  expect_identical(rm$better, mean(abs(e)) < mean(abs(.y - bench)))
  expect_error(morie_relmae(.y, .yh, .y), "benchmark MAE is zero")
  expect_error(morie_rmse(1:3, 1:2), "same length")
})

test_that("pinball and Winkler interval scores", {
  for (q in c(0.1, 0.5, 0.9)) {
    p <- morie_pinball(.y, .yh, q)
    d <- .y - .yh
    expect_equal(p$loss, mean(ifelse(d >= 0, q * d, (q - 1) * d)), tolerance = 1e-12)
    expect_equal(p$coverage, mean(.y <= .yh), tolerance = 1e-12)
  }
  expect_error(morie_pinball(.y, .yh, 1), "strictly in")
  lo <- .yh - 1
  up <- .yh + 1
  w <- morie_winkler(.y, lo, up, alpha = 0.2)
  sc <- (up - lo) + 10 * pmax(lo - .y, 0) + 10 * pmax(.y - up, 0)
  expect_equal(w$score, mean(sc), tolerance = 1e-12)
  expect_equal(w$coverage, mean(.y >= lo & .y <= up), tolerance = 1e-12)
  expect_error(morie_winkler(.y, up, lo), "upper must be at least lower")
  expect_error(morie_winkler(.y, lo, up, alpha = 0), "strictly in")
})

test_that("Box-Cox, log and differencing transforms", {
  for (lam in c(0, 0.5, -1)) {
    b <- morie_boxcox(.y, lam)
    w <- if (lam == 0) log(.y) else (.y^lam - 1) / lam
    expect_equal(b$w, w, tolerance = 1e-12)
    expect_equal(b$var, mean((w - mean(w))^2), tolerance = 1e-12)
  }
  expect_error(morie_boxcox(c(1, 0), 1), "strictly positive")
  lt <- morie_logtrans(.y - 10, offset = 2)
  w <- log(.y - 8)
  expect_equal(lt$w, w, tolerance = 1e-12)
  sdp <- function(v) sqrt(mean((v - mean(v))^2))
  expect_equal(c(lt$cvbefore, lt$cvafter), c(sdp(.y - 10) / mean(.y - 10), sdp(w) / abs(mean(w))), tolerance = 1e-12)
  expect_error(morie_logtrans(c(-3, 1), offset = 2), "strictly positive")
  for (o in 1:2) {
    for (s in c(1L, 4L)) {
      d <- morie_diffser(.y, order = o, season = s)
      ref <- diff(.y, lag = s, differences = o)
      expect_equal(d$w, ref, tolerance = 1e-12)
      expect_identical(d$dropped, length(.y) - length(ref))
    }
  }
  expect_error(morie_diffser(1:4, order = 2, season = 2), "too short")
})

test_that("lag, rolling, Fourier and calendar features", {
  lf <- morie_lagfeat(.y, c(3, 1))
  em <- stats::embed(.y, 4)
  expect_equal(lf$rows, em[, c(2, 4)], tolerance = 1e-12)
  expect_equal(lf$target, em[, 1], tolerance = 1e-12)
  expect_identical(lf$lags, c(1L, 3L))
  expect_error(morie_lagfeat(.y, 0), "positive integers")
  expect_error(morie_lagfeat(1:3, 3), "too short")
  rf <- morie_rollfeat(.y, 4, minperiods = 2)
  ws <- lapply(2:16, function(i) .y[max(1, i - 3):i])
  expect_equal(rf$mean, vapply(ws, mean, 1), tolerance = 1e-12)
  expect_equal(rf$sd, vapply(ws, function(w) sqrt(mean((w - mean(w))^2)), 1), tolerance = 1e-12)
  expect_equal(rf$max, vapply(ws, max, 1))
  expect_identical(morie_rollfeat(.y, 4)$nrows, 13L)
  expect_error(morie_rollfeat(.y, 3, minperiods = 4), "minperiods")
  ff <- morie_fourfeat(10, 12, 2, start = 5)
  tt <- 5:14
  expect_equal(ff$rows, cbind(sin(2 * pi * tt / 12), cos(2 * pi * tt / 12), sin(4 * pi * tt / 12), cos(4 * pi * tt / 12)), tolerance = 1e-12)
  expect_error(morie_fourfeat(10, 3, 2), "Nyquist")
  dts <- as.Date(c("2024-02-29", "2023-12-31", "1969-07-20", "2000-03-01", "2021-01-02", "2021-01-04"))
  cf <- morie_calfeat(cbind(as.integer(format(dts, "%Y")), as.integer(format(dts, "%m")), as.integer(format(dts, "%d"))))
  lt <- as.POSIXlt(dts)
  # Monday = 0 (pandas dayofweek); the weekend is Saturday and Sunday
  dow <- (lt$wday + 6L) %% 7L
  expect_equal(vapply(cf$rows, function(r) r$dow, 1), dow)
  expect_equal(vapply(cf$rows, function(r) r$doy, 1), lt$yday + 1)
  expect_equal(vapply(cf$rows, function(r) r$weekend, 1), as.numeric(lt$wday %in% c(0L, 6L)))
  expect_identical(cf$nweekend, sum(lt$wday %in% c(0L, 6L)))
  expect_equal(vapply(cf$rows, function(r) r$monthend, 1), c(1, 1, 0, 0, 0, 0))
  expect_equal(vapply(cf$rows, function(r) r$quarter, 1), (lt$mon %/% 3) + 1)
  expect_equal(vapply(cf$rows, function(r) r$dowsin, 1), sin(2 * pi * dow / 7), tolerance = 1e-12)
  expect_error(morie_calfeat(list(c(2023L, 2L, 29L))), "out of range")
  expect_error(morie_calfeat(list(c(2023L, 13L, 1L))), "1..12")
})

test_that("tsimpute fills gaps by each documented rule", {
  x <- c(NA, 2, NA, NA, 8, 5, NA, 11, NA)
  obs <- which(!is.na(x))
  gm <- mean(x[obs])
  expect_equal(morie_tsimpute(x)$x, stats::approx(obs, x[obs], seq_along(x), rule = 2)$y, tolerance = 1e-12)
  expect_equal(morie_tsimpute(x, "ffill")$x, c(gm, 2, 2, 2, 8, 5, 5, 11, 11))
  expect_equal(morie_tsimpute(x, "bfill")$x, c(2, 2, 8, 8, 8, 5, 11, 11, gm))
  expect_equal(morie_tsimpute(x, "mean")$x, ifelse(is.na(x), gm, x))
  s <- morie_tsimpute(x, "seasonal", season = 3)
  pos <- (seq_along(x) - 1) %% 3
  ref <- x
  for (i in which(is.na(x))) ref[i] <- if (any(pos[obs] == pos[i])) mean(x[obs][pos[obs] == pos[i]]) else gm
  expect_equal(s$x, ref, tolerance = 1e-12)
  expect_identical(s$nmissing, 5L)
  expect_error(morie_tsimpute(x, "spline"), "unknown method")
  expect_error(morie_tsimpute(c(NA, NA)), "no observed")
})

test_that("ACF and PACF agree with stats::acf and stats::pacf", {
  set.seed(4)
  x <- as.numeric(stats::arima.sim(list(ar = c(0.6, -0.3)), n = 80))
  a <- morie_autocorf(x, 10)
  expect_equal(a$acf, as.numeric(stats::acf(x, lag.max = 10, plot = FALSE)$acf), tolerance = 1e-12)
  expect_identical(a$nsignif, sum(abs(a$acf[-1]) > 1.96 / sqrt(80)))
  p <- morie_pacfts(x, 10)
  expect_equal(p$pacf[-1], as.numeric(stats::pacf(x, lag.max = 10, plot = FALSE)$acf), tolerance = 1e-12)
  expect_error(morie_autocorf(x, 80), "maxlag must lie")
  expect_error(morie_autocorf(rep(1, 5), 2), "constant")
})

test_that("ADF drift regression agrees with urca::ur.df", {
  skip_if_not_installed("urca")
  set.seed(7)
  y <- cumsum(stats::rnorm(60)) + 0.3 * sin(1:60)
  for (k in 0:2) {
    r <- morie_adfur(y, lags = k)
    ref <- urca::ur.df(y, type = "drift", lags = k)
    # the module adds a 1e-12 ridge to X'X before solving
    expect_equal(r$stat, unname(ref@teststat[1]), tolerance = 1e-9)
    n <- r$n
    expect_equal(r$crit5, -2.86154 - 2.8903 / n - 4.234 / n^2, tolerance = 1e-12)
    expect_identical(r$stationary5, r$stat < r$crit5)
  }
  expect_error(morie_adfur(y, lags = -1), "non-negative")
  expect_error(morie_adfur(1:6, lags = 2), "too short")
})

test_that("stldecomp: one pass is a centred mean trend plus centred period means", {
  x <- 0.5 * (1:24) + rep(c(3, -1, 0, -2), 6) + c(0.2, -0.1)
  r <- morie_stldecomp(x, 4, iters = 1)
  n <- 24
  tr <- vapply(1:n, function(i) mean(x[max(1, i - 2):min(n, i + 2)]), 1)
  ag <- vapply(1:4, function(s) mean((x - tr)[seq(s, n, 4)]), 1)
  ag <- ag - mean(ag)
  expect_equal(r$trend, tr, tolerance = 1e-12)
  expect_equal(r$seasonal, rep(ag, 6), tolerance = 1e-12)
  expect_equal(r$trend + r$seasonal + r$remainder, x, tolerance = 1e-12)
  rr <- morie_stldecomp(x, 4, robust = TRUE, iters = 3)
  expect_equal(sum(rr$seasonal[1:4]), 0, tolerance = 1e-12)
  expect_equal(rr$seasonalstrength, max(0, 1 - mean(rr$remainder^2) / mean((x - mean(x))^2)), tolerance = 1e-12)
  expect_error(morie_stldecomp(x[1:7], 4), "two full periods")
})

test_that("tsregmat and the recursive, direct and DirRec strategies refit by least squares", {
  x <- c(5, 7, 6, 9, 8, 11, 10, 13, 12, 15, 13, 16, 15, 18, 17, 20)
  tm <- morie_tsregmat(x, c(1, 2), horizon = 3)
  idx <- 3:14
  expect_equal(tm$rows, cbind(x[idx - 1], x[idx - 2]), tolerance = 0)
  expect_equal(tm$y, x[idx + 2], tolerance = 0)
  expect_error(morie_tsregmat(1:3, 3, 2), "too short")
  ols <- function(X, y, new) sum(c(1, new) * stats::lm.fit(cbind(1, X), y)$coefficients)
  # the module's normal equations carry a 1e-12 ridge
  rec <- morie_recmulti(x, c(1, 2), 3)
  hist <- x
  tr <- morie_tsregmat(x, c(1, 2), 1)
  for (h in 1:3) {
    p <- ols(tr$rows, tr$y, hist[length(hist) - c(1, 2) + 1])
    expect_equal(rec$forecast[h], p, tolerance = 1e-9)
    hist <- c(hist, p)
  }
  dr <- morie_dirmulti(x, c(1, 2), 3)
  for (h in 1:3) {
    t2 <- morie_tsregmat(x, c(1, 2), h)
    expect_equal(dr$forecast[h], ols(t2$rows, t2$y, x[16 - c(1, 2) + 1]), tolerance = 1e-9)
  }
  dd <- morie_dirrec(x, c(1, 2), 3)
  preds <- numeric(0)
  for (h in 1:3) {
    b <- morie_tsregmat(x, c(1, 2), h)
    ex <- length(preds)
    keep <- which((2 + seq_len(b$nrows) - 1 + ex) < 16)
    X <- b$rows[keep, , drop = FALSE]
    if (ex > 0) X <- cbind(X, matrix(unlist(lapply(keep, function(i) x[2 + i - 1 + seq_len(ex)])), ncol = ex, byrow = TRUE))
    preds <- c(preds, ols(X, b$y[keep], c(x[16 - c(1, 2) + 1], preds)))
  }
  expect_equal(dd$forecast, preds, tolerance = 1e-9)
  expect_identical(c(dd$ncolsfirst, dd$ncolslast), c(2L, 4L))
  sn <- morie_seasnaive(x, 4, 6)
  expect_equal(sn$forecast, x[c(13:16, 13:14)])
  expect_error(morie_seasnaive(1:3, 4, 2), "shorter than one season")
  expect_error(morie_recmulti(x, 1, 0), "horizon must be at least 1")
})

test_that("sliding, expanding and walk-forward validation layouts", {
  sl <- morie_slidecv(20, 8, 3, step = 4)
  starts <- c(0L, 4L, 8L)
  expect_equal(sl$folds, lapply(starts, function(s) c(s, s + 8L, s + 8L, s + 11L)))
  expect_error(morie_slidecv(5, 8, 3), "too small")
  ex <- morie_expandcv(20, 10, 4)
  expect_equal(ex$folds, lapply(c(10L, 14L), function(e) c(0L, e, e, e + 4L)))
  expect_error(morie_expandcv(20, 0, 4), "positive")
  wf <- morie_walkfwd(.y, .yh, 8, 4)
  sc <- vapply(list(9:12, 13:16), function(i) sqrt(mean((.y[i] - .yh[i])^2)), 1)
  expect_equal(wf$scores, sc, tolerance = 1e-12)
  expect_equal(wf$sd, sqrt(mean((sc - mean(sc))^2)), tolerance = 1e-12)
})

test_that("quantile regression IRLS and adaptive conformal alpha", {
  set.seed(11)
  x <- stats::runif(40, 0, 10)
  y <- 2 + 0.5 * x + stats::rexp(40)
  r <- morie_quantreg(x, y, 0.5, iters = 60)
  X <- cbind(1, x)
  expect_equal(r$fitted, as.numeric(X %*% r$beta), tolerance = 1e-12)
  d <- y - r$fitted
  expect_equal(r$loss, mean(ifelse(d >= 0, 0.5 * d, -0.5 * d)), tolerance = 1e-12)
  skip_if_not_installed("quantreg")
  ref <- quantreg::rq(y ~ x, tau = 0.5)
  # IRLS with 1e-6-floored weights approaches the L1 (linear-programming)
  # optimum from above; its check loss is within 1e-4 of the exact minimum
  refloss <- mean(abs(stats::residuals(ref))) / 2
  expect_gte(r$loss, refloss - 1e-12)
  expect_lt(r$loss - refloss, 1e-4)
  expect_error(morie_quantreg(x, y[-1], 0.5), "same number of rows")
  ins <- c(TRUE, FALSE, TRUE, TRUE, FALSE, TRUE)
  ac <- morie_aci(ins, alpha = 0.1, gamma = 0.05)
  path <- 0.1 + cumsum(c(0, 0.05 * (0.1 - (!ins))))
  expect_equal(ac$alpha, path, tolerance = 1e-12)
  expect_equal(ac$empirical, 2 / 6, tolerance = 1e-12)
  expect_error(morie_aci(ins, gamma = 0), "gamma be positive")
})

test_that("series decomposition and AutoCorrelation attention (Autoformer eqs 1, 5, 6)", {
  x <- c(3, 1, 4, 1, 5, 9, 2, 6, 5, 3)
  sd5 <- morie_seriesdecomp(x, 5)
  pad <- c(rep(3, 2), x, rep(3, 2))
  expect_equal(sd5$trend, vapply(1:10, function(i) mean(pad[i:(i + 4)]), 1), tolerance = 1e-12)
  expect_equal(sd5$seasonal, x - sd5$trend, tolerance = 1e-12)
  sd4 <- morie_seriesdecomp(x, 4)
  pad4 <- c(rep(3, 2), x, rep(3, 1))
  expect_equal(sd4$trend, vapply(1:10, function(i) mean(pad4[i:(i + 3)]), 1), tolerance = 1e-12)
  q <- c(0.5, -1, 2, 0.3, -0.7, 1.1, 0.9, -0.2)
  k <- c(1, 0.4, -0.3, 2, -1, 0.6, 0.1, 0.8)
  v <- c(2, 3, -1, 0.5, 4, -2, 1, 0)
  af <- morie_autoform(q, k, v, kernel = 3, c = 1.5)
  L <- 8
  r <- Re(stats::fft(stats::fft(q) * Conj(stats::fft(k)), inverse = TRUE)) / L / L
  kk <- floor(1.5 * log(8))
  top <- sort(order(-r[2:L], 1:(L - 1))[1:kk])
  expect_identical(af$taus, top)
  w <- .smax(r[top + 1])
  expect_equal(af$weights, w, tolerance = 1e-12)
  roll <- function(t) v[((0:(L - 1) - t) %% L) + 1]
  expect_equal(af$out, Reduce(`+`, Map(function(t, a) a * roll(t), top, w)), tolerance = 1e-12)
  expect_error(morie_autoform(q, k[-1], v), "same length")
})

test_that("PatchTST instance-normalised patches", {
  x <- c(2, 4, 3, 7, 5, 8, 6, 9, 11)
  p <- morie_patchts(list(x, rev(x)), patchlen = 4, stride = 2)
  z <- (x - mean(x)) / sqrt(mean((x - mean(x))^2) + 1e-5)
  padded <- c(z, rep(z[9], 2))
  N <- (9L - 4L) %/% 2L + 2L
  expect_identical(p$n, N)
  expect_equal(p$patches[[1L]], lapply(0:(N - 1), function(i) padded[i * 2 + 1:4]), tolerance = 1e-12)
  expect_identical(p$nchannels, 2L)
  expect_error(morie_patchts(1:3, 4, 1), "shorter than one patch")
})

test_that("N-HiTS blocks: max-pool, linear projection and linear interpolation", {
  y <- c(1, 3, 2, 5, 4, 6, 8, 7)
  wf <- list(matrix(c(0.2, -0.1, 0.3, 0.05, 0.1, 0.4, -0.2, 0.3), 2, 4), matrix(c(0.5, 0.1, -0.3, 0.2, 0.1, 0.4, 0.7, -0.5), 4, 2))
  wb <- list(matrix(c(0.1, 0.2, 0.3, 0.4), 1, 4), matrix(c(0.2, -0.1, 0.05, 0.3), 2, 2))
  r <- morie_nhitsnet(y, 4, c(2, 4), c(0.5, 1), wf, wb)
  pool <- function(v, k) vapply(seq(1, length(v), by = k), function(s) max(v[s:min(s + k - 1, length(v))]), 1)
  itp <- function(th, n) if (length(th) == 1) rep(th, n) else stats::approx(0:(length(th) - 1), th, xout = (0:(n - 1)) * (length(th) - 1) / (n - 1))$y
  res <- y
  fc <- numeric(4)
  for (l in 1:2) {
    pl <- pool(res, c(2, 4)[l])
    fc <- fc + itp(as.numeric(wf[[l]] %*% pl), 4)
    res <- res - itp(as.numeric(wb[[l]] %*% pl), 8)
  }
  expect_equal(r$forecast, fc, tolerance = 1e-12)
  expect_equal(r$residual, res, tolerance = 1e-12)
  expect_identical(r$sizes, c(2L, 4L))
  expect_error(morie_nhitsnet(y, 4, 2, c(0.5, 1), wf, wb), "line up")
})

test_that("TFT gated residual network and variable selection", {
  a <- c(0.4, -1.2, 0.7)
  W <- function(r, c, s) matrix(round(sin(seq_len(r * c) * s), 3), r, c)
  w1 <- W(3, 3, 1.1)
  w2 <- W(3, 3, 0.7)
  w4 <- W(3, 3, 1.9)
  w5 <- W(3, 3, 2.3)
  wsel <- W(3, 3, 0.4)
  wq <- W(2, 3, 1.3)
  wc <- W(3, 2, 0.9)
  b <- c(0.1, -0.2, 0.05)
  cc <- c(1, -0.5)
  r <- morie_tftnet(a, w1, b, w2, b, w4, b, w5, b, wsel, b, wq, c(0.3, -0.1), c = cc, wc = wc, y = c(1, 0), q = 0.3)
  e2 <- w2 %*% a + b + wc %*% cc
  e2 <- ifelse(e2 > 0, e2, expm1(e2))
  e1 <- w1 %*% e2 + b
  gate <- as.numeric(stats::plogis(w4 %*% e1 + b) * (w5 %*% e1 + b))
  grn <- .lnorm(a + gate)
  expect_equal(r$gate, gate, tolerance = 1e-12)
  expect_equal(r$grn, grn, tolerance = 1e-12)
  sel <- .smax(as.numeric(wsel %*% grn + b))
  expect_equal(r$weights, sel, tolerance = 1e-12)
  yh <- as.numeric(wq %*% grn + c(0.3, -0.1))
  expect_equal(r$yhat, yh, tolerance = 1e-12)
  expect_equal(r$entropy, -sum(sel * log(sel)), tolerance = 1e-12)
  expect_equal(r$ql, morie_pinball(c(1, 0), yh, 0.3)$loss, tolerance = 1e-12)
  expect_identical(r$topvar, which.max(sel) - 1L)
  expect_error(morie_tftnet(a, w1, b, w2, b, w4, b, w5, b, wsel, b, wq, 0, c = cc), "wc is required")
})

test_that("TiDE residual blocks, TSMixer mixing and iTransformer attention", {
  W <- function(r, c, s) matrix(round(cos(seq_len(r * c) * s), 3), r, c)
  rb <- function(x, w1, b1, w2, b2, ws) .lnorm(as.numeric(w2 %*% pmax(w1 %*% x + b1, 0) + b2 + ws %*% x))
  y <- c(1, 2, 1.5, 3)
  f1 <- c(0.2, -0.4)
  fproj <- list(W(2, 2, 0.3), c(0, 0.1), W(2, 2, 0.8), c(0.05, 0), W(2, 2, 1.2))
  enc <- list(W(4, 6, 0.5), rep(0.1, 4), W(4, 4, 0.9), rep(0, 4), W(4, 6, 1.4))
  dec <- list(W(3, 4, 0.6), rep(0, 3), W(4, 3, 1.1), rep(0.05, 4), W(4, 4, 0.2))
  tdec <- list(W(2, 2, 0.4), c(0, 0), W(1, 2, 0.7), 0.1, W(1, 2, 1.3))
  wg <- W(2, 4, 0.25)
  td <- morie_tide(y, list(f1), fproj, enc, dec, tdec, wg, 2)
  pr <- do.call(rb, c(list(f1), fproj))
  e <- do.call(rb, c(list(c(y, pr)), enc))
  g <- do.call(rb, c(list(e), dec))
  tmp <- vapply(1:2, function(t) do.call(rb, c(list(g[(t - 1) * 2 + 1:2]), tdec)), 1)
  expect_equal(td$temporal, tmp, tolerance = 1e-12)
  expect_equal(td$forecast, tmp + as.numeric(wg %*% y), tolerance = 1e-12)
  expect_error(morie_tide(y, list(f1), fproj, enc, dec, tdec, W(3, 4, 0.1), 2), "wglobal must map")
  expect_error(morie_tide(y, list(f1), fproj, enc, dec, tdec, wg, 3), "multiple of horizon")
  ch <- list(c(1, 2, 3, 2), c(0.5, -1, 0, 2))
  wt <- W(4, 4, 0.35)
  wfe <- W(2, 2, 0.65)
  wp <- W(2, 4, 0.15)
  tm <- morie_tsmixer(ch, wt, rep(0.1, 4), wfe, c(0, 0.2), wp, c(0.1, 0), 2)
  mx <- lapply(ch, function(v) .lnorm(v + pmax(as.numeric(wt %*% v + 0.1), 0)))
  M <- rbind(mx[[1]], mx[[2]])
  for (t in 1:4) M[, t] <- .lnorm(M[, t] + pmax(as.numeric(wfe %*% M[, t] + c(0, 0.2)), 0))
  expect_equal(tm$mixed, list(M[1, ], M[2, ]), tolerance = 1e-12)
  expect_equal(tm$forecast, list(as.numeric(wp %*% M[1, ] + c(0.1, 0)), as.numeric(wp %*% M[2, ] + c(0.1, 0))), tolerance = 1e-12)
  expect_error(morie_tsmixer(list(1:3, 1:4), wt, 0, wfe, 0, wp, 0, 2), "same length")
  we <- W(3, 4, 0.45)
  wq <- W(2, 3, 0.55)
  wk <- W(2, 3, 0.75)
  wv <- W(3, 3, 0.95)
  it <- morie_itrans(ch, we, rep(0, 3), wq, wk, wv, W(3, 3, 1.05), rep(0.1, 3), W(3, 3, 1.15), rep(0, 3), W(2, 3, 1.25), c(0, 0.5))
  tok <- lapply(ch, function(v) .lnorm(as.numeric(we %*% v)))
  Q <- lapply(tok, function(t) as.numeric(wq %*% t))
  K <- lapply(tok, function(t) as.numeric(wk %*% t))
  V <- lapply(tok, function(t) as.numeric(wv %*% t))
  att <- lapply(1:2, function(i) .smax(c(sum(Q[[i]] * K[[1]]), sum(Q[[i]] * K[[2]])) / sqrt(2)))
  expect_equal(it$attn, att, tolerance = 1e-12)
  h1 <- lapply(1:2, function(i) .lnorm(tok[[i]] + att[[i]][1] * V[[1]] + att[[i]][2] * V[[2]]))
  ff <- lapply(h1, function(t) .lnorm(t + as.numeric(W(3, 3, 1.15) %*% pmax(W(3, 3, 1.05) %*% t + 0.1, 0))))
  expect_equal(it$forecast, lapply(ff, function(t) as.numeric(W(2, 3, 1.25) %*% t + c(0, 0.5))), tolerance = 1e-12)
  expect_equal(it$attndiag, (att[[1]][1] + att[[2]][2]) / 2, tolerance = 1e-12)
  expect_error(morie_itrans(list(1:3, 1:4), we, 0, wq, wk, wv, 0, 0, 0, 0, 0, 0), "same length")
})
