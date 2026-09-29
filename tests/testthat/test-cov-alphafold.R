# Coverage for the AlphaFold 2 modules (Alf*.R, alfmpv_native.R): each
# module is re-derived in the test with vectorised base-R algebra from the
# supplementary-information algorithm it implements.

af_w <- function(r, c, k) matrix(sin(seq_len(r * c) * k + k), r, c)
af_ln <- function(v) (v - mean(v)) / sqrt(mean((v - mean(v))^2) + 1e-5)
af_sm <- function(v) exp(v - max(v)) / sum(exp(v - max(v)))
af_sig <- function(x) 1 / (1 + exp(-x))
af_arr <- function(d, k) array(cos(seq_len(prod(d)) * k), d)
af_rot <- function(a) {
  q <- c(1, a) / sqrt(1 + sum(a^2))
  w <- q[1]
  x <- q[2]
  y <- q[3]
  z <- q[4]
  rbind(c(1 - 2 * (y^2 + z^2), 2 * (x * y - w * z), 2 * (x * z + w * y)),
        c(2 * (x * y + w * z), 1 - 2 * (x^2 + z^2), 2 * (y * z - w * x)),
        c(2 * (x * z - w * y), 2 * (y * z + w * x), 1 - 2 * (x^2 + y^2)))
}
af_frame <- function(a, t) list(R = af_rot(a), t = t)

test_that("Alfbkb turns (b, c, d, t) into rigid frames", {
  s <- rbind(c(0.2, -0.1, 0.4), c(0.5, 0.3, -0.2))
  W <- af_w(6, 3, 0.7)
  b <- c(0.1, 0, -0.1, 1, 2, 3)
  r <- Alfbkb(s, W, b)
  p1 <- as.numeric(W %*% s[1, ] + b)
  expect_equal(r$frames[[1]]$R, af_rot(p1[1:3]), tolerance = 1e-12)
  expect_equal(crossprod(r$frames[[1]]$R), diag(3), tolerance = 1e-12)
  expect_equal(r$frames[[1]]$t, p1[4:6], tolerance = 1e-12)
  expect_equal(r$quat[1, ], c(1, p1[1:3]) / sqrt(1 + sum(p1[1:3]^2)), tolerance = 1e-12)
  base <- list(af_frame(c(0.3, 0.1, 0), c(1, 0, 0)), af_frame(c(0, 0, 0.5), c(0, 1, 0)))
  rc <- Alfbkb(s, W, b, frames = base)
  p2 <- as.numeric(W %*% s[2, ] + b)
  expect_equal(rc$frames[[2]]$R, base[[2]]$R %*% af_rot(p2[1:3]), tolerance = 1e-12)
  expect_equal(rc$frames[[2]]$t, as.numeric(base[[2]]$R %*% p2[4:6]) + c(0, 1, 0),
               tolerance = 1e-12)
})

test_that("Alfcrop slices a contiguous window", {
  tg <- matrix(1:20, 10)
  pr <- matrix(1:100, 10)
  ms <- matrix(1:30, 3)
  r <- Alfcrop(10, 4, start = 3, target = tg, pair = pr, msa = ms)
  expect_equal(r$idx, 3:6)
  expect_equal(r$pair, pr[3:6, 3:6])
  expect_equal(r$msa, ms[, 3:6])
  expect_equal(r$startmax, 7)
  expect_null(Alfcrop(10, 4)$target)
  expect_error(Alfcrop(10, 4, mode = "x"), "clamped")
  expect_error(Alfcrop(3, 4), "exceeds")
  expect_error(Alfcrop(10, 4, start = 8), "falls outside")
})

test_that("Alfdgram, Alfpae and Alfplddt are softmax bin expectations", {
  z <- af_arr(c(3, 3, 2), 0.9)
  W <- af_w(64, 2, 0.3)
  bins <- 2 + 20 / 63 * (0:63)
  r <- Alfdgram(z, W, dtrue = matrix(c(3, 5, 7, 5, 3, 9, 7, 9, 3), 3))
  p12 <- af_sm(W %*% (z[1, 2, ] + z[2, 1, ]))
  expect_equal(r$p[1, 2, ], as.numeric(p12), tolerance = 1e-12)
  expect_equal(r$dist[1, 2], sum(p12 * bins), tolerance = 1e-12)
  expect_equal(r$dist, t(r$dist), tolerance = 1e-12)
  loss <- mean(vapply(1:9, function(k) {
    i <- (k - 1) %% 3 + 1
    j <- (k - 1) %/% 3 + 1
    d <- matrix(c(3, 5, 7, 5, 3, 9, 7, 9, 3), 3)[i, j]
    -log(r$p[i, j, which.min(abs(d - bins))])
  }, 0))
  expect_equal(r$loss, loss, tolerance = 1e-12)
  expect_null(Alfdgram(z, af_w(3, 2, 1), bins = 1:3)$loss)
  pa <- Alfpae(z, W)
  expect_equal(pa$pae[2, 3], sum(af_sm(W %*% z[2, 3, ]) * (0.25 + 0.5 * 0:63)),
               tolerance = 1e-12)
  s <- rbind(c(0.1, 0.5, -0.3), c(1, -1, 0.2))
  w1 <- af_w(4, 3, 0.2)
  w2 <- af_w(4, 4, 0.5)
  w3 <- af_w(50, 4, 0.8)
  pl <- Alfplddt(s, w1, w2, w3, rtrue = c(10, 70))
  h <- pmax(w2 %*% pmax(w1 %*% af_ln(s[2, ]), 0), 0)
  p <- af_sm(w3 %*% h)
  expect_equal(pl$plddt[2], sum(p * (1 + 2 * 0:49)), tolerance = 1e-12)
  expect_equal(pl$loss, mean(c(-log(pl$p[1, 5]), -log(pl$p[2, 35]))), tolerance = 1e-12)
})

test_that("Alfembed builds pair and MSA embeddings", {
  tf <- rbind(c(1, 0), c(0, 1), c(0.5, 0.5))
  ri <- c(1, 2, 4)
  msa <- af_arr(c(2, 3, 2), 0.4)
  wa <- af_w(3, 2, 0.1)
  wb <- af_w(3, 2, 0.2)
  wrel <- af_w(3, 65, 0.05)
  wm <- af_w(2, 2, 0.6)
  wt <- af_w(2, 2, 0.9)
  r <- Alfembed(tf, ri, msa, wa, wb, wrel, wm, wt)
  oh <- numeric(65)
  oh[(1 - 4) + 33] <- 1
  expect_equal(r$z[1, 3, ], as.numeric(wa %*% tf[1, ] + wb %*% tf[3, ] + wrel %*% oh),
               tolerance = 1e-12)
  expect_equal(r$m[2, 3, ], as.numeric(wm %*% msa[2, 3, ] + wt %*% tf[3, ]),
               tolerance = 1e-12)
})

test_that("Alffape clamps the frame-aligned point error", {
  fp <- list(af_frame(c(0.1, 0.2, 0), c(0, 0, 0)), af_frame(c(0, 0.4, 0.1), c(1, 1, 0)))
  ft <- list(af_frame(c(0, 0, 0), c(0, 0, 0)), af_frame(c(0, 0.3, 0.1), c(1, 0, 0)))
  x <- rbind(c(1, 2, 0), c(0, 1, 3), c(20, 0, 0))
  xt <- rbind(c(1, 2.2, 0), c(0.1, 1, 3), c(0, 0, 0))
  r <- Alffape(fp, x, ft, xt)
  d <- outer(1:2, 1:3, Vectorize(function(i, j) {
    a <- t(fp[[i]]$R) %*% (x[j, ] - fp[[i]]$t)
    b <- t(ft[[i]]$R) %*% (xt[j, ] - ft[[i]]$t)
    sqrt(sum((a - b)^2) + 1e-4)
  }))
  expect_equal(r$d, d, tolerance = 1e-12)
  expect_equal(r$estimate, mean(pmin(d, 10)) / 10, tolerance = 1e-12)
})

test_that("Alfloss, Alfrcyl and Alfviol", {
  r <- Alfloss(1, 2, 3, 4, 5, expres = 6, viol = 7, phase = "finetuning", ncrop = 4)
  u <- 0.5 + 1 + 0.9 + 8 + 0.05 + 0.06 + 7
  expect_equal(r$unscaled, u, tolerance = 1e-12)
  expect_equal(r$estimate, 2 * u, tolerance = 1e-12)
  expect_equal(Alfloss(1, 2, 3, 4, 5, expres = 6)$unscaled, 0.5 + 1 + 0.9 + 8 + 0.05,
               tolerance = 1e-12)
  expect_error(Alfloss(1, 1, 1, 1, 1, phase = "x"), "training")
  rc <- Alfrcyl(c(3, 2, 1.5, 1), nprime = 2)
  expect_equal(c(rc$estimate, rc$average), c(2, 1.875))
  expect_equal(Alfrcyl(c(3, 1))$estimate, 2)
  expect_error(Alfrcyl(numeric(0)), "empty")
  expect_error(Alfrcyl(1:3, nprime = 4), "outside")
  v <- Alfviol(blen = c(1.5, 1.2), blen_lit = c(1.33, 1.33), blen_sigma = c(0.01, 0.01),
               cosang = c(0.1, -0.5), cosang_lit = c(0, 0), cosang_sigma = c(0.02, 0.02),
               dnb = c(1.5, 3.5), dnb_lit = c(3.4, 3.4))
  expect_equal(v$bondlength, mean(pmax(abs(c(0.17, -0.13)) - 0.12, 0)), tolerance = 1e-12)
  expect_equal(v$bondangle, mean(pmax(abs(c(0.1, -0.5)) - 0.24, 0)), tolerance = 1e-12)
  expect_equal(v$clash, 3.4 - 1.5 - 1.5, tolerance = 1e-12)
  expect_equal(Alfviol()$estimate, 0)
})

msa_ref <- function(m, wq, wk, wv, wg, wo, z = NULL, wb = NULL, mode = "row") {
  s <- dim(m)[1]
  n <- dim(m)[2]
  cc <- nrow(wq[[1]])
  mn <- apply(m, c(1, 2), af_ln)
  out <- array(0, c(s, n, nrow(wo)))
  for (si in 1:s) for (i in 1:n) {
    cat_ <- unlist(lapply(seq_along(wq), function(h) {
      q <- wq[[h]] %*% mn[, si, i]
      if (mode == "row") {
        lg <- vapply(1:n, function(j) sum(q * (wk[[h]] %*% mn[, si, j])) / sqrt(cc) +
                       sum(wb[h, ] * af_ln(z[i, j, ])), 0)
        vv <- vapply(1:n, function(j) as.numeric(wv[[h]] %*% mn[, si, j]), numeric(cc))
      } else {
        lg <- vapply(1:s, function(t2) sum(q * (wk[[h]] %*% mn[, t2, i])) / sqrt(cc), 0)
        vv <- vapply(1:s, function(t2) as.numeric(wv[[h]] %*% mn[, t2, i]), numeric(cc))
      }
      af_sig(as.numeric(wg[[h]] %*% mn[, si, i])) * as.numeric(matrix(vv, cc) %*% af_sm(lg))
    }))
    out[si, i, ] <- wo %*% cat_
  }
  out
}

test_that("Alfmsaat row and column gated attention", {
  m <- af_arr(c(2, 3, 4), 0.37)
  z <- af_arr(c(3, 3, 2), 0.53)
  wq <- list(af_w(2, 4, 0.1), af_w(2, 4, 0.2))
  wk <- list(af_w(2, 4, 0.3), af_w(2, 4, 0.4))
  wv <- list(af_w(2, 4, 0.5), af_w(2, 4, 0.6))
  wg <- list(af_w(2, 4, 0.7), af_w(2, 4, 0.8))
  wo <- af_w(4, 4, 0.9)
  wb <- af_w(2, 2, 1.1)
  r <- Alfmsaat(m, wq, wk, wv, wg, wo, z = z, wb = wb, mode = "row")
  expect_equal(r$m, msa_ref(m, wq, wk, wv, wg, wo, z, wb, "row"), tolerance = 1e-12)
  cl <- Alfmsaat(m, wq, wk, wv, wg, wo, mode = "column")
  expect_equal(cl$m, msa_ref(m, wq, wk, wv, wg, wo, mode = "column"), tolerance = 1e-12)
  expect_equal(sum(cl$attn[1, 1, 1, ]), 1, tolerance = 1e-12)
  expect_error(Alfmsaat(m, wq, wk, wv, wg, wo, mode = "x"), "row")
  expect_error(Alfmsaat(m, wq, wk, wv, wg, wo, mode = "row"), "needs the pair")
})

opm_ref <- function(m, wa, wb, wo, ln = TRUE) {
  s <- dim(m)[1]
  n <- dim(m)[2]
  z <- array(0, c(n, n, nrow(wo)))
  for (i in 1:n) for (j in 1:n) {
    a <- t(vapply(1:s, function(si) as.numeric(wa %*% (if (ln) af_ln(m[si, i, ]) else m[si, i, ])),
                  numeric(nrow(wa))))
    b <- t(vapply(1:s, function(si) as.numeric(wb %*% (if (ln) af_ln(m[si, j, ]) else m[si, j, ])),
                  numeric(nrow(wb))))
    o <- crossprod(matrix(a, s), matrix(b, s)) / s
    z[i, j, ] <- wo %*% as.numeric(t(o))
  }
  z
}

test_that("Alfopm is the outer product mean", {
  m <- af_arr(c(3, 2, 3), 0.61)
  wa <- af_w(2, 3, 0.15)
  wb <- af_w(2, 3, 0.25)
  wo <- af_w(3, 4, 0.35)
  expect_equal(Alfopm(m, wa, wb, wo)$z, opm_ref(m, wa, wb, wo), tolerance = 1e-12)
  expect_equal(Alfopm(m, wa, wb, wo, layernorm = FALSE)$z, opm_ref(m, wa, wb, wo, FALSE),
               tolerance = 1e-12)
})

trimu_ref <- function(z, wag, wav, wbg, wbv, wg, wo, mode, ln = TRUE) {
  n <- dim(z)[1]
  zn <- if (ln) aperm(apply(z, c(1, 2), af_ln), c(2, 3, 1)) else z
  f <- function(i, j, W, V) af_sig(as.numeric(W %*% zn[i, j, ])) * as.numeric(V %*% zn[i, j, ])
  out <- array(0, c(n, n, nrow(wo)))
  for (i in 1:n) for (j in 1:n) {
    s <- Reduce(`+`, lapply(1:n, function(k) {
      if (mode == "outgoing") f(i, k, wag, wav) * f(j, k, wbg, wbv)
      else f(k, i, wag, wav) * f(k, j, wbg, wbv)
    }))
    if (ln) s <- af_ln(s)
    out[i, j, ] <- af_sig(as.numeric(wg %*% zn[i, j, ])) * as.numeric(wo %*% s)
  }
  out
}

test_that("Alftrimu outgoing and incoming triangle updates", {
  z <- af_arr(c(3, 3, 3), 0.47)
  a <- list(af_w(2, 3, 0.1), af_w(2, 3, 0.2), af_w(2, 3, 0.3), af_w(2, 3, 0.4),
            af_w(3, 3, 0.5), af_w(3, 2, 0.6))
  for (md in c("outgoing", "incoming")) {
    r <- Alftrimu(z, a[[1]], a[[2]], a[[3]], a[[4]], a[[5]], a[[6]], mode = md)
    expect_equal(r$z, trimu_ref(z, a[[1]], a[[2]], a[[3]], a[[4]], a[[5]], a[[6]], md),
                 tolerance = 1e-12)
  }
  r0 <- Alftrimu(z, a[[1]], a[[2]], a[[3]], a[[4]], a[[5]], a[[6]], layernorm = FALSE)
  expect_equal(r0$z, trimu_ref(z, a[[1]], a[[2]], a[[3]], a[[4]], a[[5]], a[[6]],
                               "outgoing", FALSE), tolerance = 1e-12)
  expect_error(Alftrimu(z, a[[1]], a[[2]], a[[3]], a[[4]], a[[5]], a[[6]], mode = "x"),
               "outgoing")
})

triat_ref <- function(z, wq, wk, wv, wb, wg, wo, mode) {
  n <- dim(z)[1]
  cc <- nrow(wq[[1]])
  zn <- aperm(apply(z, c(1, 2), af_ln), c(2, 3, 1))
  out <- array(0, c(n, n, nrow(wo)))
  for (i in 1:n) for (j in 1:n) {
    cat_ <- unlist(lapply(seq_along(wq), function(h) {
      q <- wq[[h]] %*% zn[i, j, ]
      lg <- vapply(1:n, function(k) {
        if (mode == "starting") {
          sum(q * (wk[[h]] %*% zn[i, k, ])) / sqrt(cc) + sum(wb[h, ] * zn[j, k, ])
        } else {
          sum(q * (wk[[h]] %*% zn[k, j, ])) / sqrt(cc) + sum(wb[h, ] * zn[k, i, ])
        }
      }, 0)
      vv <- vapply(1:n, function(k) {
        as.numeric(wv[[h]] %*% (if (mode == "starting") zn[i, k, ] else zn[k, j, ]))
      }, numeric(cc))
      af_sig(as.numeric(wg[[h]] %*% zn[i, j, ])) * as.numeric(matrix(vv, cc) %*% af_sm(lg))
    }))
    out[i, j, ] <- wo %*% cat_
  }
  out
}

test_that("Alftriat starting and ending node attention", {
  z <- af_arr(c(3, 3, 2), 0.29)
  wq <- list(af_w(2, 2, 0.1))
  wk <- list(af_w(2, 2, 0.2))
  wv <- list(af_w(2, 2, 0.3))
  wg <- list(af_w(2, 2, 0.4))
  wb <- af_w(1, 2, 0.5)
  wo <- af_w(2, 2, 0.6)
  for (md in c("starting", "ending")) {
    r <- Alftriat(z, wq, wk, wv, wb, wg, wo, mode = md)
    expect_equal(r$z, triat_ref(z, wq, wk, wv, wb, wg, wo, md), tolerance = 1e-12)
  }
  expect_error(Alftriat(z, wq, wk, wv, wb, wg, wo, mode = "x"), "starting")
})

test_that("Alftmpl attends over templates per pair", {
  tt <- af_arr(c(2, 2, 2, 3), 0.21)
  z <- af_arr(c(2, 2, 2), 0.33)
  wq <- list(af_w(2, 2, 0.1))
  wk <- list(af_w(2, 3, 0.2))
  wv <- list(af_w(2, 3, 0.3))
  wo <- af_w(2, 2, 0.4)
  r <- Alftmpl(tt, z, wq, wk, wv, wo)
  q <- wq[[1]] %*% z[1, 2, ]
  lg <- vapply(1:2, function(st) sum(q * (wk[[1]] %*% tt[st, 1, 2, ])) / sqrt(2), 0)
  vv <- vapply(1:2, function(st) as.numeric(wv[[1]] %*% tt[st, 1, 2, ]), numeric(2))
  expect_equal(r$z[1, 2, ], as.numeric(wo %*% (vv %*% af_sm(lg))), tolerance = 1e-12)
})

test_that("Alfstrtr, Alfrecyc and Alfschn", {
  s <- rbind(c(0.3, -0.2), c(1, 0.4))
  w1 <- af_w(3, 2, 0.2)
  w2 <- af_w(3, 3, 0.4)
  w3 <- af_w(2, 3, 0.6)
  r <- Alfstrtr(s, w1, w2, w3)
  u <- s[2, ] + as.numeric(w3 %*% pmax(w2 %*% pmax(w1 %*% s[2, ], 0), 0))
  expect_equal(r$s[2, ], af_ln(u), tolerance = 1e-12)
  rd <- Alfstrtr(s, w1, w2, w3, layernorm = FALSE, drop = rbind(c(1, 0), c(0, 2)))
  expect_equal(rd$s[2, ], u * c(0, 2), tolerance = 1e-12)
  z <- af_arr(c(2, 2, 3), 0.7)
  m1 <- rbind(c(1, 2, 4), c(0, -1, 3))
  x <- rbind(c(0, 0, 0), c(3, 4, 0))
  wd <- af_w(3, 15, 0.1)
  rr <- Alfrecyc(m1, z, x, wd)
  bins <- 3.375 + 18 / 14 * (0:14)
  oh <- numeric(15)
  oh[which.min(abs(5 - bins))] <- 1
  expect_equal(rr$z[1, 2, ], as.numeric(wd %*% oh) + af_ln(z[1, 2, ]), tolerance = 1e-12)
  expect_equal(rr$m1[2, ], af_ln(m1[2, ]), tolerance = 1e-12)
  r2 <- Alfrecyc(m1, z, x, wd, ncycle = 2)
  oh0 <- numeric(15)
  oh0[1] <- 1
  expect_equal(r2$z[1, 1, ], as.numeric(wd %*% oh0) + af_ln(rr$z[1, 1, ]), tolerance = 1e-12)
  fr <- list(af_frame(c(0.1, 0, 0.2), c(1, 0, 0)))
  lit <- list(af_frame(c(0, 0, 0), c(0, 1, 0)), af_frame(c(0.2, 0, 0), c(1, 1, 0)))
  ang <- array(c(1, 0.6, 0, 0.8), c(1, 2, 2))
  lx <- rbind(c(1, 0, 0), c(0, 1, 1))
  sc <- Alfschn(fr, ang, lit, parent = c(0, 1), litx = lx, frameof = c(1, 2))
  rx <- function(a) {
    cs <- a[1] / sqrt(sum(a^2))
    sn <- a[2] / sqrt(sum(a^2))
    rbind(c(1, 0, 0), c(0, cs, -sn), c(0, sn, cs))
  }
  comp <- function(A, B) list(R = A$R %*% B$R, t = as.numeric(A$R %*% B$t) + A$t)
  f1 <- comp(comp(fr[[1]], lit[[1]]), list(R = rx(ang[1, 1, ]), t = c(0, 0, 0)))
  f2 <- comp(comp(f1, lit[[2]]), list(R = rx(ang[1, 2, ]), t = c(0, 0, 0)))
  expect_equal(sc$x[1, 2, ], as.numeric(f2$R %*% lx[2, ]) + f2$t, tolerance = 1e-12)
  expect_error(Alfschn(fr, ang, lit, parent = c(2, 1), litx = lx, frameof = c(1, 2)),
               "before its parent")
})

test_that("Alfipa combines scalar, pair and point attention", {
  n <- 3
  s <- rbind(c(0.2, 0.5), c(-0.3, 0.1), c(0.4, -0.6))
  z <- af_arr(c(3, 3, 2), 0.41)
  fr <- list(af_frame(c(0.1, 0, 0), c(0, 0, 0)), af_frame(c(0, 0.2, 0), c(1, 0, 0)),
             af_frame(c(0, 0, 0.3), c(0, 1, 1)))
  wq <- list(af_w(2, 2, 0.1))
  wk <- list(af_w(2, 2, 0.2))
  wv <- list(af_w(2, 2, 0.3))
  wqp <- list(list(af_w(3, 2, 0.4)))
  wkp <- list(list(af_w(3, 2, 0.5)))
  wvp <- list(list(af_w(3, 2, 0.6)))
  wb <- af_w(1, 2, 0.7)
  wo <- af_w(2, 2 + 2 + 3 + 1, 0.8)
  r <- Alfipa(s, z, fr, wq, wk, wv, wqp, wkp, wvp, wb, gamma = 0.9, wo = wo)
  ap <- function(f, x) as.numeric(f$R %*% x) + f$t
  gq <- t(vapply(1:n, function(i) ap(fr[[i]], wqp[[1]][[1]] %*% s[i, ]), numeric(3)))
  gk <- t(vapply(1:n, function(i) ap(fr[[i]], wkp[[1]][[1]] %*% s[i, ]), numeric(3)))
  gv <- t(vapply(1:n, function(i) ap(fr[[i]], wvp[[1]][[1]] %*% s[i, ]), numeric(3)))
  i <- 2
  lg <- vapply(1:n, function(j) sqrt(1 / 3) * (sum((wq[[1]] %*% s[i, ]) * (wk[[1]] %*% s[j, ])) /
    sqrt(2) + sum(wb[1, ] * z[i, j, ]) - 0.5 * 0.9 * sqrt(2 / 9) * sum((gq[i, ] - gk[j, ])^2)), 0)
  a <- af_sm(lg)
  expect_equal(r$attn[[1]][i, ], a, tolerance = 1e-12)
  pt <- as.numeric(t(fr[[i]]$R) %*% (colSums(a * gv) - fr[[i]]$t))
  vs <- as.numeric(wv[[1]] %*% t(s))
  feat <- c(colSums(a * z[i, , ]), as.numeric(matrix(vs, 2) %*% a), pt, sqrt(sum(pt^2)))
  expect_equal(r$s[i, ], as.numeric(wo %*% feat), tolerance = 1e-12)
})

test_that("Alfevo composes the Evoformer sub-blocks", {
  m <- af_arr(c(2, 2, 2), 0.3)
  z <- af_arr(c(2, 2, 2), 0.5)
  k <- 0
  nw <- function(r, c) {
    k <<- k + 0.13
    af_w(r, c, k)
  }
  w <- list(rowq = list(nw(2, 2)), rowk = list(nw(2, 2)), rowv = list(nw(2, 2)),
            rowg = list(nw(2, 2)), rowo = nw(2, 2), rowb = nw(1, 2),
            colq = list(nw(2, 2)), colk = list(nw(2, 2)), colv = list(nw(2, 2)),
            colg = list(nw(2, 2)), colo = nw(2, 2), mt1 = nw(3, 2), mt2 = nw(2, 3),
            opa = nw(2, 2), opb = nw(2, 2), opo = nw(2, 4),
            tmoag = nw(2, 2), tmoav = nw(2, 2), tmobg = nw(2, 2), tmobv = nw(2, 2),
            tmog = nw(2, 2), tmoo = nw(2, 2),
            tmiag = nw(2, 2), tmiav = nw(2, 2), tmibg = nw(2, 2), tmibv = nw(2, 2),
            tmig = nw(2, 2), tmio = nw(2, 2),
            tasq = list(nw(2, 2)), task = list(nw(2, 2)), tasv = list(nw(2, 2)),
            tasb = nw(1, 2), tasg = list(nw(2, 2)), taso = nw(2, 2),
            taeq = list(nw(2, 2)), taek = list(nw(2, 2)), taev = list(nw(2, 2)),
            taeb = nw(1, 2), taeg = list(nw(2, 2)), taeo = nw(2, 2),
            pt1 = nw(3, 2), pt2 = nw(2, 3), sout = nw(3, 2))
  tr <- function(x, w1, w2) as.numeric(w2 %*% pmax(w1 %*% af_ln(x), 0))
  drop <- list(row = 0, trimulout = 0, trimulin = 0, triattnstart = 0, triattnend = 0)
  r <- Alfevo(m, z, w, drop = drop)
  mm <- m + msa_ref(m, w$colq, w$colk, w$colv, w$colg, w$colo, mode = "column")
  for (si in 1:2) for (i in 1:2) mm[si, i, ] <- mm[si, i, ] + tr(mm[si, i, ], w$mt1, w$mt2)
  zz <- z + opm_ref(mm, w$opa, w$opb, w$opo)
  for (i in 1:2) for (j in 1:2) zz[i, j, ] <- zz[i, j, ] + tr(zz[i, j, ], w$pt1, w$pt2)
  expect_equal(r$m, mm, tolerance = 1e-12)
  expect_equal(r$z, zz, tolerance = 1e-12)
  expect_equal(r$s[2, ], as.numeric(w$sout %*% mm[1, 2, ]), tolerance = 1e-12)
  full <- Alfevo(m, z, w)
  m1 <- m + msa_ref(m, w$rowq, w$rowk, w$rowv, w$rowg, w$rowo, z, w$rowb, "row")
  m1 <- m1 + msa_ref(m1, w$colq, w$colk, w$colv, w$colg, w$colo, mode = "column")
  for (si in 1:2) for (i in 1:2) m1[si, i, ] <- m1[si, i, ] + tr(m1[si, i, ], w$mt1, w$mt2)
  expect_equal(full$m, m1, tolerance = 1e-12)
  z1 <- z + opm_ref(m1, w$opa, w$opb, w$opo)
  z1 <- z1 + trimu_ref(z1, w$tmoag, w$tmoav, w$tmobg, w$tmobv, w$tmog, w$tmoo, "outgoing")
  z1 <- z1 + trimu_ref(z1, w$tmiag, w$tmiav, w$tmibg, w$tmibv, w$tmig, w$tmio, "incoming")
  z1 <- z1 + triat_ref(z1, w$tasq, w$task, w$tasv, w$tasb, w$tasg, w$taso, "starting")
  z1 <- z1 + triat_ref(z1, w$taeq, w$taek, w$taev, w$taeb, w$taeg, w$taeo, "ending")
  for (i in 1:2) for (j in 1:2) z1[i, j, ] <- z1[i, j, ] + tr(z1[i, j, ], w$pt1, w$pt2)
  expect_equal(full$z, z1, tolerance = 1e-12)
})

test_that("morie_alfmpv pairs MSA hits by species", {
  a <- list(species = c("h", "m", "h", "y"), identity = c(0.5, 0.9, 0.8, 0.4),
            evalue = c(1e-3, 1e-5, 1e-9, 1e-2), coverage = c(0.9, 0.3, 0.8, 0.6),
            gaps = c(0.1, 0.95, 0.2, 0.3))
  b <- list(species = c("m", "h", "h", "h"), identity = c(0.7, 0.6, 0.9, 0.2),
            evalue = c(1e-4, 1e-6, 1e-3, 1e-8), coverage = c(0.9, 0.9, 0.2, 0.9),
            gaps = c(0.2, 0.1, 0.1, 0.5))
  r <- morie_alfmpv_msa_pairing(list(a, b))
  # multimer: species in first-seen order, hits by descending identity
  expect_equal(r$species_paired, c("h", "h", "m"))
  expect_equal(r$paired, list(c(2L, 2L), c(0L, 1L), c(1L, 0L)))
  expect_equal(r$unpaired, list(3L, 3L))
  expect_equal(r$n_rows, 5L)
  cf <- morie_alfmpv(msas = list(a, b), mode = "colabfold")
  # coverage >= 0.5 drops a[2] and b[3]; best e-value per species
  expect_equal(cf$paired, list(c(2L, 3L)))
  expect_equal(cf$n_filtered, c(1L, 1L))
  fd <- morie_alfmpv(list(a, b), mode = "folddock")
  expect_equal(fd$paired, list(c(0L, 1L)))
  cp <- morie_alfmpv_msa_pairing(list(a, b), copies = c(2, 1), max_pairs = 1)
  expect_equal(cp$chain_source, c(0L, 0L, 1L))
  expect_equal(cp$n_paired, 1L)
  at <- morie_alfmpv_msa_pairing(list(c("h", "m"), c("m")))
  expect_equal(at$paired, list(c(1L, 0L)))
  expect_error(morie_alfmpv(), "give the per-chain")
  expect_error(morie_alfmpv_msa_pairing(list(a), mode = "x"), "expected one of")
  expect_error(morie_alfmpv_msa_pairing(list()), "no chains")
  expect_error(morie_alfmpv_msa_pairing(list(a), copies = c(1, 2)), "copies has")
  expect_error(morie_alfmpv_msa_pairing(list(a), copies = 0), "at least 1")
  expect_error(morie_alfmpv_msa_pairing(list(list(identity = 1))), "no species")
  expect_error(morie_alfmpv_msa_pairing(list(list(species = "h", identity = 1:2))),
               "has 2 entries")
})
