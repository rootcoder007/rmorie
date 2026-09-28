test_that("PolygonBoolean areas equal polyclip (Clipper)", {
  skip_if_not_installed("polyclip")
  u <- .morie_random_uniform(400, seed = 9, stream = 0)
  star <- function(cx, cy, r, k, off) {
    th <- 2 * pi * (0:(k - 1)) / k + off
    rad <- r * (0.6 + 0.4 * u[(off * 100) %% 300 + seq_len(k)])
    cbind(cx + rad * cos(th), cy + rad * sin(th))
  }
  cases <- list(list(star(0, 0, 3, 9, 0.1), star(1.5, 0.7, 2.5, 7, 0.37)),
                list(star(0, 0, 3, 12, 0.2), star(-1, 1.2, 2, 5, 0.9)))
  area <- function(rr) sum(vapply(rr, function(q) PolygonArea(cbind(q$x, q$y)), 0))
  for (cs in cases) {
    P <- cs[[1]]
    Q <- cs[[2]]
    lp <- list(x = P[, 1], y = P[, 2])
    lq <- list(x = Q[, 1], y = Q[, 2])
    expect_equal(PolygonBoolean(P, Q, "intersection")$area, area(polyclip::polyclip(lp, lq, op = "intersection")), tolerance = 1e-6)
    expect_equal(PolygonBoolean(P, Q, "union")$area, area(polyclip::polyclip(lp, lq, op = "union")), tolerance = 1e-6)
    expect_equal(PolygonBoolean(P, Q, "difference")$area, area(polyclip::polyclip(lp, lq, op = "minus")), tolerance = 1e-6)
  }
})
