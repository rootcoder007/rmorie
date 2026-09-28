test_that("MapProject and geodesics equal PROJ (sf) and geosphere", {
  skip_if_not_installed("sf")
  skip_if_not_installed("geosphere")
  u <- .morie_random_uniform(40, seed = 71, stream = 0)
  P <- cbind(-170 + 340 * u[1:10], -70 + 140 * u[11:20])
  ll <- "+proj=longlat +datum=WGS84"
  for (d in list(list("merc", "+proj=merc +datum=WGS84", list()),
                 list("tmerc", "+proj=tmerc +lon_0=15 +k_0=0.9996 +datum=WGS84", list(lon_0 = 15, k_0 = 0.9996)),
                 list("moll", "+proj=moll +R=6371000", list()), list("eck4", "+proj=eck4 +R=6371000", list()))) {
    src <- if (grepl("R=", d[[2]])) "+proj=longlat +R=6371000" else ll
    ours <- t(apply(P, 1, function(q) do.call(MapProject, c(list(q[1], q[2], d[[1]]), d[[3]]))))
    expect_equal(ours, sf::sf_project(src, d[[2]], P), tolerance = 1e-12, info = d[[1]])
  }
  v <- sapply(1:9, function(i) VincentyInverse(P[i, 1], P[i, 2], P[i + 1, 1], P[i + 1, 2])$distance)
  expect_equal(v, sapply(1:9, function(i) geosphere::distVincentyEllipsoid(P[i, ], P[i + 1, ])), tolerance = 1e-9)
})
