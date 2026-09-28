test_that("crown segmentation equals itcSegment::itcIMG and gaps equal ForestGapR", {
  skip_if_not_installed("itcSegment")
  skip_if_not_installed("ForestGapR")
  skip_if_not_installed("terra")
  skip_if_not_installed("raster")
  u <- .morie_random_uniform(1200, seed = 12)
  nr <- 30
  nc <- 36
  xy <- expand.grid(c = 1:nc, r = 1:nr)
  tr <- cbind(u[1:12] * nc, u[13:24] * nr, 12 + 18 * u[25:36], 1.5 + 2 * u[37:48])
  chm <- matrix(0, nr, nc)
  for (k in 1:12) {
    chm <- pmax(chm, matrix(tr[k, 3] * exp(-((xy$c - tr[k, 1])^2 + (xy$r - tr[k, 2])^2) / (2 * tr[k, 4]^2)),
                            nr, nc, byrow = TRUE))
  }
  chm <- chm + matrix(u[49:(48 + nr * nc)] * 0.5, nr, nc)
  r <- terra::rast(chm, extent = terra::ext(0, nc, 0, nr))
  env <- new.env()
  # itcIMG returns hull polygons; capture its crown raster at exit
  suppressMessages(trace(itcSegment::itcIMG, exit = bquote(assign("CR", Crowns, envir = .(env))), print = FALSE,
                         where = asNamespace("itcSegment")))
  on.exit(suppressMessages(untrace(itcSegment::itcIMG, where = asNamespace("itcSegment"))))
  for (th in c(2, 8)) {
    try(suppressWarnings(itcSegment::itcIMG(r, epsg = 32632, th = th)), silent = TRUE)
    ours <- CrownSegmentation(chm, th = th, method = "itcsegment")
    expect_equal(ours$labels, t(env$CR)[nr:1, ], ignore_attr = TRUE)
  }
  for (thr in c(5, 10)) {
    g <- raster::values(ForestGapR::getForestGaps(raster::raster(r), threshold = thr, size = c(2, 1000)))
    ga <- as.vector(t(CanopyGaps(chm, thr, 2, 1000)$labels))
    ga[ga == 0] <- NA
    expect_identical(is.na(g), is.na(ga))
    tab <- table(g, ga)
    expect_true(all(rowSums(tab > 0) == 1) && all(colSums(tab > 0) == 1))
  }
})
