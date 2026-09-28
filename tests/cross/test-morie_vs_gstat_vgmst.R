skip_if_not_installed("gstat")

test_that("StModelVariogram matches gstat::variogramSurface for every vgmST model", {
  g <- data.frame(spacelag = c(0, 10, 50, 120, 300, 0, 75), timelag = c(0, 1, 3, 10, 2, 4, 0))
  sp <- list(psill = 2, model = "Exp", range = 100, nugget = 0.5)
  tm <- list(psill = 3, model = "Sph", range = 5)
  jt <- list(psill = 1.5, model = "Gau", range = 40)
  gv <- function(m) gstat::vgm(m$psill, m$model, m$range, if (is.null(m$nugget)) 0 else m$nugget)
  ref <- gstat::variogramSurface(gstat::vgmST("productSum", space = gv(sp), time = gv(tm), k = 0.2), g)$gamma
  expect_equal(StModelVariogram(g$spacelag, g$timelag, "productSum", space = sp, time = tm, k = 0.2), ref, tolerance = 1e-13)
  ref <- gstat::variogramSurface(gstat::vgmST("metric", joint = gv(jt), stAni = 10), g)$gamma
  expect_equal(StModelVariogram(g$spacelag, g$timelag, "metric", joint = jt, stani = 10), ref, tolerance = 1e-13)
  ref <- gstat::variogramSurface(gstat::vgmST("sumMetric", space = gv(sp), time = gv(tm), joint = gv(jt), stAni = 10),
                                 g)$gamma
  expect_equal(StModelVariogram(g$spacelag, g$timelag, "sumMetric", space = sp, time = tm, joint = jt, stani = 10), ref,
               tolerance = 1e-13)
  us <- list(psill = 0.8, model = "Exp", range = 100, nugget = 0.2)
  ut <- list(psill = 1, model = "Sph", range = 5)
  ref <- gstat::variogramSurface(gstat::vgmST("separable", space = gv(us), time = gv(ut), sill = 4), g)$gamma
  expect_equal(StModelVariogram(g$spacelag, g$timelag, "separable", space = us, time = ut, sill = 4), ref, tolerance = 1e-13)
})
