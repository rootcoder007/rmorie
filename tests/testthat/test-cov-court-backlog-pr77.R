# R/court_backlog.R, the line PR #77 added that no other test reached: one row of band shares is
# recycled over several windows.

test_that("a single band-share row applies to every window", {
  s <- c(0.808, 0.105, 0.034, 0.053)
  fl <- c(0, 304, 456, 548)
  out <- morie_backlog_published(c(21265, 20000), c(30, 31), c(148904, 150000), c(175, 180),
                                 band_shares = rbind(s), band_floors = fl)
  expect_equal(out$band_floor_bound, rep(sum(s * fl) / sum(s), 2))
  expect_equal(out$filing_rate, c(21265 / 30, 20000 / 31))
})
