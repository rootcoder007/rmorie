# Extracted from test-multiple-point-simulation.R:7

# test -------------------------------------------------------------------------
ti <- outer(0:17, 0:17, function(i, j) as.numeric((i %/% 3 + j %/% 3) %% 2 == 0))
cond <- rbind(c(0, 0, 1), c(4, 5, 0))
r <- DirectSampling(ti, 9, 9, n_neighbors = 8, threshold = 0, max_fraction = 1, conditioning = cond, seed = 2)
expect_equal(c(r$grid[1, 1], r$grid[5, 6]), c(1, 0))
expect_true(all(r$grid %in% c(0, 1)))
expect_equal(s$grid[3, 3], 0)
