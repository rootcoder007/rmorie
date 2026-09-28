.idw_u <- .morie_random_uniform(200, seed = 91, stream = 0)
.idw_P <- cbind(10 * .idw_u[1:30], 10 * .idw_u[31:60])
.idw_z <- round(5 + 2 * .idw_u[61:90] + 0.3 * .idw_P[, 1], 4)
.idw_Q <- rbind(c(1.5, 2.5), c(5, 5), c(8.2, 1.1), .idw_P[4, ])

test_that("IdwPredict and IdwCv equal gstat idw and krige.cv", {
  expect_equal(IdwPredict(.idw_z, .idw_P, .idw_Q)$prediction, c(7.0091797675761915, 7.822991807235447,
      7.895743019251255, 6.9981), tolerance = 1e-12)
  expect_equal(IdwPredict(.idw_z, .idw_P, .idw_Q, nmax = 5)$prediction, c(6.975626522293049,
      7.96126637950984, 8.063331143854155, 6.9981), tolerance = 1e-12)
  expect_equal(IdwPredict(.idw_z, .idw_P, .idw_Q, maxdist = 2.5, power = 3.5)$prediction,
      c(7.1687919495442864, 8.098861519735607, 7.9072, 6.9981),
               tolerance = 1e-12)
  blk <- as.matrix(expand.grid(c(-0.25, 0.25), c(-0.25, 0.25)))[c(1, 3, 2, 4), ]
  expect_equal(IdwPredict(.idw_z, .idw_P, .idw_Q[1:3, ], block = blk)$prediction, c(6.936873352239505,
      7.831999328698995, 7.897477714678273), tolerance = 1e-12)
  expect_equal(IdwCv(.idw_z, .idw_P, nmax = 8)$prediction, c(7.913682816512851, 6.621255599918106,
      8.219070266439596, 6.666507319762718, 8.377411986498435, 6.271365210252634, 7.4402581630917295,
      5.649752385808979, 6.582576555662915, 6.545797894958416, 6.934940754238349, 8.169372176737323,
      7.840597955096391, 7.519817473187683, 7.851765755774197, 6.307418572947136, 7.494105019000853,
      7.178563524844677, 7.922234895005098, 8.489566653945365, 8.130178242173036, 7.932092854262572,
      6.871205829668301, 8.387088232428907, 7.075681079864747, 8.086800231270045, 7.452642558455051,
      7.67499294126524, 6.676451261581935, 5.793414544756043), tolerance = 1e-12)
})

test_that("Shepard weights and anisotropy by hand", {
  w <- c(((3 - 0.5) / (3 * 0.5))^2, ((3 - 1.5) / (3 * 1.5))^2)
  expect_equal(ShepardPredict(c(1, 3), rbind(c(0, 0), c(2, 0)), rbind(c(0.5, 0)), radius = 3)$prediction,
               sum(w * c(1, 3)) / sum(w))
  a <- IdwPredict(c(1, 3), rbind(c(0, 1), c(1, 0)), rbind(c(0, 0)), angle = 0, ratio = 0.5)$prediction
  expect_equal(a, (0.25 * 1 + 1 * 3) / 1.25)
  expect_error(ShepardPredict(1, rbind(c(0, 0)), rbind(c(1, 1))), "exactly one")
})
