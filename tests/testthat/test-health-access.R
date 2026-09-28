.ha_u <- .morie_random_uniform(300, seed = 61, stream = 0)
.ha_x <- round(5 + 40 * .ha_u[1:25], 3)
.ha_D <- matrix(0.5 + 12 * .ha_u[51:200], 25, 6, byrow = TRUE)
.ha_P <- round(100 + 900 * .ha_u[201:225])
.ha_S <- round(2 + 10 * .ha_u[241:246])

test_that("SpatialGini and TheilDecomposition equal ineq; FCA equals SpatialAcc", {
  expect_equal(SpatialGini(.ha_x)$gini, 0.28905566578122205, tolerance = 1e-12)
  expect_equal(unlist(TheilDecomposition(.ha_x)[c("T", "L")]), c(T = 0.13685663909645407,
      L = 0.16279294454452048), tolerance = 1e-12)
  expect_equal(FcaAccessibility(.ha_S, .ha_P, .ha_D, 6)$access, c(0.006118154889136125,
      0.0017409470752089136, 0.006230173314600265, 0, 0.007139047118645196, 0.003821835064734945,
      0.004489226239391352, 0.006092611499163783, 0.005343334009882281, 0.003674221999408312,
      0.005411660183971829, 0.004662382694690327, 0.001727386934673367, 0.001727386934673367,
      0.007139047118645196, 0.002761839304717985, 0.005192212053910251, 0.001589825119236884,
      0.006079051358628236, 0.0043551733146002655, 0.0026277863799268985, 0.002614226239391352,
      0.0028009428352258735, 0.003317212053910251, 0.0036877821399438587), tolerance = 1e-12)
  expect_equal(FcaAccessibility(.ha_S, .ha_P, .ha_D, 6, "KD2SFCA", power = 0.5)$access,
      c(0.005938275317265873, 0.0006632390592284051, 0.006763721788652434, 0, 0.0059198995186280755,
      0.0044434753477125034, 0.0036419141273981884, 0.005032285277855135, 0.004383656233573718,
      0.002056032590965433, 0.005527456026919067, 0.004422538546489696, 0.0007319644324769469,
      0.0010018734789090115, 0.007834537675629104, 0.00333751171158587, 0.006845756887772577,
      0.002527481764247039, 0.0077509856689676575, 0.005110848055497405, 0.0036959095066541837,
      0.003107423387090804, 0.0023379004490593357, 0.003114540669580374, 0.0036687926909934527),
               tolerance = 1e-12)
  expect_equal(GravityAccessibility(.ha_S, .ha_D, 0.3), as.vector(exp(-0.3 * .ha_D) %*% .ha_S))
})

test_that("indices and access measures by hand", {
  expect_equal(HealthConcentrationIndex(c(4, 3, 2, 1), c(10, 20, 30, 40))$index, -0.25)
  expect_equal(HealthConcentrationIndex(1:3, c(5, 5, 9))$fractional_rank, c(1 / 3, 1 / 3, 2.5 / 3))
  W <- (abs(outer(1:4, 1:4, `-`)) == 1) * 1
  expect_equal(SpatialGini(1:4, W)$neighbour, 6 / 80)
  t <- TheilDecomposition(1:4, c(0, 0, 1, 1))
  expect_equal(t$within + t$between, t$T)
  expect_equal(DeprivationIndex(rbind(c(1, 2), c(3, 2), c(5, 8)), "sum")$score,
               c(-1 - 1 / sqrt(3), -1 / sqrt(3), 1 + 2 / sqrt(3)))
  e <- FcaAccessibility(c(10, 5), c(100, 200, 100), rbind(c(1, 5), c(2, 2), c(5, 1)), 3, "E2SFCA",
                        steps = list(c(1.5, 1), c(3, 0.5)))
  expect_equal(e$ratio, c(0.05, 0.025))
  expect_equal(NearestFacility(rbind(c(3, 1, 2), c(0.5, 4, 4)))$distance, c(1, 0.5))
  Tm <- RadiationFlows(c(10, 20, 30), rbind(c(0, 0), c(1, 0), c(3, 0)))
  expect_equal(Tm[1, 2:3], c(10 * 10 * 20 / (10 * 30), 10 * 10 * 30 / (30 * 60)))
  expect_error(FcaAccessibility(.ha_S, .ha_P, .ha_D, 6, "E2SFCA"), "steps")
})
