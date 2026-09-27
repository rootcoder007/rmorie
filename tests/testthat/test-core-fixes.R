cf_i <- 0:19
cf_x <- sin(1.1 * cf_i) + 0.1 * cf_i
cf_x2 <- cos(0.7 * cf_i)
cf_y <- 1 + 0.5 * cf_x - 0.3 * cf_x2 + 0.2 * sin(3.3 * cf_i)
cf_cl <- cf_i %/% 4

test_that("morie_gee_regression matches geepack at tight tolerance", {
  r <- morie_gee_regression(cf_y, cf_x, cf_cl, corr_structure = "exchangeable", tol = 1e-12, max_iter = 200)
  expect_equal(unname(r$coefficients), c(1.03801472401938, 0.436513717677338), tolerance = 1e-8)
  expect_equal(unname(r$se), c(0.108147011877728, 0.0695202196566055), tolerance = 1e-7)
  expect_equal(r$alpha, 0.364548110491062, tolerance = 1e-7)
  r <- morie_gee_regression(cf_y, cf_x, cf_cl, corr_structure = "ar1", tol = 1e-12, max_iter = 200)
  expect_equal(unname(r$coefficients), c(1.025167284833, 0.449627311438093), tolerance = 1e-8)
  expect_equal(r$alpha, 0.517380259417559, tolerance = 1e-7)
  r <- morie_gee_regression(round(3 + 2 * abs(cf_y)), cf_x, cf_cl, family = "poisson", tol = 1e-12,
                            max_iter = 200)
  expect_equal(unname(r$coefficients), c(1.61423684477466, 0.146132417456451), tolerance = 1e-9)
  expect_equal(unname(r$se), c(0.0502981825287144, 0.0297813134314384), tolerance = 1e-8)
})

test_that("morie_concordance_corr matches DescTools::CCC", {
  r <- morie_concordance_corr(cf_x, cf_y)
  expect_equal(r$estimate, 0.59824339226692769, tolerance = 1e-14)
  expect_equal(c(r$ci_lower, r$ci_upper), c(0.38254267739764947, 0.75210290764044829), tolerance = 1e-13)
  expect_equal(r$asymptotic_ci, c(0.41372033522548701, 0.78276644930836836), tolerance = 1e-13)
})

test_that("morie_dunnett_test p-values equal mvtnorm::pmvt", {
  r <- morie_dunnett_test(c(5.1, 4.8, 5.5, 5.0, 4.9, 5.2), c(5.9, 6.1, 5.7, 6.3),
                          c(5.2, 5.4, 4.9, 5.6, 5.3, 5.0, 5.7))
  expect_equal(r$comparisons$t, c(5.23979288689629, 1.43695157250271), tolerance = 1e-12)
  expect_equal(r$comparisons$p_adj, c(0.000241724598797788, 0.290411591498084), tolerance = 1e-9)
})

test_that("morie_anova_twoway equals anova(lm()) on unbalanced data", {
  d <- data.frame(y = c(3.1, 4.2, 5.0, 3.6, 4.9, 5.8, 2.2, 2.9, 4.4, 2.7, 3.3, 4.1, 3.9, 2.6),
                  a = c(rep("p", 6), rep("q", 8)), b = c(rep(c("u", "v", "w"), 4), "u", "w"))
  r <- morie_anova_twoway(d)
  expect_equal(c(r$F, r$f_b, r$ss_resid), c(8.94718115906096, 3.90146001067526, 5.25311764705882),
               tolerance = 1e-12)
  r <- morie_anova_twoway(d, interaction = TRUE)
  expect_equal(c(r$F, r$f_ab, r$p_ab), c(9.04582426394778, 1.05512524175668, 0.392023666436377),
               tolerance = 1e-11)
})

test_that("morie_proportion_ci offers agresti-coull and the clopper-pearson alias", {
  z <- stats::qnorm(0.975)
  nt <- 25 + z^2
  pt <- (7 + z^2 / 2) / nt
  r <- morie_proportion_ci(7, 25, method = "agresti-coull")
  expect_equal(c(r$ci_lower, r$ci_upper), pt + c(-1, 1) * z * sqrt(pt * (1 - pt) / nt), tolerance = 1e-14)
  expect_equal(morie_proportion_ci(7, 25, method = "clopper-pearson"), morie_proportion_ci(7, 25, method = "exact"))
})

test_that("morie_gee_regression stops on a singular working correlation", {
  x <- sin(1:20)
  # every cluster has the same residual pattern: exchangeable alpha = -1/(m - 1) exactly, as geepack
  expect_error(morie_gee_regression(x + rep(c(0.2, -0.1, 0.3, 0), 5), x, rep(1:5, each = 4)),
               "working correlation is singular")
  y <- x + rep(c(0.2, -0.1, 0.3, 0), 5) + 0.3 * cos(7 * (1:20))
  expect_equal(morie_gee_regression(y, x, rep(1:5, each = 4))$alpha, -0.21085606432035597, tolerance = 1e-8)
})
