# Free-Wilson analysis: additive substituent contributions (Free & Wilson
# 1964).
#
# The model is a linear one in substituent indicators, so base R's lm is an
# exact anchor for the reference-coded fit. Beyond that: a dataset built to
# be perfectly additive must be fitted exactly, and the two constraints are
# reparametrisations of one model, so their fitted values must agree even
# though their coefficients do not.

# every combination of three groups at position one and two at position two
GRID <- expand.grid(p1 = c("H", "Me", "Cl"), p2 = c("H", "F"),
                    stringsAsFactors = FALSE)
CMP <- lapply(seq_len(nrow(GRID)),
              function(i) c(GRID$p1[i], GRID$p2[i]))
# a perfectly additive activity: 5 + contribution(p1) + contribution(p2)
A1 <- c(H = 0, Me = 1.5, Cl = -0.8)
A2 <- c(H = 0, F = 2.0)
ACT <- 5 + A1[GRID$p1] + A2[GRID$p2]
ACT <- as.numeric(ACT)

test_that("inputs are checked before anything is fitted", {
  expect_error(.frwil_prep(CMP, ACT[1:3]), "compounds but .* activities")
  expect_error(.frwil_prep(list(), numeric(0)), "no compounds given")
  # every compound has to describe the same positions
  expect_error(.frwil_prep(list(c("H", "H"), c("Me")), c(1, 2)),
               "same number of positions")
  expect_error(.frwil_prep(list(character(0)), 1), "same number of positions")
  p <- .frwil_prep(CMP, ACT)
  expect_equal(p$k, 2L)
  expect_length(p$C, 6L)
  expect_equal(p$y, ACT)
})

test_that("the design matrix is reference-coded indicators", {
  d <- .frwil_design_matrix(CMP, "reference")
  # an intercept plus two free groups at position one and one at position two
  expect_equal(d$names, c("intercept", "P1:Me", "P1:Cl", "P2:F"))
  expect_equal(dim(d$matrix), c(6L, 4L))
  expect_equal(d$matrix[, 1], rep(1, 6))
  expect_true(all(d$matrix %in% c(0, 1)))
  # the reference group is the one seen first at each position
  expect_equal(unname(vapply(d$groups, function(g) g[1], character(1))),
               c("H", "H"))
  # a compound of two reference groups is all-zero apart from the intercept
  i_ref <- which(GRID$p1 == "H" & GRID$p2 == "H")
  expect_equal(d$matrix[i_ref, ], c(1, 0, 0, 0))
  # and a Cl/F compound flags exactly those two columns
  i_cf <- which(GRID$p1 == "Cl" & GRID$p2 == "F")
  expect_equal(d$matrix[i_cf, ], c(1, 0, 1, 1))

  # the sum-zero coding keeps a column for every group instead
  ds <- .frwil_design_matrix(CMP, "sum_zero")
  expect_equal(ds$names,
               c("intercept", "P1:H", "P1:Me", "P1:Cl", "P2:H", "P2:F"))
  expect_equal(ncol(ds$matrix), 6L)

  expect_error(.frwil_design_matrix(CMP, "helmert"), "constraint must be one of")
  expect_error(.frwil_design_matrix(list(), "reference"), "no compounds given")
  # a position with a single group cannot be told apart from the intercept
  expect_error(.frwil_design_matrix(list(c("H", "H"), c("H", "F")),
                                    "reference"),
               "only the group")
})

test_that("the reference fit is base R's linear model", {
  set.seed(3)
  noisy <- ACT + rnorm(6, 0, 0.2)
  fit <- .frwil_free_wilson(CMP, noisy, "reference")
  # levels in order of appearance, matching the module's reference choice
  f1 <- factor(GRID$p1, levels = unique(GRID$p1))
  f2 <- factor(GRID$p2, levels = unique(GRID$p2))
  ref <- lm(noisy ~ f1 + f2)
  expect_equal(fit$beta, unname(coef(ref)), tolerance = 1e-9)
  expect_equal(fit$fitted, unname(fitted(ref)), tolerance = 1e-9)
  expect_equal(fit$residuals, unname(residuals(ref)), tolerance = 1e-9)
  expect_equal(fit$rss, sum(residuals(ref)^2), tolerance = 1e-9)
  expect_equal(fit$r_squared, summary(ref)$r.squared, tolerance = 1e-9)
  expect_equal(fit$sigma, summary(ref)$sigma, tolerance = 1e-9)
  expect_equal(fit$df_residual, ref$df.residual)
  expect_equal(fit$n_parameters, 4L)
  # the coefficients are also reported by name
  expect_named(fit$coefficients,
               c("intercept", "P1:Me", "P1:Cl", "P2:F"))
  expect_equal(fit$estimate, fit$coefficients)
  expect_equal(fit$constraint, "reference")
  expect_equal(fit$n_positions, 2L)
  expect_match(fit$method, "Free & Wilson")
})

test_that("a perfectly additive series is fitted exactly", {
  fit <- .frwil_free_wilson(CMP, ACT, "reference")
  # the activity was built by addition, so an additive model reproduces it
  expect_equal(fit$rss, 0, tolerance = 1e-18)
  expect_equal(fit$r_squared, 1)
  expect_equal(fit$fitted, ACT, tolerance = 1e-12)
  expect_equal(fit$residuals, rep(0, 6), tolerance = 1e-12)
  # and the recovered contributions are the ones used to build it
  expect_equal(fit$coefficients[["intercept"]], 5, tolerance = 1e-10)
  expect_equal(fit$coefficients[["P1:Me"]], 1.5, tolerance = 1e-10)
  expect_equal(fit$coefficients[["P1:Cl"]], -0.8, tolerance = 1e-10)
  expect_equal(fit$coefficients[["P2:F"]], 2.0, tolerance = 1e-10)
  # occurrence counts are the number of compounds carrying each group
  expect_equal(fit$occurrences[["P1:Me"]], 2L)
  expect_equal(fit$occurrences[["P2:F"]], 3L)
})

test_that("the two constraints are the same model reparametrised", {
  set.seed(5)
  noisy <- ACT + rnorm(6, 0, 0.3)
  a <- .frwil_free_wilson(CMP, noisy, "reference")
  b <- .frwil_free_wilson(CMP, noisy, "sum_zero")
  # different coefficients
  expect_false(length(a$beta) == length(b$beta))
  # but the same fit, because both span the same column space
  expect_equal(b$fitted, a$fitted, tolerance = 1e-8)
  expect_equal(b$rss, a$rss, tolerance = 1e-8)
  expect_equal(b$r_squared, a$r_squared, tolerance = 1e-8)
  expect_equal(b$df_residual, a$df_residual)
  expect_equal(b$n_parameters, a$n_parameters)
  # under sum_zero the occurrence-weighted contributions vanish at each
  # position, which is what the constraint asks for
  for (p in 1:2) {
    nm <- grep(sprintf("^P%d:", p), b$names, value = TRUE)
    w <- vapply(nm, function(x) b$occurrences[[x]], numeric(1))
    v <- vapply(nm, function(x) b$coefficients[[x]], numeric(1))
    expect_equal(sum(w * v), 0, tolerance = 1e-7)
  }
  expect_equal(b$constraint, "sum_zero")
})

test_that("prediction is the sum of the fitted contributions", {
  fit <- .frwil_free_wilson(CMP, ACT, "reference")
  # a compound from the training set predicts its own fitted value
  for (i in seq_len(6)) {
    p <- .frwil_predict_activity(fit, CMP[[i]])
    val <- if (is.list(p)) p$estimate else p
    expect_equal(as.numeric(val), fit$fitted[i], tolerance = 1e-10)
  }
  # and the additive prediction is the intercept plus the two contributions
  p <- .frwil_predict_activity(fit, c("Cl", "F"))
  val <- if (is.list(p)) p$estimate else p
  expect_equal(as.numeric(val), 5 - 0.8 + 2.0, tolerance = 1e-10)
  # the reference compound predicts the intercept
  p0 <- .frwil_predict_activity(fit, c("H", "H"))
  v0 <- if (is.list(p0)) p0$estimate else p0
  expect_equal(as.numeric(v0), 5, tolerance = 1e-10)
  # a compound describing the wrong number of positions is refused
  expect_error(.frwil_predict_activity(fit, c("Cl")),
               "lists 1 positions but the model has 2")
})
