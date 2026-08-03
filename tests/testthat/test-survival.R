# Parity for the survival block, anchored on the survival package.

TT <- c(5,8,12,3,15,7,20,11,4,18, 9,22,6,14,25,10,17,2,13,19,
        16,21,1,24,23,27,26,30,28,29, 31,33,32,35,34,37,36,39,38,40)
EV <- c(1,1,0,1,1,1,0,1,1,1, 0,1,1,1,0,1,1,1,0,1,
        1,0,1,1,1,0,1,1,1,0, 1,1,0,1,1,1,0,1,1,1)
GG <- rep(c(1,2), each = 20)
X1 <- sapply(0:39, function(i) ((i*7) %% 11)/5 - 1)
X2 <- sapply(0:39, function(i) ((i*5) %% 7)/3 - 1)
XX <- cbind(X1, X2)

test_that("Kaplan-Meier matches survfit", {
  km <- morie_kaplan_meier(TT, EV)
  expect_equal(length(km$time), 30)
  expect_equal(km$surv[1], 0.975, tolerance = 1e-14)
  i <- which(km$time == 10)
  expect_equal(km$surv[i], 0.774193548387097, tolerance = 1e-13)
  expect_equal(km$lower[i], 0.610837039143546, tolerance = 1e-12)
  expect_equal(km$upper[i], 0.87556658555096, tolerance = 1e-12)
  expect_equal(km$se_cumhaz[1], 0.0253184841770917, tolerance = 1e-14)
  expect_equal(km$se, km$surv * km$se_cumhaz, tolerance = 1e-14)
})

test_that("survival curve is monotone and inside [0, 1]", {
  km <- morie_kaplan_meier(TT, EV)
  expect_true(all(diff(km$surv) <= 1e-15))
  expect_true(all(km$lower >= 0 & km$upper <= 1))
})

test_that("Nelson-Aalen matches survfit type fh", {
  na <- morie_nelson_aalen(TT, EV)
  expect_equal(na$surv[1], 0.975309912028333, tolerance = 1e-13)
  expect_equal(na$surv[5], 0.876549922217358, tolerance = 1e-13)
  expect_true(all(na$surv >= morie_kaplan_meier(TT, EV)$surv - 1e-12))
})

test_that("log-rank matches survdiff", {
  r <- morie_logrank_test(TT, EV, GG)
  expect_equal(r$statistic, 20.5279554955505, tolerance = 1e-10)
  expect_equal(r$df, 1)
  expect_equal(r$p_value, 5.87666766058383e-06, tolerance = 1e-9)
  expect_equal(sum(r$observed), sum(r$expected), tolerance = 1e-9)
})

test_that("Cox matches coxph with Efron ties", {
  c <- morie_cox_ph(TT, EV, XX)
  expect_equal(unname(c$coef), c(-0.427673034112693, -0.157489759400814),
               tolerance = 1e-6)
  expect_equal(unname(c$se), c(0.304717144602484, 0.285683063051433),
               tolerance = 1e-8)
  expect_equal(c$loglik, -81.1317187602372, tolerance = 1e-9)
  expect_equal(c$loglik_null, -82.4119335691052, tolerance = 1e-9)
  expect_equal(unname(c$hazard_ratio), exp(unname(c$coef)),
               tolerance = 1e-14)
  expect_gte(c$loglik, c$loglik_null)
})

test_that("Efron and Breslow coincide without ties and differ with them", {
  a <- morie_cox_ph(TT, EV, XX, ties = "efron")$coef
  b <- morie_cox_ph(TT, EV, XX, ties = "breslow")$coef
  expect_equal(unname(a), unname(b), tolerance = 1e-7)
  tied <- pmin(TT, 10)
  a2 <- morie_cox_ph(tied, EV, XX, ties = "efron")$coef
  b2 <- morie_cox_ph(tied, EV, XX, ties = "breslow")$coef
  expect_gt(max(abs(a2 - b2)), 1e-6)
})

test_that("concordance matches survival::concordance", {
  c <- morie_cox_ph(TT, EV, XX)
  risk <- exp(as.numeric(XX %*% c$coef))
  expect_equal(morie_concordance_index(TT, EV, risk)$c_index,
               0.483443708609272, tolerance = 1e-11)
  expect_equal(morie_concordance_index(TT, EV, -TT)$c_index, 1,
               tolerance = 1e-12)
})

test_that("survival inputs are validated", {
  expect_error(morie_kaplan_meier(TT, EV[1:5]))
  expect_error(morie_kaplan_meier(TT, rep(2, length(TT))))
  expect_error(morie_logrank_test(TT, EV, rep(1, length(TT))))
  expect_error(morie_cox_ph(TT, rep(0, length(TT)), XX))
})
