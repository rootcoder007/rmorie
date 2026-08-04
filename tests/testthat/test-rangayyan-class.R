# Rangayyan pattern classification and CAD in R.  Same book equations and
# the same expected values as the Python arm.
#
# The book's Table 10.4 -- 18 abnormal VAG signals, direct playback
# against sonification -- is the oracle for the test of symmetry, and its
# stated 15/18 and 4/18 sensitivities are reproduced from that table.

TAB104 <- matrix(c(2, 0, 10, 1, 0, 1, 0, 0, 4), nrow = 3, byrow = TRUE)
XC <- matrix(c(1, 1, 1.4, 0.8, 0.7, 1.3, 1.1, 1.6,
               3, 3, 3.3, 2.7, 2.6, 3.2, 3.1, 3.5), ncol = 2, byrow = TRUE)
YC <- c(0, 0, 0, 0, 1, 1, 1, 1)
CC1 <- matrix(c(2, 0.3, 0.3, 1), 2, 2)
CC2 <- matrix(c(1, -0.2, -0.2, 3), 2, 2)

test_that("sensitivity eq (10.100) ignores false alarms", {
  r <- Sens(45, 5)
  expect_equal(r$sensitivity, 0.9)
  expect_equal(r$fnf, 0.1)
  expect_equal(Sens(50, 0)$sensitivity, 1)   # calling everyone positive
  expect_true(r$says_nothing_about_false_alarms)
})

test_that("specificity eq (10.101) is the mirror", {
  r <- Spec(40, 10)
  expect_equal(r$specificity, 0.8)
  expect_equal(r$specificity + r$fpf, 1)
  expect_equal(Sens(45, 5)$sensitivity + Sens(45, 5)$fnf, 1)
  t <- matrix(c(45, 5, 10, 40), 2, 2, byrow = TRUE)
  expect_equal(Sens(t)$sensitivity, 0.9)
  expect_equal(Spec(t)$specificity, 0.8)
})

test_that("PPV eq (10.106) depends on the prevalence", {
  r <- Ppv(45, 10)
  expect_equal(r$ppv, 45 / 55)
  expect_true(r$depends_on_prevalence)
  expect_lt(Ppv(45, 10, prevalence = 0.001, sensitivity = 0.9,
                specificity = 0.8)$ppv_at_prevalence, 0.01)
})

test_that("accuracy eq (10.102) is prevalence weighted", {
  r <- Accuracy(tp = 45, tn = 40, fp = 10, fn = 5)
  expect_equal(r$raw_accuracy, 0.85)
  expect_false(r$prior_weighted)
  w <- Accuracy(tp = 45, tn = 40, fp = 10, fn = 5, prevalence = 0.01)
  expect_equal(w$accuracy, 0.9 * 0.01 + 0.8 * 0.99)
  expect_true(w$prior_weighted)
  # eq (10.103) IS eq (10.102) at the test-set prevalence
  at <- Accuracy(tp = 45, tn = 40, fp = 10, fn = 5,
                 prevalence = r$test_set_prevalence)
  expect_equal(at$accuracy, r$raw_accuracy)
  expect_error(Accuracy(tp = 0, tn = 5, fp = 0, fn = 0), "empty")
})

test_that("the ROC area equals the Mann-Whitney statistic", {
  s <- c(0.9, 0.75, 0.7, 0.62, 0.55, 0.4, 0.35, 0.2)
  l <- c(1, 1, 0, 1, 0, 1, 0, 0)
  r <- Roc(s, l)
  expect_true(r$trapezoidal_equals_mann_whitney)
  expect_equal(r$auc, r$mann_whitney)
  expect_equal(Roc(c(0.9, 0.8, 0.2, 0.1), c(1, 1, 0, 0))$auc, 1)
  tied <- Roc(rep(0.5, 4), c(1, 1, 0, 0))
  expect_equal(tied$auc, 0.5)
  expect_true(tied$ties_counted_as_half)
  expect_error(Roc(c(0.1, 0.2), c(1, 1)), "both classes")
})

test_that("McNemar reproduces the book's Table 10.4", {
  r <- McNemar(TAB104)
  expect_equal(r$statistic, 12)
  expect_equal(r$df, 3L)
  # absolute, matching the Python arm; testthat tolerance is relative
  expect_close(r$p_value, 0.007383, tol = 1e-6)
  expect_true(r$is_bowker)
  expect_equal(r$n, 18)
  # and the book's stated sensitivities follow from the same table
  expect_equal(sum(TAB104[, 3]), 15)      # sonification 15/18
  expect_equal(sum(TAB104[3, ]), 4)       # direct playback 4/18
})

test_that("McNemar 2x2 uses the continuity correction", {
  t <- matrix(c(20, 5, 8, 25), 2, 2, byrow = TRUE)
  r <- McNemar(t)
  expect_equal(r$df, 1L)
  expect_false(r$is_bowker)
  expect_equal(r$statistic, 4 / 13)
  expect_equal(McNemar(t, correct = FALSE)$statistic, 9 / 13)
})

test_that("only the off-diagonal matters", {
  a <- McNemar(TAB104)
  b <- McNemar(matrix(c(99, 0, 10, 1, 99, 1, 0, 0, 99), 3, byrow = TRUE))
  expect_equal(a$statistic, b$statistic)
  expect_true(a$diagonal_contributes_nothing)
  expect_error(McNemar(matrix(c(5, 0, 0, 5), 2, 2)), "symmetric")
})

test_that("the normalized distance eq (10.112) and its blind spot", {
  r <- NormDist(0, 2, 1, 1)
  expect_equal(r$dn, 1)                   # 2 / (1 + 1), the SUM of SDs
  expect_true(r$denominator_is_the_sum_not_the_quadrature_sum)
  blind <- NormDist(1, 1, 1, 5)
  expect_equal(blind$dn, 0)
  expect_true(blind$blind_to_variance_when_means_match)
})

test_that("the divergence eq (10.117) has no such blind spot", {
  d <- Divergence(c(0, 0), c(0, 0), diag(2), diag(3, 2))
  expect_gt(d$divergence, 0)
  expect_equal(d$mean_term, 0)
  expect_true(d$separates_equal_means_via_the_covariance_term)
})

test_that("the divergence is symmetric and vanishes for identical PDFs", {
  a <- Divergence(c(0, 1), c(2, -1), CC1, CC2)$divergence
  b <- Divergence(c(2, -1), c(0, 1), CC2, CC1)$divergence
  expect_equal(a, b)
  same <- Divergence(c(1, 2), c(1, 2), CC1, CC1)
  expect_equal(same$divergence, 0, tolerance = 1e-12)
  expect_true(same$zero_for_identical_pdfs)
})

test_that("the divergence is additive over independent features", {
  d1 <- Divergence(0, 1, matrix(1), matrix(2))$divergence
  d2 <- Divergence(0, 3, matrix(1), matrix(1))$divergence
  joint <- Divergence(c(0, 0), c(1, 3), diag(2),
                      matrix(c(2, 0, 0, 1), 2, 2))$divergence
  expect_equal(joint, d1 + d2)
})

test_that("DivAv reports the worst pair, not only the average", {
  r <- DivAv(list(c(0, 0), c(0.05, 0), c(8, 8)),
             list(diag(2), diag(2), diag(2)))
  expect_lt(r$minimum, r$average)
  expect_equal(r$worst_pair, c(0, 1))
  expect_true(r$average_hides_the_worst_pair)
})

test_that("Bhattacharyya is documented as not from this book", {
  r <- BhattGauss(c(0, 1), c(2, -1), CC1, CC2)
  expect_true(r$not_from_this_book)
  expect_true(r$book_uses_divergence_eq_10_115)
  expect_gt(r$bhattacharyya, 0)
  expect_equal(BhattGauss(c(1, 2), c(1, 2), CC1, CC1)$bhattacharyya, 0,
               tolerance = 1e-12)
})

test_that("the error bound pairs with Bhattacharyya, not the divergence", {
  r <- ErrBound(0.5, 0.5, 1.4)
  expect_equal(r$bound, 0.5 * exp(-1.4))
  expect_true(r$not_from_this_book)
  expect_true(r$pairs_with_bhatt_not_with_divergence)
  expect_error(ErrBound(0.3, 0.3, 1), "sum to 1")
})

test_that("Fisher's criterion is not eq (10.112)", {
  r <- FishCrit(c(1, 2, 3, 4), c(4, 5, 6, 8))
  expect_true(r$is_not_eq_10_112)
  expect_true(FishCrit(c(1, 2, 3, 4), c(5, 6, 7, 8))$
                agrees_with_eq_10_112_ranking_only_for_equal_spread)
})

test_that("the separability index grows as the classes separate", {
  near <- SepIndex(matrix(c(0, 0, 0.1, 0, 1, 0, 1.1, 0), ncol = 2,
                          byrow = TRUE), c(0, 0, 1, 1))
  far <- SepIndex(matrix(c(0, 0, 0.1, 0, 9, 0, 9.1, 0), ncol = 2,
                         byrow = TRUE), c(0, 0, 1, 1))
  expect_gt(far$j, near$j)
  expect_true(near$ignores_off_diagonal_structure)
})

test_that("Fisher LDA separates and is two-class only", {
  r <- FishLda(XC, YC)
  expect_equal(r$classes, c(0, 1))
  expect_true(r$two_class_only)
  expect_gt(r$criterion, 0)
  expect_error(FishLda(rbind(XC, c(9, 9), c(9.1, 9)), c(YC, 2, 2)),
               "two-class")
})

test_that("LinDSep fits a threshold at least as good as the midpoint", {
  r <- LinDSep(XC, YC)
  expect_lte(r$training_errors, r$midpoint_errors)
  expect_true(r$resubstitution_error_is_optimistic)
})

test_that("LinDisc assigns to the largest discriminant", {
  r <- LinDisc(c(1, 2), matrix(c(1, 0, 0, 1), 2, 2, byrow = TRUE),
               c(0.1, -0.1))
  expect_equal(r$d, c(1.1, 1.9))
  expect_equal(r$assigned, 1L)
  expect_equal(r$margin, 0.8)
  expect_true(r$regions_are_convex)
})

test_that("Mahalanobis differs from Euclidean under scaling", {
  r <- Mahal(c(2, 0), c(0, 0), matrix(c(4, 0, 0, 1), 2, 2))
  expect_equal(r$distance, 1)
  expect_equal(r$euclidean, 2)
  expect_true(r$differs_from_euclidean)
  e <- Mahal(c(3, 4), c(0, 0), diag(2))
  expect_equal(e$distance, 5)
  expect_equal(e$euclidean, 5)
})

test_that("k-NN eq (10.29) classifies and warns about k = 1", {
  expect_equal(Knn(XC, YC, c(1.1, 1.0))$assigned, 0)
  expect_equal(Knn(XC, YC, c(3.1, 3.0))$assigned, 1)
  expect_true(Knn(XC, YC, c(1.1, 1.0))$single_neighbour_may_be_an_outlier)
  expect_equal(Knn(XC, YC, c(1.1, 1.0), k = 3)$k, 3L)
  expect_error(Knn(XC, YC, c(1, 1), metric = "mahalanobis"), "covariance")
  expect_error(Knn(XC, YC, c(1, 1), k = 99), "exceeds")
})

test_that("the Bayes prior can overturn the likelihood", {
  r <- BayesCls(c(0.6, 0.4), c(0.1, 0.9))
  expect_equal(r$maximum_likelihood_choice, 0L)
  expect_equal(r$assigned, 1L)
  expect_true(r$prior_changed_the_decision)
  expect_equal(sum(r$posterior), 1)
})

test_that("Bayes normal: the log form and the dropped constant", {
  r <- BayesNorm(c(0.5, 0.5), list(c(0, 0), c(3, 3)), list(CC1, CC2))
  const <- 0.5 * 2 * log(2 * pi)
  expect_equal(r$constant_term, const)
  expect_equal(r$d_full, r$d_dropped_constant - const)
  # dropping it cannot change the ranking
  expect_equal(which.max(r$d_full), which.max(r$d_dropped_constant))
})

test_that("equal covariances make the boundary linear", {
  same <- BayesNorm(c(0.5, 0.5), list(c(0, 0), c(3, 3)),
                    list(diag(2), diag(2)))
  expect_true(same$linear_when_covariances_are_equal)
  diffc <- BayesNorm(c(0.5, 0.5), list(c(0, 0), c(3, 3)), list(CC1, CC2))
  expect_false(diffc$linear_when_covariances_are_equal)
  expect_true(diffc$surfaces_are_hyperquadrics)
})

test_that("QDA classifies and counts its own parameters", {
  expect_equal(Qda(XC, YC, c(1.1, 1.0))$assigned, 0)
  expect_equal(Qda(XC, YC, c(3.1, 3.0))$assigned, 1)
  expect_equal(Qda(XC, YC, c(1, 1))$parameters_per_class, 3)
  expect_error(
    Qda(matrix(c(0, 0, 1, 1, 5, 5, 6, 6, 7, 8), ncol = 2, byrow = TRUE),
        c(0, 0, 1, 1, 1), c(1, 1)),
    "more samples than features")
})

test_that("logistic regression separates and reports the fit", {
  r <- LogReg(XC, YC)
  expect_equal(r$training_accuracy, 1)
  expect_lt(r$loglik, 0)
  expect_true(r$models_the_posterior_directly)
  expect_equal(length(r$coefficients), 2L)
  expect_error(LogReg(XC, c(0, 0, 0, 0, 2, 2, 2, 2)), "0/1")
})

test_that("k-means finds the two groups and is reproducible", {
  a <- KMeans(XC, 2); b <- KMeans(XC, 2)
  expect_equal(a$labels, b$labels)
  expect_equal(sort(a$sizes), c(4, 4))
  expect_true(a$local_minimum_only)
  v <- vapply(1:4, function(k) KMeans(XC, k)$wcss, numeric(1))
  expect_true(all(diff(v) <= 1e-9))
  expect_error(KMeans(XC, 99), "exceeds")
})

test_that("the elbow finds two clusters", {
  r <- Elbow(XC, kmax = 5)
  expect_equal(r$knee, 2L)
  expect_true(r$monotonic)
  expect_true(r$wcss_cannot_be_minimized)
})

test_that("hierarchical linkage changes the partition", {
  expect_false(identical(HClust(XC, "single", k = 3)$labels,
                         HClust(XC, "complete", k = 3)$labels))
  expect_true(HClust(XC, "single")$single_linkage_chains)
  for (link in c("single", "complete", "average")) {
    lab <- HClust(XC, link, k = 2)$labels
    expect_equal(length(unique(lab[1:4])), 1L)
    expect_equal(length(unique(lab[5:8])), 1L)
    expect_false(lab[1] == lab[5])
    expect_true(HClust(XC, link)$monotonic_merges)
  }
  expect_error(HClust(XC, "ward"), "linkage must be")
})

test_that("cross-validation keeps training and test separate", {
  r <- KFoldCv(XC, YC, k = 4)
  expect_equal(r$accuracy, 1)
  expect_true(r$stratified)
  expect_true(r$train_and_test_must_be_separate)
  expect_equal(sum(vapply(r$per_fold, function(f) f$n, numeric(1))), 8)
})

test_that("leave-one-out is deterministic and catches memorization", {
  a <- LooCv(XC, YC); b <- LooCv(XC, YC)
  expect_equal(a$error_rate, b$error_rate)
  expect_equal(a$n_fits, 8L)
  expect_true(a$deterministic)
  # random labels: 1-NN resubstitution is perfect, LOO is not
  X <- matrix(c(0:9, rep(0, 10)), ncol = 2)
  expect_lt(LooCv(X, c(0, 1, 0, 1, 0, 1, 0, 1, 0, 1))$accuracy, 0.6)
})

test_that("the SVM uses only its support vectors", {
  r <- Svm(matrix(c(1, 1, 2, 2, 4, 4, 5, 5), ncol = 2, byrow = TRUE),
           c(-1, -1, 1, 1))
  expect_equal(r$training_accuracy, 1)
  expect_gte(r$n_support, 2L)
  expect_gt(r$margin, 0)
  expect_true(r$boundary_set_by_the_support_vectors_only)
  expect_error(Svm(matrix(c(1, 1, 2, 2), ncol = 2, byrow = TRUE),
                   c(0, 1)), "-1 and \\+1")
})

test_that("the kernel trick solves XOR where the linear SVM cannot", {
  xor <- matrix(c(0, 0, 0, 1, 1, 0, 1, 1), ncol = 2, byrow = TRUE)
  yx <- c(-1, 1, 1, -1)
  rbf <- SvmKern(xor, yx, kernel = "rbf", gamma = 1, C = 10)
  expect_equal(rbf$training_accuracy, 1)
  expect_lt(Svm(xor, yx, C = 10)$training_accuracy, 1)
  expect_true(rbf$no_weight_vector_in_the_original_space)
  q <- SvmKern(xor, yx, query = c(0, 0), kernel = "rbf", gamma = 1, C = 10)
  expect_equal(q$assigned, -1)
  expect_true(SvmKern(xor, yx, kernel = "sigmoid", gamma = 0.5, C = 1)$
                sigmoid_kernel_is_not_always_positive_definite)
  expect_error(SvmKern(xor, yx, kernel = "cosine"), "kernel must be")
})

test_that("accuracy offers every definition at once", {
  r <- Accuracy(tp = 45, tn = 40, fp = 10, fn = 5, prevalence = 0.01)
  expect_equal(r$raw_accuracy, 0.85)
  expect_equal(r$balanced_accuracy, 0.85)
  expect_equal(r$weighted_accuracy, 0.801)
  expect_equal(r$kind, "weighted")
  expect_equal(r$accuracy, r$weighted_accuracy)
  for (k in c("raw", "balanced")) {
    a <- Accuracy(tp = 45, tn = 40, fp = 10, fn = 5, kind = k)
    expect_equal(a$kind, k)
  }
  expect_error(Accuracy(tp = 45, tn = 40, fp = 10, fn = 5,
                        kind = "weighted"), "needs a prevalence")
  expect_error(Accuracy(tp = 45, tn = 40, fp = 10, fn = 5, kind = "f1"),
               "kind must be")
  expect_error(Accuracy(tp = 45.5, tn = 40, fp = 10, fn = 5), "whole")
})

test_that("balanced accuracy is eq (10.102) at one half", {
  a <- Accuracy(tp = 45, tn = 40, fp = 10, fn = 5, exact = TRUE)
  b <- Accuracy(tp = 45, tn = 40, fp = 10, fn = 5, prevalence = "0.5",
                exact = TRUE)
  expect_true(a$balanced_accuracy == b$weighted_accuracy)
  expect_true(a$balanced_is_eq_10_102_at_one_half)
})

test_that("exact accuracy is a rational, not a rounded double", {
  r <- Accuracy(tp = 45, tn = 40, fp = 10, fn = 5, exact = TRUE)
  expect_s3_class(r$raw_accuracy, "morie_frac")
  expect_equal(format(r$raw_accuracy), "17/20")
  expect_equal(as.numeric(r$raw_accuracy), 0.85)
  w <- Accuracy(tp = 45, tn = 40, fp = 10, fn = 5, prevalence = "0.01",
                exact = TRUE)
  expect_equal(format(w$accuracy), "801/1000")
  expect_equal(as.numeric(w$accuracy), 0.801)
  # one third has no finite binary representation
  t <- Accuracy(tp = 1, tn = 1, fp = 1, fn = 2, exact = TRUE)
  expect_equal(format(t$sensitivity), "1/3")
})

test_that("the KLD eq (5.33) is weighted by the second PDF", {
  p1 <- c(0.2, 0.3, 0.5); p2 <- c(0.1, 0.4, 0.5)
  r <- Kld(p1, p2)
  expect_close(r$kld, sum(p2 * log(p2 / p1)), tol = 1e-12)
  expect_true(r$weighted_by_the_second_pdf)
  expect_true(r$asymmetric)
  expect_close(r$symmetric_sum, r$kld + r$reversed, tol = 1e-12)
  expect_true(r$symmetric_sum_is_the_divergence_of_eq_10_115)
  p <- c(0.25, 0.25, 0.5)
  expect_close(Kld(p, p)$kld, 0, tol = 1e-15)
  expect_error(Kld(c(0, 0.5, 0.5), c(0.3, 0.3, 0.4)), "unbounded")
  expect_error(Kld(c(0.5, 0.5), c(0.3, 0.3, 0.4)), "same grid")
})

test_that("the Bhattacharyya coefficient is an overlap in [0, 1]", {
  p1 <- c(0.2, 0.3, 0.5); p2 <- c(0.1, 0.4, 0.5)
  r <- BhattCoef(p1, p2)
  expect_true(r$in_unit_interval)
  expect_close(BhattCoef(p1, p1)$coefficient, 1, tol = 1e-12)
  expect_true(BhattCoef(p1, p1)$identical)
  d <- BhattCoef(c(1, 0), c(0, 1))
  expect_close(d$coefficient, 0, tol = 1e-15)
  expect_true(d$disjoint)
  expect_equal(d$distance, Inf)
  expect_close(r$distance, -log(r$coefficient), tol = 1e-12)
  expect_true(r$the_overlap_is_where_errors_must_happen)
  expect_true(r$not_from_this_book)
  # the bound tightens as the overlap falls
  close <- BhattCoef(c(0.5, 0.5), c(0.45, 0.55))$distance
  far <- BhattCoef(c(0.9, 0.1), c(0.1, 0.9))$distance
  expect_lt(ErrBound(0.5, 0.5, far)$bound, ErrBound(0.5, 0.5, close)$bound)
})

test_that("Chernoff at one half is the Bhattacharyya coefficient", {
  p1 <- c(0.2, 0.3, 0.5); p2 <- c(0.1, 0.4, 0.5)
  bc <- BhattCoef(p1, p2)$coefficient
  h <- Chernoff(p1, p2, alpha = 0.5)
  expect_close(h$coefficient, bc, tol = 1e-12)
  expect_true(h$bhattacharyya_is_alpha_one_half)
  expect_false(h$alpha_searched)
  best <- Chernoff(p1, p2)
  expect_lte(best$coefficient, bc + 1e-12)
  expect_true(best$at_least_as_tight_as_bhattacharyya)
  expect_true(best$alpha_searched)
  expect_error(Chernoff(p1, p2, alpha = 1.5), "alpha must lie")
})

test_that("Hellinger squared is one minus the coefficient", {
  p1 <- c(0.2, 0.3, 0.5); p2 <- c(0.1, 0.4, 0.5)
  h <- Hellinger(p1, p2)
  expect_close(h$squared, 1 - BhattCoef(p1, p2)$coefficient, tol = 1e-12)
  expect_true(h$identity_h2_equals_one_minus_bc)
  expect_true(h$in_unit_interval)
  expect_close(Hellinger(p1, p1)$hellinger, 0, tol = 1e-12)
  expect_close(Hellinger(c(1, 0), c(0, 1))$hellinger, 1, tol = 1e-12)
})

test_that("Hellinger is a metric where the Bhattacharyya distance is not", {
  a <- c(0.2, 0.3, 0.5); b <- c(0.1, 0.4, 0.5); cc <- c(0.5, 0.25, 0.25)
  hab <- Hellinger(a, b)$hellinger
  hbc <- Hellinger(b, cc)$hellinger
  hac <- Hellinger(a, cc)$hellinger
  expect_lte(hac, hab + hbc + 1e-12)          # the triangle inequality
  expect_close(hab, Hellinger(b, a)$hellinger, tol = 1e-12)
  expect_true(Hellinger(a, b)$is_a_true_metric)
  expect_true(Hellinger(a, b)$bhattacharyya_distance_does_not)
})

test_that("every borrowed measure carries its primary citation", {
  p1 <- c(0.2, 0.3, 0.5); p2 <- c(0.1, 0.4, 0.5)
  rs <- list(BhattCoef(p1, p2), Chernoff(p1, p2), Hellinger(p1, p2),
             ErrBound(0.5, 0.5, 1), BhattGauss(c(0, 1), c(2, -1), CC1, CC2))
  for (r in rs) {
    expect_true(r$not_from_this_book)
    expect_gt(nchar(r$reference), 40)
  }
  expect_true(grepl("1943", BhattCoef(p1, p2)$reference))
  expect_true(grepl("493-507", Chernoff(p1, p2)$reference))
  expect_true(grepl("1909", Hellinger(p1, p2)$reference))
  expect_true(grepl("1967", ErrBound(0.5, 0.5, 1)$reference))
})
