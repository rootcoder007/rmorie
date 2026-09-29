# Coverage for model inversion (Fredrikson, Jha & Ristenpart 2015): tree
# path enumeration, the row-normalised confusion error err(y, y'), the
# black-box MAP estimate argmax_v err(y, f(x_K, v)) prod_j p_j(x_j), the
# white-box-with-counts score (mass of consistent paths times the prior)
# and the accuracy / precision / recall bookkeeping.

.tree <- list(feature = 0, branches = list(
  a = list(feature = 1, branches = list(x = list(label = 1, count = 30), y = list(label = 0, count = 10))),
  b = list(feature = 1, branches = list(x = list(label = 0, count = 15), y = list(label = 1, count = 45)))))
.pri <- list("0" = list(a = 0.4, b = 0.6), "1" = list(x = 0.5, y = 0.5))

test_that("tree paths list constraints, labels and counts", {
  p <- tree_paths(.tree)
  expect_length(p, 4L)
  expect_identical(p[[3]]$constraints, list("0" = "b", "1" = "x"))
  expect_identical(vapply(p, function(z) z$count, 1), c(30, 10, 15, 45))
  expect_identical(tree_paths(list(label = 2))[[1]]$count, 0)
  expect_error(tree_paths(list(feature = 1)), "needs 'feature' and 'branches'")
})

test_that("confusion error rows are normalised", {
  C <- rbind(c(8, 2), c(3, 7))
  e <- confusion_error(C)
  expect_equal(e(0, 1), 0.2)
  expect_equal(e(1, 1), 0.7)
  el <- confusion_error(list(c(1, 3), c(0, 0)), labels = c("n", "p"))
  expect_equal(el("n", "p"), 0.75)
  expect_identical(el("p", "p"), 0)
  expect_error(e(2, 0), "unknown label")
  expect_error(confusion_error(matrix(1, 2, 3)), "square")
  expect_error(confusion_error(-C), "cannot be negative")
  expect_error(confusion_error(C, labels = 1:3), "one label per")
})

test_that("black-box MAP inversion maximises err * prior product", {
  C <- rbind(c(9, 1), c(2, 8))
  err <- confusion_error(C)
  f <- function(x) if (x[["0"]] == "a") (if (x[["1"]] == "x") 1 else 0) else (if (x[["1"]] == "x") 0 else 1)
  m <- map_invert(f, 1, list("1" = "x"), list("a", "b"), err, .pri)
  sc <- c(a = 0.8 * 0.4 * 0.5, b = 0.2 * 0.6 * 0.5)
  expect_equal(vapply(m$scores, `[[`, 1, "score"), unname(sc), tolerance = 1e-12)
  expect_identical(m$estimate, "a")
  expect_error(map_invert(f, 1, list(), list(), err, .pri), "no candidate")
})

test_that("white-box inversion weights consistent path mass by the prior", {
  w <- wbwc_invert(.tree, list("1" = "y"), list("a", "b"), .pri)
  expect_equal(w$N, 100)
  expect_equal(vapply(w$scores, `[[`, 1, "score"), c(0.10 * 0.4, 0.45 * 0.6), tolerance = 1e-12)
  expect_identical(w$estimate, "b")
  u <- wbwc_invert(.tree, list("1" = "y"), list("a", "b"), .pri, unknown = 1)
  expect_equal(vapply(u$scores, `[[`, 1, "score"), c(0.4 * 0.4, 0.6 * 0.6), tolerance = 1e-12)
  expect_error(wbwc_invert(list(label = 1), list(), list("a"), .pri), "needs path counts")
})

test_that("the attack scores targets and reports accuracy, precision and recall", {
  targets <- list(list(known = list("1" = "y"), label = 1, truth = "b"),
                  list(known = list("1" = "x"), label = 1, truth = "a"),
                  list(known = list("1" = "x"), label = 0, truth = "a"))
  C <- rbind(c(9, 1), c(2, 8))
  r <- morie_attrInf(.tree, targets, .pri, confusion = C)
  f <- function(x) .attrInf_tree_predict(.tree, x)
  g <- vapply(targets, function(t) map_invert(f, t$label, t$known, list("a", "b"), confusion_error(C), .pri)$estimate, "")
  expect_identical(unlist(r$guesses), g)
  tr <- c("b", "a", "a")
  expect_equal(r$accuracy, mean(g == tr))
  tp <- sum(g == "b" & tr == "b")
  expect_identical(r$true_positives, as.integer(tp))
  expect_equal(r$precision, tp / sum(g == "b"))
  wb <- morie_attrInf(.tree, targets, .pri, mode = "whitebox")
  gw <- vapply(targets, function(t) wbwc_invert(.tree, t$known, list("a", "b"), .pri)$estimate, "")
  expect_identical(unlist(wb$guesses), gw)
  expect_identical(wb$n_paths, 4L)
  expect_identical(attribute_inference, morie_attrInf)
  expect_error(morie_attrInf(.tree, targets, .pri), "needs a confusion matrix")
  expect_error(morie_attrInf(.tree, targets, .pri, mode = "greybox"), "mode must be one of")
  expect_error(morie_attrInf(.tree, targets, list(), sensitive = 5, mode = "whitebox"), "no candidate values")
})
