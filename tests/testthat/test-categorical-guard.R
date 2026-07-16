# SPDX-License-Identifier: AGPL-3.0-or-later
# Module 25 — categorical-integrity guards. The final test reproduces
# the real-world failure mode these guards exist for: numeric-coded
# categories imported without labels, silently coerced, multiplying an
# odds ratio severalfold.

test_that("morie_safe_recode: name-only mapping, errors on unmapped", {
  x <- c("W", "B", "O", "W", NA)
  y <- morie_safe_recode(x, c(W = "White", B = "Black", O = "Other"))
  expect_equal(y[1:4], c("White", "Black", "Other", "White"),
               ignore_attr = TRUE)
  expect_true(is.na(y[5]))
  expect_error(morie_safe_recode(x, c(W = "White", B = "Black")),
               "NO mapping")
  z <- morie_safe_recode(x, c(W = "White", B = "Black"), keep = "O")
  expect_equal(z[3], "O", ignore_attr = TRUE)
  expect_match(attr(y, "morie_recode_audit")$checksum, "^[0-9a-f]{64}$")
})

test_that("morie_safe_factor: explicit levels + reference assertion", {
  x <- c("White", "Black", "White")
  f <- morie_safe_factor(x, levels = c("White", "Black"))
  expect_equal(levels(f), c("White", "Black"))
  expect_error(morie_safe_factor(c(x, "Zeta"),
                                 levels = c("White", "Black")),
               "outside the declared levels")
  expect_error(morie_safe_factor(x, levels = c("White", "Black"),
                                 reference = "Black"),
               "not levels\\[1\\]")
})

test_that("audit flags numeric-looking codes, labels, case dups", {
  df <- data.frame(race = factor(c("1", "2", "2", "3")),
                   city = c("Toronto", "toronto", "Ottawa", "Ottawa"),
                   stringsAsFactors = FALSE)
  a <- morie_audit_categories(df)
  expect_s3_class(a, "morie_category_audit")
  expect_false(isTRUE(attr(a, "clean")))
  expect_match(a$hazards[a$column == "race"], "level INDICES")
  expect_match(a$hazards[a$column == "city"], "case-variant")
  expect_output(print(a), "HAZARD")
  clean <- morie_audit_categories(
    data.frame(g = c("a", "b", "a"), stringsAsFactors = FALSE))
  expect_true(isTRUE(attr(clean, "clean")))
})

test_that("crosstab verifier catches swaps, fan-out, lost rows, NAs", {
  x <- c("W", "B", "W", "O")
  good <- c("White", "Black", "White", "Other")
  map <- c(W = "White", B = "Black", O = "Other")
  expect_silent(morie_crosstab_verify(x, good, map))
  # the catastrophic case: groups SWAPPED relative to the declaration
  swapped <- c("Black", "White", "Black", "Other")
  expect_error(morie_crosstab_verify(x, swapped, map),
               "declared mapping")
  # one category split into two
  fan <- c("White", "Black", "Other", "Other")
  expect_error(morie_crosstab_verify(x, fan, map), "MULTIPLE")
  expect_error(morie_crosstab_verify(x, good[1:3], map),
               "length mismatch")
  na_leak <- c("White", NA, "White", "Other")
  expect_error(morie_crosstab_verify(x, na_leak, map), "missingness")
})

test_that("binary-treatment guard refuses factor coercion", {
  expect_error(
    rmorie:::.morie_guard_binary_treatment(factor(c("no", "yes")),
                                           "treated"),
    "level INDICES")
  expect_error(
    rmorie:::.morie_guard_binary_treatment(c(1, 2, 1), "treated"),
    "binary 0/1")
  expect_true(rmorie:::.morie_guard_binary_treatment(c(0, 1, NA),
                                                     "treated"))
})

test_that("REPRODUCTION: code-vs-label confusion inflates an odds
           ratio; the guarded path refuses it", {
  # DGP: true odds ratio for group B vs reference A is ~4.
  set.seed(250)
  n <- 6000
  race_label <- sample(c("A", "B", "C"), n, replace = TRUE,
                       prob = c(0.5, 0.25, 0.25))
  p <- ifelse(race_label == "B", plogis(qlogis(0.05) + log(4)),
              0.05)
  y <- rbinom(n, 1, p)
  true_fit <- glm(y ~ relevel(factor(race_label), "A"),
                  family = binomial)
  or_true <- exp(coef(true_fit)[2])
  expect_equal(unname(or_true), 4, tolerance = 0.6)
  # The import hazard: the column arrives as numeric CODES with the
  # label mapping lost, and a positional recode assumes the wrong
  # code order — relabeling the groups wholesale.
  codes <- match(race_label, c("A", "B", "C"))      # truth: 1,2,3
  wrong_labels <- c("B", "C", "A")[codes]           # positional slip
  # Where this detonates hardest: benchmark rate ratios. Group B is
  # 25 percent of the population with ~4x the event rate. Swapped
  # labels attach B's event count to C's population share computed
  # from an EXTERNAL benchmark (label-keyed), so the "rate ratio"
  # multiplies severalfold — the published-then-corrected pattern.
  bench <- c(A = 0.50, B = 0.25, C = 0.05)   # label-keyed pop shares
  events_by_label <- tapply(y, wrong_labels, sum)
  rate_wrong <- events_by_label / (bench[names(events_by_label)] * n)
  rr_wrong <- max(rate_wrong) / rate_wrong[["A"]]
  events_true <- tapply(y, race_label, sum)
  pop_true <- table(race_label)[names(events_true)]
  rate_true <- events_true / as.numeric(pop_true)
  rr_true <- rate_true[["B"]] / rate_true[["A"]]
  expect_equal(unname(rr_true), 4, tolerance = 1)
  # the miscode manufactures a wildly inflated "disparity"
  expect_gt(unname(rr_wrong) / unname(rr_true), 3)
  # Guard 1: the audit flags the code-looking column on sight
  a <- morie_audit_categories(
    data.frame(race = factor(as.character(codes))))
  expect_match(a$hazards[1], "level INDICES")
  # Guard 2: the crosstab verifier catches the swap immediately
  expect_error(
    morie_crosstab_verify(race_label, wrong_labels,
                          c(A = "A", B = "B", C = "C")),
    "declared mapping")
  # Guard 3: the safe path reproduces the truth
  safe <- morie_safe_recode(as.character(codes),
                            c("1" = "A", "2" = "B", "3" = "C"))
  expect_silent(morie_crosstab_verify(as.character(codes), safe,
                                      c("1" = "A", "2" = "B",
                                        "3" = "C")))
  safe_fit <- glm(y ~ morie_safe_factor(safe, c("A", "B", "C")),
                  family = binomial)
  expect_equal(unname(exp(coef(safe_fit)[2])), unname(or_true),
               tolerance = 1e-8)
})

test_that("MRM pipeline enforces the guards end to end", {
  set.seed(251)
  n <- 400
  x <- rnorm(n)
  t01 <- rbinom(n, 1, plogis(0.5 * x))
  y <- 1 + 0.8 * t01 + 0.5 * x + rnorm(n)
  df <- data.frame(y = y, t = factor(ifelse(t01 == 1, "yes", "no")),
                   x = x)
  expect_error(
    morie_mrm_estimate_causal_effect(df, "t", "y", "x",
                                     methods = "ate"),
    "level INDICES")
  df$t <- t01
  df$grp <- factor(c("1", "2")[1 + (x > 0)])
  expect_warning(
    morie_mrm_estimate_causal_effect(df, "t", "y", c("x", "grp"),
                                     methods = "ate"),
    "coding hazards")
})
