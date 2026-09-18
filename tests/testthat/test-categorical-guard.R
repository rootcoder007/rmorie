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

test_that("marginals_verify accepts matching counts and names a label permutation", {
  x <- c("White", "White", "Black", "Indigenous", "White", "Black")
  r <- morie_marginals_verify(x, c(White = 3, Black = 2, Indigenous = 1))
  expect_true(r$ok)
  expect_null(r$permutation)
  err <- tryCatch(morie_marginals_verify(x, c(White = 2, Black = 3, Indigenous = 1)),
                  error = function(e) conditionMessage(e))
  expect_match(err, "permuted")
  expect_match(err, "White -> Black")
  expect_error(morie_marginals_verify(c(x, "Other"), c(White = 3, Black = 2, Indigenous = 1)),
               "not in the published counts")
})

test_that("odds_ratio_check recovers a label swap behind an inflated odds ratio", {
  tab <- matrix(c(900, 100, 700, 300, 400, 600), ncol = 2, byrow = TRUE,
                dimnames = list(c("A", "B", "C"), c("no", "yes")))
  r <- morie_odds_ratio_check(tab, "A", c(B = 300 / 700 / (100 / 900), C = 600 / 400 / (100 / 900)))
  expect_true(r$consistent)
  expect_equal(unname(r$computed[["B"]]), 3.857142857, tolerance = 1e-6)
  # the report says B has the odds ratio that belongs to C: B and C swapped
  r2 <- morie_odds_ratio_check(tab, "A", c(B = 13.5, C = 3.857))
  expect_false(r2$consistent)
  expect_true(nrow(r2$matches) >= 1)
  expect_match(r2$matches$relabelling[1], "B -> C")
  expect_match(r2$verdict, "mislabelled")
  # numbers that follow from no relabelling
  r3 <- morie_odds_ratio_check(tab, "A", c(B = 36, C = 4))
  expect_false(r3$consistent)
  expect_equal(nrow(r3$matches), 0)
  expect_error(morie_odds_ratio_check(tab, "Z", c(B = 1, C = 1)), "not a row")
})

test_that("morie_odds_ratio_check names the four-way rotation documented in the OHRC 2023 correction", {
  # White coded as Black, Black as Other, Other as Unknown, Unknown as White
  # (OHRC, Correction to "A Disparate Impact", 26 January 2023; Jung 2022).
  tab <- matrix(c(9000, 120, 2000, 220, 1500, 60, 4000, 1), ncol = 2, byrow = TRUE,
                dimnames = list(c("White", "Black", "Other", "Unknown"), c("no", "yes")))
  correct <- morie_odds_ratio_check(tab, "White", c(Black = 220 / 2000 / (120 / 9000), Other = 60 / 1500 / (120 / 9000), Unknown = 1 / 4000 / (120 / 9000)))
  expect_true(correct$consistent)
  rotated <- tab[c("Unknown", "White", "Black", "Other"), ]
  rownames(rotated) <- c("White", "Black", "Other", "Unknown")
  reported <- c(Black = rotated["Black", 2] / rotated["Black", 1] / (rotated["White", 2] / rotated["White", 1]),
                Other = rotated["Other", 2] / rotated["Other", 1] / (rotated["White", 2] / rotated["White", 1]),
                Unknown = rotated["Unknown", 2] / rotated["Unknown", 1] / (rotated["White", 2] / rotated["White", 1]))
  r <- morie_odds_ratio_check(tab, "White", reported)
  expect_false(r$consistent)
  expect_true(any(grepl("Unknown -> White", r$matches$relabelling)))
  expect_gt(unname(reported["Black"]) / unname(correct$computed["Black"]), 5)
  expect_match(r$verdict, "mislabelled")
})

test_that("morie_safe_relabel refuses positional labels and maps by name", {
  f <- factor(c("W", "B", "O", "W"))
  expect_error(morie_safe_relabel(f, c("Black", "Other", "White")), "POSITION")
  g <- morie_safe_relabel(f, c(W = "White", B = "Black", O = "Other"))
  expect_equal(as.character(g), c("White", "Black", "Other", "White"))
  expect_equal(levels(g), c("White", "Black", "Other"))
  expect_error(morie_safe_relabel(f, c(W = "White")), "NO mapping")
})

test_that("morie_decode_labelled decodes by code and keeps code order for the levels", {
  x <- structure(c(1, 2, 2, 4), labels = c(White = 1, Black = 2, Other = 3, Unknown = 4))
  f <- morie_decode_labelled(x)
  expect_equal(as.character(f), c("White", "Black", "Black", "Unknown"))
  expect_equal(levels(f), c("White", "Black", "Other", "Unknown"))
  expect_error(morie_decode_labelled(c(1, 5), c("1" = "White")), "NO mapping")
  expect_error(morie_decode_labelled(c(1, 2)), "NOT the categories")
})

test_that("morie_relabel_forensics reproduces the OHRC rotation from an alphabetical positional relabel", {
  vl <- c("1" = "White", "2" = "Black", "3" = "Other", "4" = "Unknown")
  obs <- c(White = "Black", Black = "Other", Other = "Unknown", Unknown = "White")
  r <- morie_relabel_forensics(vl, obs)
  expect_true(r$matches[r$mechanism == "labels sorted alphabetically, assigned by code position"])
  expect_match(attr(r, "verdict"), "reproduced EXACTLY")
  expect_output(print(r), "alphabetically")
  # what the relabel idiom actually does to those codes
  f <- factor(c(1, 2, 3, 4, 1))
  levels(f) <- sort(unname(vl))
  expect_equal(as.character(f), c("Black", "Other", "Unknown", "White", "Black"))
  none <- morie_relabel_forensics(vl, c(White = "Other", Black = "White", Other = "Black", Unknown = "Unknown"))
  expect_false(any(none$matches))
  expect_match(attr(none, "verdict"), "No positional")
  expect_error(morie_relabel_forensics(vl, c(White = "Black")), "permutation")
  fr <- morie_relabel_forensics(vl, c(White = "White", Black = "Black", Other = "Other", Unknown = "Unknown"),
             counts = c(White = 9000, Black = 2000, Other = 1500, Unknown = 60))
  expect_true(fr$matches[fr$mechanism == "labels ordered by decreasing frequency, assigned by code position"])
})

test_that("audit flags code-prefixed labels", {
  a <- morie_audit_categories(data.frame(race = c("1. White", "2. Black", "1. White"), stringsAsFactors = FALSE))
  expect_match(a$hazards[1], "code prefixes")
})

test_that("morie_transfer_verify accepts a faithful SPSS-style import and names a rotated one", {
  x <- structure(c(1, 1, 2, 4, 1), labels = c(White = 1, Black = 2, Other = 3, Unknown = 4))
  ok <- morie_transfer_verify(x, c(White = 3, Black = 1, Unknown = 1),
             code_book = c("1" = "White", "2" = "Black", "3" = "Other", "4" = "Unknown"))
  expect_true(ok$ok)
  expect_true(ok$code_book_ok)
  expect_equal(levels(ok$decoded), c("White", "Black", "Other", "Unknown"))
  rotated <- structure(c(1, 1, 2, 4, 1), labels = c(Black = 1, Other = 2, Unknown = 3, White = 4))
  expect_error(morie_transfer_verify(rotated, c(White = 3, Black = 1, Unknown = 1),
                    code_book = c("1" = "White", "2" = "Black", "3" = "Other", "4" = "Unknown")),
               "disagree with the source code book")
  expect_error(morie_transfer_verify(c("Black", "Black", "Other", "White", "Black"), c(White = 3, Black = 1, Unknown = 1)),
               "do not match|permuted|not in the published")
})
