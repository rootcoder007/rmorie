# SPDX-License-Identifier: AGPL-3.0-or-later
# Cross-validation: native backdoor identification vs dagitty
# (module 13). Suggests allowed here ONLY.

test_that("cross: adjustment sets are valid per dagitty", {
  skip_if_not_installed("dagitty")
  specs <- list(
    list(e = c("z -> x", "z -> y", "x -> y"), x = "x", y = "y"),
    list(e = c("x -> m", "m -> y", "z -> x", "z -> y"), x = "x", y = "y"),
    list(e = c("a -> x", "b -> a", "b -> y", "x -> y"), x = "x", y = "y")
  )
  for (sp in specs) {
    g <- morie_dag(sp$e, sp$x, sp$y)
    id <- morie_dag_identify(g)
    dg <- dagitty::dagitty(paste0("dag { ",
                                  paste(gsub("->", "->", sp$e),
                                        collapse = " ; "), " }"))
    dagitty::exposures(dg) <- sp$x
    dagitty::outcomes(dg) <- sp$y
    ok <- dagitty::isAdjustmentSet(dg, id$adjustment_set)
    expect_true(id$identified)
    expect_true(ok)
  }
})
