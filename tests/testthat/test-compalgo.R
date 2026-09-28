test_that("SmtSolver models satisfy the asserted constraints", {
  atoms <- list(list("s", "a", 0), list("s", "b", 0), list("a", "b", -3), list("b", "a", -4), list("a", "s", 4), list("b", "s", 3))
  r <- SmtSolver(list(atoms = atoms, clauses = list(1, 2, c(3, 4), 5, 6)))
  expect_true(r$satisfiable)
  for (k in seq_along(atoms)) {
    a <- atoms[[k]]
    holds <- r$solution[[a[[1]]]] - r$solution[[a[[2]]]] <= a[[3]]
    expect_identical(holds, r$model[[as.character(k)]])
  }
  u <- SmtSolver(list(atoms = list(list("x", "y", 0), list("y", "x", -1)), clauses = list(1, 2)))
  expect_false(u$satisfiable)
})

test_that("ShorFactoring returns nontrivial factors", {
  for (N in c(15, 21, 35, 91, 143, 27)) {
    r <- ShorFactoring(N, seed = 2)
    expect_equal(prod(r$factors), N)
    expect_true(all(r$factors > 1 & r$factors < N))
  }
  p <- .ca_orderdist(4, 256)
  expect_equal(sum(p), 1, tolerance = 1e-12)
  expect_equal(p[c(1, 65, 129, 193)], rep(0.25, 4), tolerance = 1e-12)
  expect_error(ShorFactoring(13))
})
