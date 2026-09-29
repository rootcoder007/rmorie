# Coverage for the AlphaDev AssemblyGame (Mankowitz et al. 2023): the
# mov / cmp / cmovl / cmovg semantics, execution and correctness count,
# the action space size 4 L (L - 1), the text rendering, exhaustive BFS
# (checked against brute force over the same programs) and PUCT search.

.I <- function(op, a, b) list(op, list(a[1], as.integer(a[2])), list(b[1], as.integer(b[2])))
.sort2 <- list(.I("mov", c("M", 0), c("R", 0)), .I("cmp", c("M", 0), c("M", 1)),
               .I("cmovg", c("M", 1), c("M", 0)), .I("cmovg", c("R", 0), c("M", 1)))

test_that("instructions move, compare and conditionally move", {
  st <- list(mem = c(5, 2), reg = 0, flag = 0L)
  s1 <- morie_alfqud_step(st, .I("mov", c("M", 0), c("R", 0)))
  expect_identical(s1$reg, 5)
  s2 <- morie_alfqud_step(s1, .I("cmp", c("M", 0), c("M", 1)))
  expect_identical(s2$flag, 1L)
  expect_identical(morie_alfqud_step(s2, .I("cmovl", c("M", 1), c("M", 0)))$mem, c(5, 2))
  expect_identical(morie_alfqud_step(s2, .I("cmovg", c("M", 1), c("M", 0)))$mem, c(2, 2))
  expect_identical(morie_alfqud_step(st, .I("cmp", c("M", 1), c("M", 0)))$flag, -1L)
  expect_identical(morie_alfqud_step(list(mem = c(1, 1), reg = 0, flag = 0L), .I("cmp", c("M", 1), c("M", 0)))$flag, 0L)
  expect_error(morie_alfqud_step(st, .I("add", c("M", 0), c("M", 1))), "unknown instruction")
  expect_error(morie_alfqud_step(st, .I("mov", c("M", 2), c("M", 1))), "reads outside memory")
  expect_error(morie_alfqud_step(st, .I("mov", c("M", 0), c("R", 3))), "writes a register")
  expect_error(morie_alfqud_step(st, .I("mov", c("X", 0), c("M", 1))), "memory or in a register")
})

test_that("the four-instruction sort-2 program sorts every input", {
  ins <- list(c(3, 1), c(1, 3), c(2, 2), c(-1, -4))
  for (x in ins) expect_identical(morie_alfqud_run(.sort2, x, 1), sort(x))
  tg <- lapply(ins, sort)
  expect_identical(morie_alfqud_correctness(.sort2, ins, tg, 1), 8L)
  expect_identical(morie_alfqud_correctness(list(), ins, tg, 1), 4L)
  expect_identical(morie_alfqud_text(.sort2),
                   "mov M0 R0\ncmp M0 M1\ncmovg M1 M0\ncmovg R0 M1")
  expect_identical(morie_alfqud_text(list()), "")
})

test_that("the action space has 4 L (L - 1) moves over distinct locations", {
  a <- morie_alfqud_actions(3, 1)
  expect_length(a, 4 * 4 * 3)
  expect_false(any(vapply(a, function(i) identical(i[[2]], i[[3]]), TRUE)))
  expect_identical(unique(vapply(a, `[[`, "", 1)), c("mov", "cmp", "cmovl", "cmovg"))
  expect_length(morie_alfqud_actions(2, 0), 8L)
})

test_that("BFS returns the best program by brute force; MCTS never beats it", {
  ins <- list(c(3, 1), c(1, 3), c(5, 2))
  tg <- lapply(ins, sort)
  acts <- c(.sort2, list(.I("mov", c("M", 1), c("M", 0)), .I("cmovl", c("R", 0), c("M", 0))))
  b <- morie_alfqud(ins, action_space = acts, n_reg = 1, max_len = 4, search = "bfs", latency_weight = 0.01)
  # enumerate all programs of length 0..4 in BFS order, keep the first strict maximum
  best <- -Inf
  arg <- list()
  for (len in 0:4) {
    idx <- if (len == 0) list(integer(0)) else asplit(as.matrix(rev(expand.grid(rep(list(seq_along(acts)), len)))), 1)
    for (ix in idx) {
      p <- lapply(unname(ix), function(k) acts[[k]])
      s <- morie_alfqud_correctness(p, ins, tg, 1) - 0.01 * len
      if (s > best) {
        best <- s
        arg <- p
      }
    }
  }
  expect_equal(b$score, best, tolerance = 1e-12)
  expect_identical(b$program, arg)
  expect_true(b$solved)
  expect_identical(b$nodes, as.integer(1 + sum(6^(1:4))))
  expect_identical(b$outputs, tg)
  m <- morie_alfqud(ins, action_space = acts, n_reg = 1, max_len = 4, n_sim = 300, latency_weight = 0.01)
  expect_lte(m$score, b$score + 1e-12)
  expect_equal(m$score, morie_alfqud_correctness(m$program, ins, tg, 1) - 0.01 * m$length, tolerance = 1e-12)
  rf <- function(p, i, t, r) length(p)
  g <- morie_alfqud(ins, action_space = acts, n_reg = 1, max_len = 2, search = "bfs", reward_fn = rf)
  expect_identical(g$length, 2L)
  expect_error(morie_alfqud(list()), "no test input")
  expect_error(morie_alfqud(list(1:2, 1:3)), "same length")
  expect_error(morie_alfqud(ins, action_space = list()), "no legal move")
  expect_error(morie_alfqud(ins, search = "dfs"), "mcts or bfs")
  expect_match(morie_alfqud_cheatsheet(), "AssemblyGame")
})
