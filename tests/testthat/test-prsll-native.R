# Anchors for the LL(1) module.
#
# The grammar below is the expression grammar of Aho, Lam, Sethi &
# Ullman, "Compilers: Principles, Techniques, and Tools", 2nd ed.,
# section 4.4.2. Its FIRST sets, FOLLOW sets and predictive parsing
# table are worked out in the book, so every expectation here is a
# published value rather than whatever this implementation happens to
# return. Getting one of them wrong is a failure, not a tolerance.
#
#   E  -> T E'
#   E' -> + T E' | eps
#   T  -> F T'
#   T' -> * F T' | eps
#   F  -> ( E ) | id

# leaves of a tree, independent of .prsLL_linearise, so the yield check
# below is not comparing the implementation against itself
.prsLll_leaves <- function(node) {
  if (is.null(node$children)) return(node$symbol)
  unlist(lapply(node$children, .prsLll_leaves), use.names = FALSE)
}

dragon_rules <- function() {
  list(
    list("E",  c("T", "Ep")),          # 1
    list("Ep", c("+", "T", "Ep")),     # 2
    list("Ep", character(0)),          # 3
    list("T",  c("F", "Tp")),          # 4
    list("Tp", c("*", "F", "Tp")),     # 5
    list("Tp", character(0)),          # 6
    list("F",  c("(", "E", ")")),      # 7
    list("F",  "id")                   # 8
  )
}

dragon <- function() .prsLL_grammar(dragon_rules(), start = "E")

test_that("grammar construction records the productions and the start symbol", {
  g <- dragon()
  expect_length(g$rules, 8L)
  expect_identical(g$start, "E")
  expect_identical(.prsLL_nonterminals(g), c("E", "Ep", "T", "Tp", "F"))
  expect_setequal(.prsLL_terminals(g), c("+", "*", "(", ")", "id"))
  # the start symbol defaults to the first left-hand side
  expect_identical(.prsLL_grammar(dragon_rules())$start, "E")
})

test_that("FIRST sets match the published values", {
  first <- .prsLL_first_sets(dragon())
  expect_setequal(first$E,  c("(", "id"))
  expect_setequal(first$T,  c("(", "id"))
  expect_setequal(first$F,  c("(", "id"))
  expect_setequal(first$Ep, c("+", ""))
  expect_setequal(first$Tp, c("*", ""))
  # epsilon belongs to exactly the two sets above, not to E, T or F
  expect_false("" %in% first$E)
  expect_false("" %in% first$T)
  expect_false("" %in% first$F)
})

test_that("FOLLOW sets match the published values", {
  follow <- .prsLL_follow_sets(dragon())
  expect_setequal(follow$E,  c(")", "$"))
  expect_setequal(follow$Ep, c(")", "$"))
  expect_setequal(follow$T,  c("+", ")", "$"))
  expect_setequal(follow$Tp, c("+", ")", "$"))
  expect_setequal(follow$F,  c("+", "*", ")", "$"))
})

test_that("FIRST of a symbol sequence propagates epsilon left to right", {
  g <- dragon()
  first <- .prsLL_first_sets(g)
  nts <- .prsLL_nonterminals(g)
  # a leading terminal ends the walk immediately
  expect_identical(.prsLL_first_seq(c("+", "T"), first, nts), "+")
  # Tp can vanish, so the walk continues into Ep, which can also vanish,
  # so epsilon survives to the end
  expect_setequal(.prsLL_first_of(c("Tp", "Ep"), g), c("*", "+", ""))
  # F cannot vanish, so nothing past it contributes
  expect_setequal(.prsLL_first_of(c("F", "Tp"), g), c("(", "id"))
  # the empty sequence derives only epsilon
  expect_identical(.prsLL_first_seq(character(0), first, nts), "")
})

test_that("the LL(1) table holds the published production in every cell", {
  t <- .prsLL_ll1_table(dragon())
  expect_length(t$conflicts, 0L)
  cell <- function(A, a) t$table[[paste(A, a, sep = "\r")]]
  # Aho et al., 2nd ed., figure 4.17
  expect_identical(cell("E",  "id"), 1L); expect_identical(cell("E",  "("), 1L)
  expect_identical(cell("Ep", "+"),  2L)
  expect_identical(cell("Ep", ")"),  3L); expect_identical(cell("Ep", "$"), 3L)
  expect_identical(cell("T",  "id"), 4L); expect_identical(cell("T",  "("), 4L)
  expect_identical(cell("Tp", "*"),  5L)
  expect_identical(cell("Tp", "+"),  6L); expect_identical(cell("Tp", ")"), 6L)
  expect_identical(cell("Tp", "$"),  6L)
  expect_identical(cell("F",  "("),  7L)
  expect_identical(cell("F",  "id"), 8L)
  # the table has exactly these thirteen entries and no others
  expect_length(t$table, 13L)
  # cells the book leaves blank really are absent
  expect_null(cell("E", "+"))
  expect_null(cell("F", "*"))
})

test_that("the expression grammar is recognised as LL(1) and not left recursive", {
  r <- .prsLL_is_ll1(dragon())
  expect_true(r$ll1)
  expect_length(r$conflicts, 0L)
  expect_length(r$left_recursive, 0L)
})

test_that("both parse routes return the same tree, and the yield is the input", {
  g <- dragon()
  for (toks in list(
    "id",
    c("id", "+", "id"),
    c("id", "+", "id", "*", "id"),
    c("(", "id", "+", "id", ")", "*", "id"),
    c("(", "(", "id", ")", ")")
  )) {
    a <- morie_prsLL(g, toks, route = "table")
    b <- morie_prsLL(g, toks, route = "recursive_descent")
    expect_identical(a$tree, b$tree)
    # a parse tree whose leaves are not the input is not a parse of the input
    expect_identical(a$yield, toks)
    expect_identical(b$yield, toks)
  }
})

test_that("the table route builds a populated tree, not an empty root", {
  # The two routes once disagreed here: the table route assigned children
  # onto a node it had pulled off its stack, which in R updates a copy, so
  # the root came back childless and the yield was empty. Anything that
  # only checked "the parse did not error" passed throughout.
  tree <- morie_prsLL(dragon(), c("id", "+", "id"), route = "table")$tree
  expect_identical(tree$symbol, "E")
  expect_length(tree$children, 2L)                       # E -> T E'
  expect_identical(vapply(tree$children, function(k) k$symbol, character(1)),
                   c("T", "Ep"))
  expect_gt(length(.prsLll_leaves(tree)), 0L)
})

test_that("the parse tree has the shape the grammar dictates", {
  # id * id: E -> T E', T -> F T', T' -> * F T', E' -> eps
  tree <- morie_prsLL(dragon(), c("id", "*", "id"))$tree
  E <- tree
  expect_identical(E$symbol, "E")
  Tn <- E$children[[1]]; Ep <- E$children[[2]]
  expect_identical(Tn$symbol, "T")
  expect_identical(Ep$symbol, "Ep")
  expect_length(Ep$children, 0L)                         # E' -> eps
  F1 <- Tn$children[[1]]; Tp <- Tn$children[[2]]
  expect_identical(F1$children[[1]]$symbol, "id")
  expect_identical(vapply(Tp$children, function(k) k$symbol, character(1)),
                   c("*", "F", "Tp"))
  expect_length(Tp$children[[3]]$children, 0L)           # the inner T' -> eps
  expect_identical(.prsLll_leaves(tree), c("id", "*", "id"))
})

test_that("linearise returns the frontier in left-to-right order", {
  tree <- .prsLL_node("A", list(
    .prsLL_leaf("x"),
    .prsLL_node("B", list(.prsLL_leaf("y"), .prsLL_leaf("z"))),
    .prsLL_node("C", list())
  ))
  expect_identical(.prsLL_linearise(tree), c("x", "y", "z"))
  expect_identical(.prsLL_linearise(.prsLL_leaf("q")), "q")
})

test_that("left recursion is detected, directly and through a chain", {
  direct <- .prsLL_grammar(list(
    list("E", c("E", "+", "T")),
    list("E", "T"),
    list("T", "id")
  ), start = "E")
  expect_identical(.prsLL_left_recursive(direct), "E")
  expect_false(.prsLL_is_ll1(direct)$ll1)

  # A -> B a, B -> A b: neither production starts with its own left-hand
  # side, but the cycle is still there
  indirect <- .prsLL_grammar(list(
    list("A", c("B", "a")),
    list("B", c("A", "b")),
    list("A", "x")
  ), start = "A")
  expect_setequal(.prsLL_left_recursive(indirect), c("A", "B"))

  expect_length(.prsLL_left_recursive(dragon()), 0L)
})

test_that("removing left recursion yields an LL(1) grammar for the same language", {
  direct <- .prsLL_grammar(list(
    list("E", c("E", "+", "T")),
    list("E", "T"),
    list("T", "id")
  ), start = "E")
  g2 <- .prsLL_remove_left_recursion(direct)
  expect_length(.prsLL_left_recursive(g2), 0L)
  expect_true(.prsLL_is_ll1(g2)$ll1)
  # the transformed grammar still accepts id, id + id, id + id + id
  for (toks in list("id", c("id", "+", "id"), c("id", "+", "id", "+", "id"))) {
    res <- morie_prsLL(g2, toks)
    expect_identical(res$yield, toks)
  }
})

test_that("a grammar that is not LL(1) is reported with the offending cell", {
  # S -> a S | a : both productions are entered on lookahead "a"
  amb <- .prsLL_grammar(list(
    list("S", c("a", "S")),
    list("S", "a")
  ), start = "S")
  r <- .prsLL_is_ll1(amb)
  expect_false(r$ll1)
  expect_length(r$conflicts, 1L)
  expect_identical(r$conflicts[[1]]$nonterminal, "S")
  expect_identical(r$conflicts[[1]]$lookahead, "a")
  expect_identical(r$conflicts[[1]]$rules, c(1L, 2L))
  # and parsing refuses rather than picking one
  expect_error(morie_prsLL(amb, c("a", "a")), "not LL\\(1\\)")
})

test_that("grammar construction rejects malformed input", {
  expect_error(.prsLL_grammar(list()), "no productions")
  expect_error(
    .prsLL_grammar(list(list("S", c("a", "")))),
    "empty right-hand side"
  )
  expect_error(
    .prsLL_grammar(list(list("S", c("a", "$")))),
    "reserved for end of input"
  )
  expect_error(
    .prsLL_grammar(list(list("S", "a")), start = "T"),
    "has no production"
  )
  expect_error(
    .prsLL_grammar(list(list("S", "a"), list("U", "b")), start = "S"),
    "cannot be reached"
  )
  expect_error(.prsLL_grammar(list(list(1L, "a"))), "non-empty symbol")
})

test_that("parsing rejects input the grammar does not accept", {
  g <- dragon()
  # no table cell for E on "+"
  expect_error(morie_prsLL(g, "+"), "no production for E")
  # trailing token left over
  expect_error(morie_prsLL(.prsLL_grammar(list(list("S", "a"))), c("a", "a")),
               "input not consumed")
  # a terminal mismatch inside a production
  expect_error(morie_prsLL(g, c("(", "id")), "no production for|expected")
  expect_error(morie_prsLL(g, "id", route = "sideways"), "route must be one of")
})

test_that("both routes agree on rejection, not just on acceptance", {
  g <- dragon()
  for (bad in list("+", c("id", "+"), c("(", "id"))) {
    expect_error(morie_prsLL(g, bad, route = "table"))
    expect_error(morie_prsLL(g, bad, route = "recursive_descent"))
  }
})

test_that("the set helpers behave as sets, preserving first-seen order", {
  expect_identical(.prsLL_union(c("a", "b"), c("b", "c")), c("a", "b", "c"))
  expect_identical(.prsLL_union(character(0), "a"), "a")
  expect_identical(.prsLL_union(c("a", "a"), character(0)), "a")
  expect_identical(.prsLL_setdiff(c("a", "b", "c"), "b"), c("a", "c"))
  expect_identical(.prsLL_setdiff(c("a", "b"), c("a", "b")), character(0))
  expect_true(.prsLL_subset(c("a", "b"), c("a", "b", "c")))
  expect_false(.prsLL_subset("z", c("a", "b")))
  expect_true(.prsLL_subset(character(0), "a"))
})

test_that("reachability follows productions from the start symbol only", {
  g <- .prsLL_grammar(list(
    list("S", c("A", "b")),
    list("A", c("B")),
    list("B", "c")
  ), start = "S")
  expect_setequal(.prsLL_reachable(g), c("S", "A", "B"))
})

test_that("the entry point accepts a rule list as well as a built grammar", {
  from_rules <- morie_prsLL(dragon_rules(), c("id", "+", "id"))
  from_grammar <- morie_prsLL(dragon(), c("id", "+", "id"))
  expect_identical(from_rules$tree, from_grammar$tree)
  expect_identical(from_rules$route, "table")
  expect_identical(from_rules$tokens, c("id", "+", "id"))
  expect_identical(from_rules$estimate, from_rules$tree)
  expect_match(from_rules$method, "Knuth")
})
