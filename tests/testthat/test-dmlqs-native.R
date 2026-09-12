# D-MPNN: messages on directed bonds, excluding the reverse edge (Yang et
# al. 2019).
#
# Anchors outside the module: walk counts on graphs small enough to
# enumerate by hand, the defining property that excluding the reverse edge
# leaves no tottering walk at all, and message updates computed longhand on
# a three-atom path.

# a path 1 - 2 - 3
PATH3 <- list("1" = 2L, "2" = c(1L, 3L), "3" = 2L)
# a triangle
TRI <- list("1" = c(2L, 3L), "2" = c(1L, 3L), "3" = c(1L, 2L))

test_that("edge keys name a direction", {
  expect_equal(.dmlqs_edge_key(1, 2), "1,2")
  expect_equal(.dmlqs_edge_key(2, 1), "2,1")
  # the key is directed, so the two orders differ
  expect_false(.dmlqs_edge_key(1, 2) == .dmlqs_edge_key(2, 1))
  expect_equal(.dmlqs_edge_key(10, 3), "10,3")
})

test_that("adjacency is normalised to sorted unique neighbours", {
  a <- .dmlqs_norm_adj(list("1" = c(3L, 2L, 2L), "2" = 1L))
  expect_equal(a[["1"]], c(2L, 3L))
  expect_equal(a[["2"]], 1L)
  # a self-loop is dropped
  expect_equal(.dmlqs_norm_adj(list("1" = c(1L, 2L)))[["1"]], 2L)
  # an empty neighbour set survives as empty
  expect_length(.dmlqs_norm_adj(list("1" = integer(0)))[["1"]], 0L)
  expect_length(.dmlqs_norm_adj(NULL), 0L)
  # a matrix is read with rows as sources and non-zero entries as edges
  M <- matrix(0, 3, 3)
  M[1, 2] <- 1
  M[2, 1] <- 1
  M[2, 3] <- 1
  M[3, 2] <- 1
  m <- .dmlqs_norm_adj(M)
  expect_equal(m[["1"]], 2L)
  expect_equal(m[["2"]], c(1L, 3L))
  expect_equal(m[["3"]], 2L)
  # a matrix diagonal is a self-loop and is dropped
  Md <- diag(3)
  expect_length(.dmlqs_norm_adj(Md)[["1"]], 0L)
  expect_error(.dmlqs_norm_adj("nonsense"),
               "adj must be a named list, named integer vector, or matrix")
})

test_that("directed edges are both orientations of every bond", {
  e <- .dmlqs_directed_edges(PATH3)
  # two bonds, so four directed edges
  expect_length(e, 4L)
  keys <- vapply(e, function(p) paste(p, collapse = ","), character(1))
  expect_setequal(keys, c("1,2", "2,1", "2,3", "3,2"))
  # a triangle has three bonds and six directed edges
  expect_length(.dmlqs_directed_edges(TRI), 6L)
  # an isolated atom contributes none
  expect_length(.dmlqs_directed_edges(list("1" = integer(0))), 0L)
  expect_length(.dmlqs_directed_edges(NULL), 0L)
  # every edge is a pair of atom ids
  for (p in e) expect_length(p, 2L)
})

test_that("excluding the reverse edge removes every tottering walk", {
  # This is the paper's whole point, and it is exact rather than
  # approximate: a walk cannot return to where it was two steps ago if it
  # is never allowed to step back.
  for (g in list(PATH3, TRI)) {
    ex <- .dmlqs_count_totters(g, length = 3L, exclude_reverse = TRUE)
    expect_equal(ex$totters, 0L)
    expect_equal(ex$fraction, 0)
    expect_true(ex$excluded_reverse)
    # and allowing it produces tottering walks
    inc <- .dmlqs_count_totters(g, length = 3L, exclude_reverse = FALSE)
    expect_true(inc$totters > 0L)
    expect_true(inc$fraction > 0)
    expect_false(inc$excluded_reverse)
    expect_true(inc$paths > ex$paths)
  }
  # the counts on the path 1-2-3 are small enough to enumerate: with
  # backtracking barred only (1,2,3) and (3,2,1) survive
  p_ex <- .dmlqs_count_totters(PATH3, length = 3L, exclude_reverse = TRUE)
  expect_equal(p_ex$paths, 2L)
  # allowing it, each of the three starts has two continuations, and the
  # four walks that come straight back are the totters
  p_in <- .dmlqs_count_totters(PATH3, length = 3L, exclude_reverse = FALSE)
  expect_equal(p_in$paths, 6L)
  expect_equal(p_in$totters, 4L)
  expect_equal(p_in$fraction, 4 / 6)
  # on the triangle every start has two choices then one, and none totters
  t_ex <- .dmlqs_count_totters(TRI, length = 3L, exclude_reverse = TRUE)
  expect_equal(t_ex$paths, 6L)
  t_in <- .dmlqs_count_totters(TRI, length = 3L, exclude_reverse = FALSE)
  expect_equal(t_in$paths, 12L)
  expect_equal(t_in$totters, 6L)
  # longer walks are also counted, and still never totter when barred
  expect_equal(.dmlqs_count_totters(TRI, length = 4L,
                                    exclude_reverse = TRUE)$totters, 0L)
  expect_true(.dmlqs_count_totters(TRI, length = 4L,
                                   exclude_reverse = TRUE)$paths > 0L)
  expect_error(.dmlqs_count_totters(PATH3, length = 2L),
               "at least 3 steps")
})

test_that("the activations are their closed forms", {
  x <- c(-2, -0.5, 0, 0.5, 2)
  expect_equal(.dmlqs_act(x, "relu"), pmax(0, x))
  expect_equal(.dmlqs_act(x, "tanh"), tanh(x))
  expect_equal(.dmlqs_act(0, "relu"), 0)
  # the rectifier is idempotent on its own output
  expect_equal(.dmlqs_act(.dmlqs_act(x, "relu"), "relu"),
               .dmlqs_act(x, "relu"))
  # tanh is odd and bounded
  expect_equal(.dmlqs_act(-x, "tanh"), -.dmlqs_act(x, "tanh"))
  expect_true(all(abs(.dmlqs_act(c(-50, 50), "tanh")) <= 1))
  expect_error(.dmlqs_act(x, "sigmoid"), "activation must be relu or tanh")
})

test_that("messages sum the incoming edges except the reverse one", {
  # On the path 1-2-3 the only edge feeding (2,3) is (1,2), and nothing
  # feeds (1,2) because atom 1 has no other neighbour. So after one step
  #   h_12 = relu(h0_12)
  #   h_23 = relu(h0_23 + h_12)
  h0 <- list("1,2" = 1, "2,1" = 2, "2,3" = 3, "3,2" = 4)
  r <- .dmlqs_message_pass(h0, PATH3, T = 1L)
  H <- r$edge_states
  expect_equal(H[["1,2"]], 1)
  expect_equal(H[["2,3"]], 3 + 1)
  # symmetrically, (2,1) is fed by (3,2) and (3,2) by nothing
  expect_equal(H[["3,2"]], 4)
  expect_equal(H[["2,1"]], 2 + 4)
  expect_equal(r$T, 1L)
  expect_true(r$excluded_reverse)
  # a second step propagates once more from the updated states
  r2 <- .dmlqs_message_pass(h0, PATH3, T = 2L)
  expect_equal(r2$edge_states[["2,3"]], 3 + 1)
  expect_equal(r2$T, 2L)
  # allowing the reverse edge lets a message come straight back, which is
  # exactly the tottering the paper removes
  rb <- .dmlqs_message_pass(h0, PATH3, T = 1L, exclude_reverse = FALSE)
  expect_false(rb$excluded_reverse)
  expect_equal(rb$edge_states[["1,2"]], 1 + 2)
  expect_true(rb$edge_states[["1,2"]] > H[["1,2"]])
  # the rectifier clamps a negative update
  neg <- .dmlqs_message_pass(list("1,2" = -5, "2,1" = -5, "2,3" = -5,
                                  "3,2" = -5), PATH3, T = 1L)
  expect_true(all(unlist(neg$edge_states) >= 0))
  expect_match(r$note, "reverse edge")
})

test_that("a weight matrix is applied to the aggregated message", {
  h0 <- list("1,2" = c(1, 0), "2,1" = c(0, 1), "2,3" = c(1, 1),
             "3,2" = c(2, 0))
  # the identity leaves the sum alone
  I2 <- diag(2)
  ri <- .dmlqs_message_pass(h0, PATH3, T = 1L, W = I2, activation = "tanh")
  rn <- .dmlqs_message_pass(h0, PATH3, T = 1L, activation = "tanh")
  expect_equal(ri$edge_states[["2,3"]], rn$edge_states[["2,3"]])
  # doubling the weights doubles the message contribution
  rd <- .dmlqs_message_pass(h0, PATH3, T = 1L, W = 2 * I2,
                            activation = "tanh")
  expect_equal(rd$edge_states[["2,3"]],
               tanh(h0[["2,3"]] + 2 * h0[["1,2"]]))
  expect_error(.dmlqs_message_pass(h0, PATH3, T = 1L, W = diag(3)),
               "W must be a 2 x 2 matrix")
  expect_error(.dmlqs_message_pass(h0, PATH3, T = 0L),
               "T must be at least 1")
  expect_error(.dmlqs_message_pass(list(), PATH3), "non-empty mapping")
  expect_error(.dmlqs_message_pass(list(1, 2), PATH3), "named mapping")
  expect_error(.dmlqs_message_pass(list("1" = 1), PATH3), "is not 'v,w'")
})

test_that("the atom readout sums the edges pointing at each atom", {
  es <- list("1,2" = c(1, 0), "2,1" = c(0, 1), "2,3" = c(1, 1),
             "3,2" = c(2, 0))
  out <- .dmlqs_atom_readout(es, PATH3, 3L)
  expect_length(out, 3L)
  # atom 1 receives only from atom 2
  expect_equal(out[[1]], c(0, 1))
  # atom 2 receives from atoms 1 and 3
  expect_equal(out[[2]], c(1, 0) + c(2, 0))
  # atom 3 receives only from atom 2
  expect_equal(out[[3]], c(1, 1))
  # an atom with no neighbours reads out zero, not an error
  iso <- .dmlqs_atom_readout(es, list("1" = integer(0)), 1L)
  expect_equal(iso[[1]], c(0, 0))
  # no edge states at all gives empty vectors
  empt <- .dmlqs_atom_readout(list(), PATH3, 3L)
  expect_length(empt, 3L)
  expect_length(empt[[1]], 0L)
})

test_that("descriptors are concatenated onto the learned vector", {
  r <- .dmlqs_concat_descriptors(c(1, 2, 3), c(10, 20))
  expect_equal(r$representation, c(1, 2, 3, 10, 20))
  expect_equal(r$estimate, r$representation)
  expect_equal(r$learned_dim, 3L)
  expect_equal(r$descriptor_dim, 2L)
  expect_match(r$method, "Yang et al")
  # an empty descriptor set leaves the learned vector alone
  expect_equal(.dmlqs_concat_descriptors(c(1, 2), numeric(0))$representation,
               c(1, 2))
  expect_equal(.dmlqs_concat_descriptors(numeric(0), c(3))$learned_dim, 0L)
})

test_that("the dispatcher routes every documented alias", {
  h0 <- list("1,2" = 1, "2,1" = 2, "2,3" = 3, "3,2" = 4)
  direct <- .dmlqs_directed_edges(PATH3)
  for (nm in c("directed_edges", "directededges")) {
    expect_equal(morie_dmlqs(nm, PATH3), direct)
  }
  tot <- .dmlqs_count_totters(PATH3, length = 3L)
  for (nm in c("count_totters", "counttotters")) {
    expect_equal(morie_dmlqs(nm, PATH3, length = 3L), tot)
  }
  mp <- .dmlqs_message_pass(h0, PATH3, T = 1L)
  for (nm in c("dmpnn_message_pass", "dmpnn_messagepass", "directedmpnn",
               "deepml_qsar", "deepmlqsar", "message_pass", "messagepass")) {
    expect_equal(morie_dmlqs(nm, h0, PATH3, T = 1L), mp)
  }
  es <- list("1,2" = c(1, 0), "2,1" = c(0, 1), "2,3" = c(1, 1),
             "3,2" = c(2, 0))
  ro <- .dmlqs_atom_readout(es, PATH3, 3L)
  for (nm in c("atom_readout", "atomreadout")) {
    expect_equal(morie_dmlqs(nm, es, PATH3, 3L), ro)
  }
  cd <- .dmlqs_concat_descriptors(c(1, 2), c(3))
  for (nm in c("concat_descriptors", "concatdescriptors")) {
    expect_equal(morie_dmlqs(nm, c(1, 2), c(3)), cd)
  }
  # the default route is the message pass
  expect_equal(morie_dmlqs(clean <- "dmpnn_message_pass", h0, PATH3, T = 1L),
               mp)
})
