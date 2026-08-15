# morie.fn -- function file (rootcoder007/morie)
# ssmpar: The parallel scan that makes a state space model trainable.
#
# A linear recurrence x_t = A_t x_{t-1} + b_t is sequential by definition --
# L steps, no parallelism, which is exactly why RNNs lost to Transformers
# on modern hardware. The escape is that this recurrence is an associative
# operation in disguise. With (A_2, b_2) o (A_1, b_1) = (A_2 A_1, A_2 b_1 + b_2),
# composition is associative, so the prefix products can be computed by a
# parallel scan: an up-sweep building partial compositions and a down-sweep
# distributing them, O(L) work in O(log L) depth.
#
# References
# ----------
# Smith, J. T. H., Warrington, A. & Linderman, S. W. (2023)
# "Simplified State Space Layers for Sequence Modeling",
# International Conference on Learning Representations (ICLR 2023),
# arXiv:2208.04933.
# Gu, A. & Dao, T. (2024) "Mamba: Linear-Time Sequence Modeling with
# Selective State Spaces", Conference on Language Modeling (COLM 2024),
# arXiv:2312.00752.
# Blelloch, G. E. (1990) "Prefix Sums and Their Applications",
# Technical Report CMU-CS-90-190, School of Computer Science, Carnegie
# Mellon University.

.EPS <- 1e-12


compose <- function(left, right) {
  # (A2, b2) o (A1, b1) = (A2*A1, A2*b1 + b2)
  left <- as.numeric(left)
  right <- as.numeric(right)
  A1 <- left[1]
  b1 <- left[2]
  A2 <- right[1]
  b2 <- right[2]
  c(A2 * A1, A2 * b1 + b2)
}


sequential_scan <- function(pairs, x0 = 0.0) {
  # The recurrence as written: L steps, no parallelism.
  n <- length(pairs)
  if (n == 0) {
    return(list(states = numeric(0), steps = 0, depth = 0,
                note = paste("depth equals length -- the reason RNNs",
                             "do not use the hardware")))
  }
  P <- lapply(pairs, as.numeric)
  x <- as.numeric(x0)
  out <- numeric(n)
  for (i in seq_len(n)) {
    A <- P[[i]][1]
    b <- P[[i]][2]
    x <- A * x + b
    out[i] <- x
  }
  list(states = out, steps = n, depth = n,
       note = paste("depth equals length -- the reason RNNs",
                    "do not use the hardware"))
}


.ssmpar_upsweep <- function(P) {
  # Build the tree of partial compositions (private helper).
  tree <- list()
  level <- P
  while (length(level) > 1) {
    tree[[length(tree) + 1]] <- level
    nxt <- list()
    n_level <- length(level)
    for (i in seq(1, n_level - 1, 2)) {
      nxt[[length(nxt) + 1]] <- compose(level[[i]], level[[i + 1]])
    }
    if (n_level %% 2 == 1) {
      nxt[[length(nxt) + 1]] <- level[[n_level]]
    }
    level <- nxt
  }
  tree[[length(tree) + 1]] <- level
  tree
}


parallel_scan <- function(pairs, x0 = 0.0) {
  # Blelloch scan over the affine composition.
  # Same states as sequential_scan, O(log L) depth.
  P <- lapply(pairs, as.numeric)
  n <- length(P)
  if (n == 0) stop("ssmpar: the sequence is empty")

  tree <- .ssmpar_upsweep(P)

  prefix <- vector("list", n)
  for (i in seq_len(n)) {
    acc <- P[[i]]
    j <- i - 1
    step <- 1
    while (j >= 1) {
      lo <- j - step + 1
      if (lo >= 1 && (j %% step) == 0 && step <= j) {
        # Compose whole block P[lo..j] in one go
        blk <- P[[lo]]
        if (lo < j) {
          for (t in (lo + 1):(j)) {
            blk <- compose(blk, P[[t]])
          }
        }
        acc <- compose(blk, acc)
        j <- lo - 1
        step <- step * 2
      } else {
        acc <- compose(P[[j]], acc)
        j <- j - 1
      }
    }
    prefix[[i]] <- acc
  }

  x <- as.numeric(x0)
  states <- numeric(n)
  for (i in seq_len(n)) {
    A <- prefix[[i]][1]
    b <- prefix[[i]][2]
    states[i] <- A * x + b
  }

  list(states = states, prefix = prefix,
       depth = if (length(tree) > 1) length(tree) - 1 else 1,
       work = n,
       note = "identical states, logarithmic DEPTH")
}


check_associativity <- function(a, b, c, tol = 1e-12) {
  # Test (a o b) o c == a o (b o c) directly.
  left <- compose(compose(a, b), c)
  right <- compose(a, compose(b, c))
  d <- max(abs(left[1] - right[1]), abs(left[2] - right[2]))
  list(left = left, right = right, deviation = d,
       associative = d <= as.numeric(tol),
       note = "the property the parallel scan rests on")
}


scan_depth <- function(length) {
  # Sequential against parallel depth.
  n <- as.integer(length)
  if (n < 1) stop("ssmpar: the length must be positive")
  d <- if (n > 1) max(1, as.integer(ceiling(log2(n)))) else 1
  list(
    estimate = d, parallel_depth = d, sequential_depth = n,
    work = n, speedup = n / as.numeric(d),
    method = paste("parallel associative scan; Smith, Warrington &",
                   "Linderman (2023), after Blelloch (1990)"),
    note = paste("O(L) work in O(log L) depth; with input-dependent",
                 "parameters (Mamba) the convolutional shortcut is",
                 "gone and this scan is what is left")
  )
}


cheatsheet <- function() {
  paste0(
    "ssmpar: x_t = A_t x_{t-1} + b_t looks sequential, but each step is ",
    "an AFFINE MAP and composition (A2,b2)o(A1,b1) = (A2A1, A2b1+b2) is ",
    "ASSOCIATIVE -- so the prefixes come from a parallel scan: O(L) work, ",
    "O(log L) depth, identical states. Associativity is load-bearing ",
    "(cut the tree anywhere and the answer must not change), so test it. ",
    "This is what makes SELECTIVE state space models viable: once the ",
    "parameters depend on the input there is no fixed convolution kernel ",
    "left, and the scan is the only route to the hardware."
  )
}


# compact alias per ledger/NAMING.md
parallelscan <- parallel_scan

# public names resolved by fn/_lazy_map.json
ssm_parallel_scan <- parallel_scan

# entry point
morie_ssmpar <- parallel_scan

#' @rdname compose
#' @export
morie_ssmpar <- compose

#' @rdname compose
#' @export
morie_ssmpar <- compose

#' @rdname compose
#' @export
morie_ssmpar <- compose

#' @rdname compose
#' @export
morie_ssmpar <- compose

#' @rdname compose
#' @export
morie_ssmpar <- compose
