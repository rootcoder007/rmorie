# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Shared layer algebra for the AlphaFold modules (Jumper et al. 2021).
#
# Reference is the Supplementary Information of Jumper et al. (2021),
# "Highly accurate protein structure prediction with AlphaFold", Nature
# 596:583-589, which specifies the network as numbered pseudocode
# (Algorithms 1-32).
#
# Every projection takes its weight matrix from the caller; nothing is
# initialised randomly and no trained value is baked in.  Helper names all
# carry the "alf" prefix so that none of them can mask a base R function
# (alfRap would otherwise collide with base::rapply).

alfSigm <- function(x) ifelse(x >= 0, 1 / (1 + exp(-x)), exp(x) / (1 + exp(x)))

alfRelu <- function(x) ifelse(x > 0, x, 0)

# Softmax with max subtraction.  The stabilisation is part of the contract:
# the Python arm subtracts the same maximum.
alfSmax <- function(v) {
  e <- exp(v - max(v))
  e / sum(e)
}

alfVdot <- function(a, b) sum(a * b)

alfVn2 <- function(a) sum(a * a)

# Dense projection of a vector; W is (n_out x n_in).
alfLin <- function(v, W, b = NULL) {
  o <- as.numeric(W %*% as.numeric(v))
  if (!is.null(b)) o <- o + as.numeric(b)
  o
}

# Layer normalisation over the channel axis of one vector.  Uses the
# population variance (divide by n), matching the Python arm.
alfLnorm <- function(v, g = NULL, b = NULL, eps = 1e-5) {
  v <- as.numeric(v)
  n <- length(v)
  mu <- sum(v) / n
  vr <- sum((v - mu)^2) / n
  o <- (v - mu) / sqrt(vr + eps)
  if (!is.null(g)) o <- o * as.numeric(g)
  if (!is.null(b)) o <- o + as.numeric(b)
  o
}

# A frame is list(R = 3x3 matrix, t = length-3 vector); T o x = R x + t.
alfRap <- function(Tf, x) as.numeric(Tf$R %*% as.numeric(x)) + as.numeric(Tf$t)

alfRinv <- function(Tf) {
  Rt <- t(Tf$R)
  list(R = Rt, t = -as.numeric(Rt %*% as.numeric(Tf$t)))
}

# T^-1 o x, without materialising the inverse frame.
alfRinvap <- function(Tf, x) {
  as.numeric(t(Tf$R) %*% (as.numeric(x) - as.numeric(Tf$t)))
}

# Frame composition A o B, i.e. (A o B) o x = A o (B o x).
alfRcomp <- function(A, B) list(R = A$R %*% B$R, t = alfRap(A, B$t))

# Non-unit quaternion (1, b, c, d) to a rotation matrix -- Algorithm 23
# lines 2-3.  Fixing the leading component to 1 before normalisation both
# guarantees a unit quaternion and makes zero input the identity rotation.
alfQ2rot <- function(b, c, d) {
  n <- sqrt(1 + b * b + c * c + d * d)
  a <- 1 / n; b <- b / n; c <- c / n; d <- d / n
  matrix(c(
    a * a + b * b - c * c - d * d, 2 * b * c - 2 * a * d, 2 * b * d + 2 * a * c,
    2 * b * c + 2 * a * d, a * a - b * b + c * c - d * d, 2 * c * d - 2 * a * b,
    2 * b * d - 2 * a * c, 2 * c * d + 2 * a * b, a * a - b * b - c * c + d * d
  ), nrow = 3, ncol = 3, byrow = TRUE)
}

alfIdent <- function() list(R = diag(3), t = c(0, 0, 0))

# One-hot encoding with nearest bin -- Algorithm 5.  Ties go to the lowest
# index, matching arg min in the pseudocode and which.min here.
alfOnehot <- function(x, bins) {
  p <- numeric(length(bins))
  p[which.min(abs(x - bins))] <- 1
  p
}

# Cross entropy -sum(y log p) for one distribution.
alfXent <- function(y, p, eps = 1e-12) {
  -sum(y * log(pmax(p, eps)))
}
