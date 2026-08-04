# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Applied machine-learning shelf -- R mirror of morie/fn/_geron.py and
# the nine shelf modules carved from it.
#
# Spec: Geron, A., Hands-On Machine Learning with Scikit-Learn and
# PyTorch, O'Reilly (2026). Locators are the printed page numbers of
# that edition, read off the running heads of the extracted text.
#
# Collision scan: ml_geron.R and all eight exported names were free in
# both R trees. morie_conv2d was ALREADY TAKEN by R/gp_ch13_15.R, so
# the convolutional layer here is exported as morie_convlayer.
#
# Determinism: the book's splitters take random_state = 42 and its
# MC-dropout listing takes torch.manual_seed(42). Neither generator
# survives translation, so this shelf uses the book's own deterministic
# alternative where it has one (the CRC-32 identifier hash of p. 58)
# and otherwise takes the stochastic part as caller-supplied input.

# --- CRC-32 (IEEE, reflected, polynomial 0xEDB88320) -------------------
#
# Written out rather than taken from a package: the p. 58 split is only
# reproducible if BOTH arms hash identically, and R has no CRC-32 in
# base. 32-bit values are carried as doubles and XORed in 16-bit
# halves, because R's integers are signed 32-bit and would overflow.

.morie_gr_xor32 <- function(a, b) {
  ah <- a %/% 65536; al <- a %% 65536
  bh <- b %/% 65536; bl <- b %% 65536
  bitwXor(ah, bh) * 65536 + bitwXor(al, bl)
}

.morie_gr_crctab <- local({
  tab <- numeric(256)
  for (n in 0:255) {
    cc <- n
    for (k in 1:8) {
      cc <- if (cc %% 2 == 1) .morie_gr_xor32(3988292384, cc %/% 2) else cc %/% 2
    }
    tab[n + 1L] <- cc
  }
  tab
})

.morie_gr_crc32 <- function(value) {
  v <- value
  if (v < 0 || v >= 2^53) {
    stop("ids must be non-negative and below 2^53.", call. = FALSE)
  }
  crc <- 4294967295
  for (i in 0:7) {
    byte <- floor(v / 256^i) %% 256
    idx <- bitwXor(crc %% 256, byte)
    crc <- .morie_gr_xor32(.morie_gr_crctab[idx + 1L], crc %/% 256)
  }
  .morie_gr_xor32(crc, 4294967295)
}


# --- ch. 4, p. 158: the bias/variance trade-off ------------------------

#' Squared-error decomposition into bias, variance and noise (p. 158)
#'
#' p. 158 states IN WORDS that the generalization error is the sum of
#' bias, variance and irreducible error; NO formula is printed there.
#' The decomposition computed here is stated explicitly rather than
#' attributed to the book:
#' MSE = mean_i (mean_b f_b(x_i) - y_i)^2 + mean_i var_b(f_b(x_i)) +
#' noisevar.
#' @param preds B-by-n matrix, one row per model, one column per point
#' @param truth the n true values
#' @param noisevar the irreducible error
#' @return list(bias2, variance, noise, total, mse, residual, b, n)
#' @export
morie_bvdecomp <- function(preds, truth, noisevar = 0) {
  p <- as.matrix(preds)
  storage.mode(p) <- "double"
  b <- nrow(p)
  n <- ncol(p)
  if (b < 2L) stop("need at least 2 predictor rows.", call. = FALSE)
  y <- as.numeric(truth)
  if (length(y) != n) {
    stop("truth must have one entry per test point.", call. = FALSE)
  }
  means <- colSums(p) / b
  bias2 <- sum((means - y)^2) / n
  v <- sum(vapply(seq_len(n), function(i) sum((p[, i] - means[i])^2) / b,
                  numeric(1))) / n
  mse <- sum(vapply(seq_len(n), function(i) sum((p[, i] - y[i])^2) / b,
                    numeric(1))) / n
  noisevar <- as.numeric(noisevar)
  list(bias2 = bias2, variance = v, noise = noisevar,
       total = bias2 + v + noisevar, mse = mse,
       residual = mse - bias2 - v, b = b, n = n)
}

# --- ch. 11, p. 410: Monte Carlo dropout -------------------------------

#' Average the softmax over repeated stochastic forward passes (p. 410)
#'
#' The listing on p. 410 is softmax(logits) over a batch repeated T
#' times, then .mean(dim = 1) across the T passes. logits here is that
#' same structure -- an n by T by k array -- supplied by the caller, so
#' the dropout masks live outside this function and both language arms
#' see identical numbers.
#' @param logits n by T by k array of class logits
#' @return list(probs, sds, pred, meanmaxprob, meanmaxsd, meanentropy,
#'   n, t, k)
#' @export
morie_mcdrop <- function(logits) {
  a <- as.array(logits)
  d <- dim(a)
  if (length(d) != 3L) stop("logits must be an n by T by k array.", call. = FALSE)
  n <- d[1]; tt <- d[2]; k <- d[3]
  probs <- matrix(0, n, k)
  sds <- matrix(0, n, k)
  ents <- numeric(n)
  for (i in seq_len(n)) {
    passes <- t(vapply(seq_len(tt),
                       function(j) .morie_gr_softmax(as.numeric(a[i, j, ])),
                       numeric(k)))
    if (k == 1L) passes <- matrix(passes, ncol = 1L)
    mean_ <- colSums(passes) / tt
    probs[i, ] <- mean_
    sds[i, ] <- vapply(seq_len(k),
                       function(c) sqrt(sum((passes[, c] - mean_[c])^2) / tt),
                       numeric(1))
    pos <- mean_[mean_ > 0]
    ents[i] <- -sum(pos * log(pos))
  }
  top <- apply(probs, 1, which.max)
  list(probs = probs, sds = sds, pred = as.integer(top - 1L),
       meanmaxprob = sum(vapply(seq_len(n), function(i) probs[i, top[i]],
                                numeric(1))) / n,
       meanmaxsd = sum(vapply(seq_len(n), function(i) sds[i, top[i]],
                              numeric(1))) / n,
       meanentropy = sum(ents) / n, n = n, t = tt, k = k)
}

# --- ch. 2, pp. 58-61: splitting off a test set ------------------------

#' Stable train/test split by identifier hash (p. 58)
#'
#' The book's own stable alternative to a seeded shuffle: an instance
#' goes to the test set when crc32(int64(id)) < testratio * 2^32. It is
#' a pure function of the identifiers, which is why the book prefers it
#' -- and why it survives translation to R unchanged.
#' @param ids non-negative integer identifiers
#' @param testratio share of instances to hold out
#' @return list(test, train, ntest, ntrain, ratio, n)
#' @export
morie_ttsplit <- function(ids, testratio = 0.2) {
  ids <- as.numeric(ids)
  if (length(ids) == 0L) stop("ids must be non-empty.", call. = FALSE)
  testratio <- as.numeric(testratio)
  if (!(testratio > 0 && testratio < 1)) {
    stop("testratio must lie strictly in (0, 1).", call. = FALSE)
  }
  cut <- testratio * 2^32
  h <- vapply(ids, .morie_gr_crc32, numeric(1))
  test <- which(h < cut) - 1L
  n <- length(ids)
  list(test = test, train = setdiff(seq_len(n) - 1L, test),
       ntest = length(test), ntrain = n - length(test),
       ratio = length(test) / n, n = n)
}

#' Three-way train/validation/test split by identifier hash
#'
#' p. 61 obtains a three-way split by calling the splitter twice; the
#' hash generalization used here -- one CRC-32 per identifier, mapped
#' to [0, 1) and cut at testratio and testratio + valratio -- is OURS,
#' not the book's, and is used because it stays deterministic across
#' both language arms. The p. 58 hash rule itself is the book's.
#' @param ids non-negative integer identifiers
#' @param valratio share held out for validation
#' @param testratio share held out for test
#' @return list(train, val, test, ntrain, nval, ntest, n)
#' @export
morie_tvtsplit <- function(ids, valratio = 0.2, testratio = 0.2) {
  ids <- as.numeric(ids)
  if (length(ids) == 0L) stop("ids must be non-empty.", call. = FALSE)
  valratio <- as.numeric(valratio); testratio <- as.numeric(testratio)
  if (valratio <= 0 || testratio <= 0 || valratio + testratio >= 1) {
    stop("valratio and testratio must be positive and sum below 1.", call. = FALSE)
  }
  h <- vapply(ids, .morie_gr_crc32, numeric(1)) / 2^32
  n <- length(ids)
  test <- which(h < testratio) - 1L
  val <- which(h >= testratio & h < testratio + valratio) - 1L
  train <- which(h >= testratio + valratio) - 1L
  list(train = train, val = val, test = test, ntrain = length(train),
       nval = length(val), ntest = length(test), n = n)
}

#' Stratified test split by proportional allocation (pp. 60-61)
#'
#' The book's point is the GUARANTEE, not the shuffle: the right number
#' of instances are sampled from each stratum so the test set is
#' representative. Allocation here is exactly that -- round(n_s *
#' testratio) from each stratum -- with members taken in their original
#' order, so the result is reproducible without a seed. maxdev is the
#' largest gap between a stratum's share of the test set and its share
#' of the population, the quantity the book checks on p. 61.
#' @param strata stratum label per instance
#' @param testratio share of instances to hold out
#' @return list(test, train, ntest, ntrain, maxdev, nstrata, n)
#' @export
morie_stratsplt <- function(strata, testratio = 0.2) {
  strata <- as.character(strata)
  n <- length(strata)
  if (n < 1L) stop("strata must be non-empty.", call. = FALSE)
  testratio <- as.numeric(testratio)
  if (!(testratio > 0 && testratio < 1)) {
    stop("testratio must lie strictly in (0, 1).", call. = FALSE)
  }
  levels_ <- sort(unique(strata))
  test <- integer(0)
  for (lv in levels_) {
    idx <- which(strata == lv) - 1L
    take <- floor(length(idx) * testratio + 0.5)
    if (take > 0) test <- c(test, idx[seq_len(take)])
  }
  test <- sort(test)
  ntest <- length(test)
  devs <- vapply(levels_, function(lv) {
    pop <- sum(strata == lv) / n
    got <- if (ntest > 0L) sum(strata[test + 1L] == lv) / ntest else 0
    abs(got - pop)
  }, numeric(1))
  list(test = test, train = setdiff(seq_len(n) - 1L, test), ntest = ntest,
       ntrain = n - ntest, maxdev = if (length(devs)) max(devs) else 0,
       nstrata = length(levels_), n = n)
}

# --- ch. 12, p. 423: Equation 12-1, the convolutional layer ------------

#' Output of a convolutional layer, Equation 12-1 (p. 423)
#'
#' z[i, j, k] = b[k] + sum_u sum_v sum_kp x[ip, jp, kp] w[u, v, kp, k]
#' with ip = i * sh + u and jp = j * sw + v. This is a
#' cross-correlation, as the book's own footnote 6 on p. 419 points
#' out. Exported as morie_convlayer because morie_conv2d was already
#' taken by R/gp_ch13_15.R.
#' @param x height by width by in-channels array
#' @param kernel fh by fw by in-channels by out-channels array
#' @param bias one entry per output feature map
#' @param stride c(sh, sw)
#' @param padding c(ph, pw) zero padding
#' @return list(z, height, width, channels, total, maxz, nparams)
#' @export
morie_convlayer <- function(x, kernel, bias = NULL, stride = c(1, 1),
                            padding = c(0, 0)) {
  xs <- as.array(x); ks <- as.array(kernel)
  dx <- dim(xs); dk <- dim(ks)
  if (length(dx) != 3L || length(dk) != 4L) {
    stop("x must be 3-D and kernel 4-D.", call. = FALSE)
  }
  h <- dx[1]; w <- dx[2]; cin <- dx[3]
  fh <- dk[1]; fw <- dk[2]; cout <- dk[4]
  if (dk[3] != cin) stop("kernel in-channels must match the input.", call. = FALSE)
  sh <- as.integer(stride[1]); sw <- as.integer(stride[2])
  ph <- as.integer(padding[1]); pw <- as.integer(padding[2])
  if (sh < 1L || sw < 1L || ph < 0L || pw < 0L) {
    stop("stride must be positive and padding non-negative.", call. = FALSE)
  }
  b <- if (is.null(bias)) rep(0, cout) else as.numeric(bias)
  if (length(b) != cout) {
    stop("bias must have one entry per output feature map.", call. = FALSE)
  }
  oh <- (h + 2L * ph - fh) %/% sh + 1L
  ow <- (w + 2L * pw - fw) %/% sw + 1L
  if (oh < 1L || ow < 1L) stop("kernel is larger than the padded input.", call. = FALSE)
  z <- array(0, c(oh, ow, cout))
  for (i in seq_len(oh)) {
    for (j in seq_len(ow)) {
      cell <- b
      for (u in seq_len(fh)) {
        ii <- (i - 1L) * sh + u - ph
        if (ii < 1L || ii > h) next
        for (v in seq_len(fw)) {
          jj <- (j - 1L) * sw + v - pw
          if (jj < 1L || jj > w) next
          for (cc in seq_len(cin)) {
            xv <- xs[ii, jj, cc]
            if (xv == 0) next
            cell <- cell + xv * as.numeric(ks[u, v, cc, ])
          }
        }
      }
      z[i, j, ] <- cell
    }
  }
  list(z = z, height = oh, width = ow, channels = cout, total = sum(z),
       maxz = max(z), nparams = fh * fw * cin * cout + cout)
}

# --- ch. 12, p. 476: object tracking -----------------------------------

.morie_gr_perms <- function(seq_, k) {
  if (k == 0L) return(list(integer(0)))
  out <- list()
  for (i in seq_along(seq_)) {
    for (rest in .morie_gr_perms(seq_[-i], k - 1L)) {
      out[[length(out) + 1L]] <- c(seq_[i], rest)
    }
  }
  out
}

.morie_gr_combos <- function(seq_, k) {
  if (k == 0L) return(list(integer(0)))
  if (length(seq_) < k) return(list())
  out <- list()
  for (i in seq_len(length(seq_) - k + 1L)) {
    for (rest in .morie_gr_combos(seq_[-seq_len(i)], k - 1L)) {
      out[[length(out) + 1L]] <- c(seq_[i], rest)
    }
  }
  out
}

#' Minimum-cost detection-to-track assignment (p. 476)
#'
#' p. 476 describes DeepSORT in words and prints no formula, but it
#' does state the objective the assignment step solves: it finds the
#' mapping that minimizes the distance between the detections and the
#' predicted positions of tracked objects while also minimizing the
#' appearance discrepancy. That objective is what is implemented --
#' (1 - weight) * position + weight * appearance, minimized over all
#' one-to-one mappings. The Kalman prediction and the appearance
#' network are the CALLER's; this takes their output as cost matrices.
#'
#' ponytail: exact optimum by exhaustive search, capped at maxn tracks.
#' The Hungarian algorithm the book names gives the same answer in
#' O(n^3) -- swap it in if n ever exceeds a handful.
#' @param posdist tracks-by-detections position cost matrix
#' @param appdist matching appearance cost matrix, or NULL
#' @param weight the appearance share of the combined cost, in [0, 1]
#' @param maxn cap on the exhaustive search
#' @return list(cost, assignment, nmatched, nunmatchedtracks,
#'   nunmatcheddets, meancost)
#' @export
morie_trkassign <- function(posdist, appdist = NULL, weight = 0.5, maxn = 8L) {
  pos <- as.matrix(posdist)
  storage.mode(pos) <- "double"
  nt <- nrow(pos); nd <- ncol(pos)
  if (nt < 1L) stop("posdist must have at least one row.", call. = FALSE)
  if (nt > maxn || nd > maxn) {
    stop("exhaustive assignment is capped at maxn.", call. = FALSE)
  }
  weight <- as.numeric(weight)
  if (weight < 0 || weight > 1) stop("weight must lie in [0, 1].", call. = FALSE)
  app <- if (is.null(appdist)) matrix(0, nt, nd) else as.matrix(appdist)
  storage.mode(app) <- "double"
  if (nrow(app) != nt || ncol(app) != nd) {
    stop("appdist must match the shape of posdist.", call. = FALSE)
  }
  cost <- (1 - weight) * pos + weight * app
  m <- min(nt, nd)
  best <- NULL
  bestcost <- Inf
  for (rows in .morie_gr_combos(seq_len(nt), m)) {
    for (cols in .morie_gr_perms(seq_len(nd), m)) {
      cvals <- sum(vapply(seq_len(m), function(q) cost[rows[q], cols[q]],
                          numeric(1)))
      if (cvals < bestcost) {
        bestcost <- cvals
        best <- cbind(rows - 1L, cols - 1L)
      }
    }
  }
  list(cost = bestcost, assignment = best, nmatched = m,
       nunmatchedtracks = nt - m, nunmatcheddets = nd - m,
       meancost = bestcost / m)
}

# --- ch. 12, pp. 458-459: using a pretrained model ---------------------

#' Preprocess an image for a pretrained model (pp. 458-459)
#'
#' The section's substance is the preprocessing contract, not the
#' download: the image must be brought to the size the pretrained model
#' expects (224 x 224 for ConvNeXt) and the pixel intensities
#' standardized per colour channel using ImageNet's means and standard
#' deviations. Those constants are NOT printed in the extracted text,
#' so mean and sd are required arguments rather than baked-in defaults.
#'
#' ponytail: centre crop to a square, then nearest-neighbour subsample.
#' weights.transforms() uses bilinear resizing; swap the sampler if you
#' need to match torchvision to the last decimal.
#' @param image height by width by channels array
#' @param size the square edge the model expects
#' @param mean,sd per-channel standardization constants
#' @param logits optional class logits, for the p. 459 argmax read-out
#' @param topk how many classes to report
#' @return list(pixels, size, channels, cropside, pixelmean, pixelmax,
#'   and pred/topk/topprob when logits are supplied)
#' @export
morie_pretprep <- function(image, size, mean, sd, logits = NULL, topk = 1L) {
  img <- as.array(image)
  d <- dim(img)
  if (length(d) != 3L) stop("image must be a 3-D array.", call. = FALSE)
  h <- d[1]; w <- d[2]; cc <- d[3]
  size <- as.integer(size)
  if (size < 1L) stop("size must be positive.", call. = FALSE)
  mean <- as.numeric(mean); sd <- as.numeric(sd)
  if (length(mean) != cc || length(sd) != cc || any(sd <= 0)) {
    stop("mean and sd need one positive entry per channel.", call. = FALSE)
  }
  side <- min(h, w)
  r0 <- (h - side) %/% 2L
  c0 <- (w - side) %/% 2L
  out <- array(0, c(size, size, cc))
  for (i in seq_len(size)) {
    for (j in seq_len(size)) {
      si <- r0 + ((i - 1L) * side) %/% size + 1L
      sj <- c0 + ((j - 1L) * side) %/% size + 1L
      out[i, j, ] <- (as.numeric(img[si, sj, ]) - mean) / sd
    }
  }
  res <- list(pixels = out, size = size, channels = cc, cropside = side,
              pixelmean = sum(out) / length(out), pixelmax = max(out))
  if (!is.null(logits)) {
    lg <- as.numeric(logits)
    ord <- order(-lg, seq_along(lg))
    res$pred <- as.integer(ord[1] - 1L)
    res$topk <- as.integer(ord[seq_len(as.integer(topk))] - 1L)
    res$topprob <- .morie_gr_softmax(lg)[ord[1]]
  }
  res
}
