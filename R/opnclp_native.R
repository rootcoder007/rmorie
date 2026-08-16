# OpenCLIP: scaling laws you can actually reproduce.
# Sources: Cherti, M., Beaumont, R., Wightman, R., Wortsman, M.,
# Ilharco, G., Gordon, C., Schuhmann, C., Schmidt, L. & Jitsev, J.
# (2023) "Reproducible scaling laws for contrastive language-image
# learning", CVPR 2023, 2818-2829, arXiv:2212.07143. The use of
# public LAION data and an open implementation; experiments up to
# 2B image-text pairs finding power-law scaling for zero-shot
# classification, retrieval, linear probing and end-to-end
# fine-tuning; and the finding that the training distribution plays
# a key role, as the OpenAI and OpenCLIP models scale differently
# despite identical architectures and similar recipes. Radford, A.
# et al. (2021) "Learning Transferable Visual Models From Natural
# Language Supervision", ICML 2021, arXiv:2103.00020, for CLIP
# itself. Kaplan, J. et al. (2020) "Scaling Laws for Neural
# Language Models", arXiv:2001.08361, for the power-law form.

# Base R only, faithful translation of opnclp_python_reference.py.

.OPNCLP_EPS <- 1e-12

#' .opnclp_vec
#'
#' A step of the opnclp_native implementation. Called by \code{fit_power_law}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param v Passed to \code{unlist}.
#' @return A vector, from \code{as.numeric}.
#' @export
.opnclp_vec <- function(v) as.numeric(unlist(v))

#' .opnclp_mat
#'
#' A step of the opnclp_native implementation. Called by \code{infonce}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param m A matrix; the body checks with \code{is.matrix}.
#' @return One of two values, depending on the branch taken.
#' @export
.opnclp_mat <- function(m) {
  if (is.matrix(m)) {
    storage.mode(m) <- "double"
    m
  } else {
    do.call(rbind, lapply(m, function(r) as.numeric(r)))
  }
}

#' total_compute
#'
#' A step of the opnclp_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param samples_seen Coerced to numeric by the body, with \code{as.numeric}.
#' @param model_params Coerced to numeric by the body, with \code{as.numeric}.
#' @return A list with \code{compute}, \code{samples_seen}, \code{params}, \code{gmac_scale}.
#' @export
total_compute <- function(samples_seen, model_params) {
  s <- as.numeric(samples_seen)
  p <- as.numeric(model_params)
  if (s <= 0.0 || p <= 0.0)
    stop("opnclp: both quantities must be positive")
  list(
    compute = s * p,
    samples_seen = s,
    params = p,
    gmac_scale = s * p / 1e9
  )
}

#' fit_power_law
#'
#' A step of the opnclp_native implementation. Called by \code{compare_scaling}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param x Passed to \code{.opnclp_vec}.
#' @param y Passed to \code{.opnclp_vec}.
#' @return A list with \code{alpha}, \code{beta}, \code{slope}, \code{r_squared}, \code{range}, \code{n}.
#' @export
fit_power_law <- function(x, y) {
  X <- .opnclp_vec(x)
  Y <- .opnclp_vec(y)
  if (length(X) != length(Y))
    stop("opnclp: ", length(X), " x values but ", length(Y),
         " y values")
  if (length(X) < 2L)
    stop("opnclp: at least 2 points are needed")
  if (any(X <= 0.0) || any(Y <= 0.0))
    stop("opnclp: a power law is fitted on the logs, so both axes must be strictly positive")
  lx <- log(X)
  ly <- log(Y)
  n <- length(lx)
  mx <- mean(lx)
  my <- mean(ly)
  sxx <- sum((lx - mx)^2)
  if (sxx <= .OPNCLP_EPS)
    stop("opnclp: every x is the same, so no slope is identified")
  sxy <- sum((lx - mx) * (ly - my))
  slope <- sxy / sxx
  inter <- my - slope * mx
  pred <- inter + slope * lx
  ss_res <- sum((ly - pred)^2)
  ss_tot <- sum((ly - my)^2)
  list(
    alpha = -slope,
    beta = exp(inter),
    slope = slope,
    r_squared = if (ss_tot > .OPNCLP_EPS) 1.0 - ss_res / ss_tot
                else 1.0,
    range = c(min(X), max(X)),
    n = n
  )
}

#' .opnclp_predict
#'
#' A step of the opnclp_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param fit A list; the body reads \code{$alpha}, \code{$beta}, \code{$range} from it.
#' @param compute Coerced to numeric by the body, with \code{as.numeric}.
#' @return A list with \code{value}, \code{extrapolation_decades}, \code{interpolated}, \code{note}.
#' @export
.opnclp_predict <- function(fit, compute) {
  c <- as.numeric(compute)
  if (c <= 0.0)
    stop("opnclp: compute must be positive")
  lo <- fit$range[1L]
  hi <- fit$range[2L]
  if (c < lo) {
    decades <- log10(lo / c)
  } else if (c > hi) {
    decades <- log10(c / hi)
  } else {
    decades <- 0.0
  }
  list(
    value = fit$beta * c^(-fit$alpha),
    extrapolation_decades = decades,
    interpolated = decades == 0.0,
    note = "an extrapolation is a different claim from an interpolation, so the distance is reported"
  )
}

#' compare_scaling
#'
#' A step of the opnclp_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param x_a Passed to \code{fit_power_law}.
#' @param y_a Passed to \code{fit_power_law}.
#' @param x_b Passed to \code{fit_power_law}.
#' @param y_b Passed to \code{fit_power_law}.
#' @param label_a Defaults to \code{"A"}.
#' @param label_b Defaults to \code{"B"}.
#' @return The value of \code{out}, as built in the body.
#' @export
compare_scaling <- function(x_a, y_a, x_b, y_b,
                            label_a = "A", label_b = "B") {
  fa <- fit_power_law(x_a, y_a)
  fb <- fit_power_law(x_b, y_b)
  d <- abs(fa$alpha - fb$alpha)
  out <- list(
    estimate = d,
    alpha_gap = d,
    same_law = d < 0.01,
    method = "power-law comparison across training distributions; Cherti et al. (2023)",
    note = "a single exponent implies a universality the paper denies; this is where the distribution shows up"
  )
  out[[label_a]] <- fa
  out[[label_b]] <- fb
  out
}

#' infonce
#'
#' A step of the opnclp_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param image_embeddings Passed to \code{.opnclp_mat}.
#' @param text_embeddings Passed to \code{.opnclp_mat}.
#' @param temperature Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0.07}.
#' @return A list with \code{loss}, \code{image_to_text}, \code{text_to_image}, \code{logits}, \code{note}.
#' @export
infonce <- function(image_embeddings, text_embeddings,
                    temperature = 0.07) {
  I <- .opnclp_mat(image_embeddings)
  Tmat <- .opnclp_mat(text_embeddings)
  n <- nrow(I)
  if (nrow(Tmat) != n)
    stop("opnclp: ", n, " images but ", nrow(Tmat), " texts")
  t <- as.numeric(temperature)
  if (t <= 0.0)
    stop("opnclp: the temperature must be positive")

  nrm <- function(v) {
    m <- sqrt(sum(v * v))
    if (m <= .OPNCLP_EPS)
      stop("opnclp: a zero embedding has no direction")
    v / m
  }

  Iu <- t(apply(I, 1L, nrm))
  Tu <- t(apply(Tmat, 1L, nrm))
  d <- ncol(Iu)
  S <- matrix(0.0, n, n)
  for (i in seq_len(n)) {
    for (j in seq_len(n)) {
      S[i, j] <- sum(Iu[i, ] * Tu[j, ]) / t
    }
  }

  ce <- function(rows) {
    tot <- 0.0
    for (i in seq_len(n)) {
      m <- max(rows[i, ])
      z <- 0.0
      for (k in seq_len(ncol(rows))) z <- z + exp(rows[i, k] - m)
      tot <- tot + -(rows[i, i] - m - log(z))
    }
    tot / n
  }

  li <- ce(S)
  St <- t(S)
  lt <- ce(St)
  list(
    loss = 0.5 * (li + lt),
    image_to_text = li,
    text_to_image = lt,
    logits = S,
    note = "symmetric, so neither modality is the anchor"
  )
}

#' .opnclp_cheatsheet
#'
#' A step of the opnclp_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @return A character value.
#' @export
.opnclp_cheatsheet <- function() {
  paste("opnclp: CLIP-scale laws had been measured on PRIVATE data ",
        "and models; re-run on public LAION with an open ",
        "implementation, up to 2B pairs, and the scaling is a POWER ",
        "LAW across zero-shot classification, retrieval, linear ",
        "probing and fine-tuning. The key finding is that the ",
        "exponent MOVES: OpenAI and OpenCLIP models scale ",
        "differently despite identical architectures and similar ",
        "recipes, so the training DISTRIBUTION is part of the law. ",
        "Fit by least squares on the logs; report how many decades ",
        "beyond the fitted range a prediction reaches.", sep = "")
}

# compact alias per ledger/NAMING.md
openclipscaling <- fit_power_law

# public names resolved by fn/_lazy_map.json
open_clip <- fit_power_law
openclip <- fit_power_law

morie_opnclp <- fit_power_law
