# morie.fn -- function file (rootcoder007/morie) -- R arm
# DiT: Scalable Diffusion Models with Transformers.
# Peebles, W. & Xie, S. (2023) ICCV 2023, 4195-4205,
# doi:10.1109/ICCV51070.2023.00387, arXiv:2212.09748.
# The U-Net diffusion backbone replaced with a transformer on latent
# patches, scalability measured in Gflops (NOT parameters, deliberately
# so the token axis is visible), adaLN-zero conditioning.

#' Patchify a latent and count tokens.
#'
#' Tokens: T = (I/p)^2. Halving p quadruples T at essentially
#' constant parameter count -- the axis a parameter count cannot see.
#'
#' @param latent_size integer, side length I of the latent.
#' @param patch      integer, patch side p; p must divide I.
#' @return list with tokens, grid, patch, latent_size, note.
#' @export
patch_grid <- function(latent_size, patch) {
  I <- as.integer(latent_size); p <- as.integer(patch)
  if (p < 1L || I < 1L)
    stop("dits16: the latent size and patch must be positive")
  if ((I %% p) != 0L)
    stop(sprintf("dits16: the patch %d does not divide the latent size %d",
                 p, I))
  t <- (I %/% p)^2L
  list(tokens = t, grid = I %/% p, patch = p, latent_size = I,
       note = paste("the token count scales as 1/p^2, at constant",
                    "parameters"))
}

#' Forward-pass Gflops for a DiT stack.
#'
#' attention: 4*T*d^2 + 2*T^2*d. mlp: 2*mlp_ratio*T*d^2. Per block
#' doubled (multiply-adds), scaled by depth L, in Gflops.
#'
#' @param tokens    integer T, number of tokens.
#' @param depth     integer L, number of DiT blocks.
#' @param width     integer d, model width.
#' @param mlp_ratio numeric, MLP hidden expansion (default 4.0).
#' @return list with gflops, tokens, depth, width, attention_share, note.
#' @export
gflops <- function(tokens, depth, width, mlp_ratio = 4.0) {
  T <- as.integer(tokens); L <- as.integer(depth); d <- as.integer(width)
  if (min(T, L, d) < 1L)
    stop("dits16: tokens, depth and width must be positive")
  attn <- 4.0 * T * d * d + 2.0 * T * T * d
  mlp  <- 2.0 * as.numeric(mlp_ratio) * T * d * d
  list(gflops = L * (attn + mlp) * 2.0 / 1e9,
       tokens = T, depth = L, width = d,
       attention_share = attn / (attn + mlp),
       note = paste("measured in Gflops, not parameters, so the",
                    "token axis is visible"))
}

# Flatten to a numeric vector (mirrors s03core.vec).
.dits16_vec <- function(x) {
  as.numeric(x)
}

#' Adaptive LayerNorm with a ZERO-initialised residual gate.
#'
#' Regresses gamma, beta and alpha from the conditioning vector.
#' With W_alpha = 0 every block is the identity at initialisation.
#'
#' @param cond    numeric, conditioning vector.
#' @param hidden  numeric, hidden activations.
#' @param W_scale d x |cond| matrix for gamma.
#' @param W_shift d x |cond| matrix for beta.
#' @param W_alpha d x |cond| matrix for alpha (initialised to zero).
#' @param eps     numeric, variance epsilon.
#' @return list with modulated, gate, identity_at_init, note.
#' @export
adaln_zero <- function(cond, hidden, W_scale, W_shift, W_alpha,
                       eps = 1e-6) {
  c <- .dits16_vec(cond)
  h <- .dits16_vec(hidden)
  d <- length(h)

  .dits16_reg <- function(W) {
    W <- as.matrix(W)
    if (nrow(W) != d)
      stop(sprintf(paste("dits16: the conditioning projection is",
                         "mis-sized (%d rows for %d channels)"),
                   nrow(W), d))
    as.numeric(W %*% c)
  }

  m <- sum(h) / d
  s <- sqrt(sum((h - m)^2) / d + as.numeric(eps))
  norm <- (h - m) / s
  g <- .dits16_reg(W_scale)
  b <- .dits16_reg(W_shift)
  a <- .dits16_reg(W_alpha)
  mod <- norm * (1.0 + g) + b
  list(modulated = mod, gate = a,
       identity_at_init = all(abs(a) < 1e-12),
       note = paste("alpha initialised to ZERO makes the block the",
                    "identity, leaving a clean residual path"))
}

#' One DiT block: adaLN-zero, attention, adaLN-zero, MLP.
#'
#' Each sub-layer is gated by a learned alpha (initialised to zero)
#' so the whole block starts as the identity.
#'
#' @param hidden    numeric hidden vector.
#' @param cond      numeric conditioning vector.
#' @param attn_fn   function(mod) -> numeric, attention over modulated.
#' @param mlp_fn    function(mod) -> numeric, MLP over modulated.
#' @param W_scale   d x |cond| matrix, first adaLN gamma.
#' @param W_shift   d x |cond| matrix, first adaLN beta.
#' @param W_alpha   d x |cond| matrix, first adaLN alpha (zero init).
#' @param W_scale2  d x |cond| matrix, second adaLN gamma.
#' @param W_shift2  d x |cond| matrix, second adaLN beta.
#' @param W_alpha2  d x |cond| matrix, second adaLN alpha (zero init).
#' @return list with output, identity_at_init.
#' @export
dit_block <- function(hidden, cond, attn_fn, mlp_fn,
                      W_scale, W_shift, W_alpha,
                      W_scale2, W_shift2, W_alpha2) {
  h <- .dits16_vec(hidden)
  a1 <- adaln_zero(cond, h, W_scale, W_shift, W_alpha)
  att <- .dits16_vec(attn_fn(a1$modulated))
  h <- h + a1$gate * att
  a2 <- adaln_zero(cond, h, W_scale2, W_shift2, W_alpha2)
  ml <- .dits16_vec(mlp_fn(a2$modulated))
  h <- h + a2$gate * ml
  list(output = h,
       identity_at_init = isTRUE(a1$identity_at_init) &&
                          isTRUE(a2$identity_at_init))
}

#' Rank configurations by Gflops.
#'
#' Each entry is c(name, latent, patch, depth, width). Three axes
#' (depth, width, token count) all move Gflops; only the first two
#' move parameters.
#'
#' @param configs list of 5-tuples (name, latent, patch, depth, width).
#' @return list with ranked entries and a note.
#' @export
scaling_comparison <- function(configs) {
  out <- list()
  for (cfg in configs) {
    name <- as.character(cfg[[1]])
    I    <- as.numeric(cfg[[2]])
    p    <- as.numeric(cfg[[3]])
    Ln   <- as.numeric(cfg[[4]])
    d    <- as.numeric(cfg[[5]])
    t <- patch_grid(I, p)$tokens
    g <- gflops(t, Ln, d)
    params <- Ln * (4 * d * d + 8 * d * d)
    out[[length(out) + 1L]] <- list(name = name, tokens = t,
                                    gflops = g$gflops,
                                    parameters = params)
  }
  gv <- vapply(out, function(r) r$gflops, numeric(1))
  out <- out[order(gv)]
  list(ranked = out,
       note = paste("depth, width and TOKEN COUNT all move Gflops;",
                    "only the first two move parameters"))
}

#' @rdname patch_grid
#' @export
morie_dits16 <- patch_grid

#' @rdname patch_grid
#' @export
morie_dits16 <- patch_grid

#' @rdname patch_grid
#' @export
morie_dits16 <- patch_grid

#' @rdname patch_grid
#' @export
morie_dits16 <- patch_grid

#' @rdname patch_grid
#' @export
morie_dits16 <- patch_grid

#' @rdname patch_grid
#' @export
morie_dits16 <- patch_grid

#' @rdname patch_grid
#' @export
morie_dits16 <- patch_grid

#' @rdname patch_grid
#' @export
morie_dits16 <- patch_grid

#' @rdname patch_grid
#' @export
morie_dits16 <- patch_grid

#' @rdname patch_grid
#' @export
morie_dits16 <- patch_grid
