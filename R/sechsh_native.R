# Hash-chained audit log and Merkle inclusion proofs.
# Sources: Laurie, B., Langley, A. & Kasper, E. (2013) "Certificate
# Transparency", RFC 6962, doi:10.17487/RFC6962; Schneier, B. & Kelsey,
# J. (1999) "Secure Audit Logs to Support Computer Forensics", ACM
# TISSEC 2(2), 159-176, doi:10.1145/317087.317089; NIST (2015) Secure
# Hash Standard (SHS), FIPS PUB 180-4.
#
# Native implementation mirroring morie.fn.sechsh exactly: the same
# h_i = H(h_{i-1} || e_i) (or HMAC when a key is given), the same
# RFC 6962 Merkle hashing with 0x00 leaves and 0x01 interior nodes,
# and the same top-down path recording with bottom-up fold in
# verify_inclusion.

.SECH_LEAF <- as.raw(0x00)
.SECH_NODE <- as.raw(0x01)
.SECH_GEN <- raw(32)

#' One step of a hash-chained audit log
#'
#' h_i = H(h_\{i-1\} || e_i), or HMAC if a key is given. With a key the
#' log writer does not hold, an attacker with write access still
#' cannot recompute the chain forward.
#'
#' @param previous_hash Raw bytes.
#' @param entry Raw bytes.
#' @param key Optional raw key.
#' @return List with \code{hash} and \code{keyed} (and \code{note}
#'   when keyed).
#' @references Schneier & Kelsey (1999).
#' @export
morie_sechsh_chain_entry <- function(previous_hash, entry, key = NULL) {
  p <- as.raw(previous_hash)
  e <- as.raw(entry)
  if (is.null(key)) return(list(hash = .sech_sha256(c(p, e)),
                                keyed = FALSE))
  list(hash = .sech_hmac(key, c(p, e)), keyed = TRUE,
       note = "forward rewriting now needs the KEY as well as write access")
}

#' Build a hash-chained audit log
#'
#' @param entries List of raw-byte entries.
#' @param key Optional raw key.
#' @param genesis Raw 32-byte genesis (default all zeros).
#' @return List with \code{hashes}, \code{head}, \code{n},
#'   \code{head_hex}, \code{keyed}.
#' @references Schneier & Kelsey (1999).
#' @export
morie_sechsh_build_chain <- function(entries, key = NULL,
                                     genesis = .SECH_GEN) {
  prev <- as.raw(genesis)
  hh <- list()
  for (e in entries) {
    prev <- morie_sechsh_chain_entry(prev, e, key)$hash
    hh[[length(hh) + 1L]] <- prev
  }
  list(hashes = hh, head = if (length(hh) > 0L) prev else as.raw(genesis),
       n = length(hh),
       head_hex = .sech_hexlify(if (length(hh) > 0L) prev
                                  else as.raw(genesis)),
       keyed = !is.null(key))
}

#' Verify a hash-chained audit log
#'
#' Returns the FIRST bad index; everything before it is still
#' evidence and everything after it is not.
#'
#' @param entries List of entries.
#' @param hashes List of expected hashes.
#' @param key Optional raw key.
#' @param genesis Raw 32-byte genesis.
#' @return List with \code{intact} (==\code{estimate}),
#'   \code{first_bad}, \code{verified_through}, \code{n}, \code{method},
#'   \code{note}.
#' @export
morie_sechsh_verify_chain <- function(entries, hashes, key = NULL,
                                      genesis = .SECH_GEN) {
  if (length(entries) != length(hashes))
    stop("sechsh: ", length(entries), " entries but ", length(hashes),
         " hashes -- an entry or a hash has been dropped")
  prev <- as.raw(genesis)
  first_bad <- NULL
  for (i in seq_along(entries)) {
    want <- morie_sechsh_chain_entry(prev, entries[[i]], key)$hash
    if (!.sech_cteq(want, hashes[[i]]))
      if (is.null(first_bad)) first_bad <- i - 1L
    prev <- as.raw(hashes[[i]])
  }
  list(estimate = is.null(first_bad), intact = is.null(first_bad),
       first_bad = first_bad,
       verified_through = if (is.null(first_bad)) length(entries)
                          else first_bad,
       n = length(entries),
       method = "hash-chained audit log; Schneier & Kelsey (1999)",
       note = paste0("tamper-EVIDENT, not tamper-proof: an attacker ",
                     "who can rewrite the whole tail recomputes every ",
                     "later hash, which is what keying and external ",
                     "anchoring are for"))
}

#' RFC 6962 Merkle tree root
#'
#' Leaves are prefixed 0x00 and interior nodes 0x01, so a node can
#' never be presented as a leaf; the split is at the largest power of
#' two strictly less than the length.
#'
#' @param leaves List of raw-byte leaves.
#' @return Raw 32-byte root.
#' @references RFC 6962 Sec. 2.
#' @export
morie_sechsh_merkle_root <- function(leaves) {
  L <- lapply(leaves, as.raw)
  if (length(L) == 0L) return(.sech_sha256(raw()))
  if (length(L) == 1L) return(.sech_sha256(c(.SECH_LEAF, L[[1]])))
  k <- 1L
  while (k * 2L < length(L)) k <- k * 2L
  c1 <- morie_sechsh_merkle_root(L[seq_len(k)])
  c2 <- morie_sechsh_merkle_root(L[seq.int(k + 1L, length(L))])
  .sech_sha256(c(.SECH_NODE, c1, c2))
}

#' Merkle audit path for one leaf
#'
#' Returns \code{log2(n)} sibling hashes, top-down; the verifier
#' folds them in reverse to recompute the root.
#'
#' @param leaves List of raw-byte leaves.
#' @param index Zero-based leaf index.
#' @return List with \code{path}, \code{path_hex}, \code{length},
#'   \code{index}, \code{size}, \code{note}.
#' @references RFC 6962 Sec. 2.
#' @export
morie_sechsh_inclusion_proof <- function(leaves, index) {
  L <- lapply(leaves, as.raw)
  m <- as.integer(index)
  if (m < 0L || m >= length(L))
    stop("sechsh: index ", m, " is outside a log of ", length(L))
  path <- list()
  lo <- 0L
  hi <- length(L)
  while (hi - lo > 1L) {
    k <- 1L
    while (k * 2L < hi - lo) k <- k * 2L
    if (m - lo < k) {
      path[[length(path) + 1L]] <- morie_sechsh_merkle_root(
        L[seq.int(lo + k + 1L, hi)])
      hi <- lo + k
    } else {
      path[[length(path) + 1L]] <- morie_sechsh_merkle_root(
        L[seq.int(lo + 1L, lo + k)])
      lo <- lo + k
    }
  }
  list(path = path,
       path_hex = lapply(path, .sech_hexlify),
       length = length(path), index = m, size = length(L),
       note = "log2(n) hashes prove membership against a trusted head")
}

#' Verify a Merkle audit path
#'
#' Reconstructs the head from the leaf and the path alone. The path is
#' recorded top-down by \code{inclusion_proof} but must be folded
#' bottom-up; consuming it in the written order combines the wrong
#' sibling at the wrong level.
#'
#' @param leaf Raw leaf.
#' @param index Zero-based leaf index.
#' @param size Total leaf count.
#' @param path List of sibling hashes (top-down).
#' @param root Raw root.
#' @return List with \code{root}, \code{root_hex}, \code{valid},
#'   \code{path_used}.
#' @export
morie_sechsh_verify_inclusion <- function(leaf, index, size, path, root) {
  m <- as.integer(index)
  n <- as.integer(size)
  if (m < 0L || m >= n)
    stop("sechsh: index ", m, " is outside a log of ", n)
  node <- .sech_sha256(c(.SECH_LEAF, as.raw(leaf)))
  lo <- 0L
  hi <- n
  steps <- list()
  used <- 0L
  p <- lapply(path, as.raw)
  while (hi - lo > 1L) {
    if (used >= length(p))
      stop("sechsh: the audit path is too short for a log of ", n)
    k <- 1L
    while (k * 2L < hi - lo) k <- k * 2L
    sib <- p[[used + 1L]]
    used <- used + 1L
    if (m - lo < k) { steps[[length(steps) + 1L]] <- list(sib = sib,
                                                         on_right = TRUE)
                      hi <- lo + k }
    else { steps[[length(steps) + 1L]] <- list(sib = sib,
                                               on_right = FALSE)
           lo <- lo + k }
  }
  for (i in rev(seq_along(steps))) {
    st <- steps[[i]]
    node <- if (st$on_right) .sech_sha256(c(.SECH_NODE, node, st$sib))
            else .sech_sha256(c(.SECH_NODE, st$sib, node))
  }
  list(root = node, root_hex = .sech_hexlify(node),
       valid = .sech_cteq(node, as.raw(root)), path_used = used)
}

# Pure base-R SHA-256 (FIPS 180-4). Slow but exact, no package.
#' Pure base-R SHA-256 (FIPS 180-4). Slow but exact, no package
#'
#' A step of the sechsh_native implementation. Called by \code{.kdf_hmac}, \code{.sech_hmac}, \code{morie_sechsh_chain_entry} and 2 others in the module.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param bytes Passed to \code{as.raw}.
#' @return The value of \code{out}, as built in the body.
#' @export
.sech_sha256 <- function(bytes) {
  # Words live as DOUBLES in [0, 2^32): R integers are signed 32-bit,
  # so bitwAnd/bitwXor on anything >= 2^31 returns NA and the old code
  # packed NaN bit patterns as the digest. XOR/AND run on 16-bit
  # halves; rotation and addition are exact double arithmetic mod 2^32.
  bx <- function(a, b) {
    bitwXor(a %/% 65536, b %/% 65536) * 65536 +
      bitwXor(a %% 65536, b %% 65536)
  }
  ba <- function(a, b) {
    bitwAnd(a %/% 65536, b %/% 65536) * 65536 +
      bitwAnd(a %% 65536, b %% 65536)
  }
  bn <- function(a) 4294967295 - a
  rotr <- function(x, n) (x %/% 2^n) + (x %% 2^n) * 2^(32 - n)
  shr <- function(x, n) x %/% 2^n
  bs <- as.integer(as.raw(bytes))
  blen <- length(bs) * 8
  bs <- c(bs, 0x80L)
  while (length(bs) %% 64L != 56L) bs <- c(bs, 0x00L)
  bs <- c(bs, (blen %/% 2^c(56, 48, 40, 32, 24, 16, 8, 0)) %% 256)
  H <- c(0x6a09e667, 0xbb67ae85, 0x3c6ef372, 0xa54ff53a,
         0x510e527f, 0x9b05688c, 0x1f83d9ab, 0x5be0cd19)
  K <- c(0x428a2f98, 0x71374491, 0xb5c0fbcf, 0xe9b5dba5, 0x3956c25b,
         0x59f111f1, 0x923f82a4, 0xab1c5ed5, 0xd807aa98, 0x12835b01,
         0x243185be, 0x550c7dc3, 0x72be5d74, 0x80deb1fe, 0x9bdc06a7,
         0xc19bf174, 0xe49b69c1, 0xefbe4786, 0x0fc19dc6, 0x240ca1cc,
         0x2de92c6f, 0x4a7484aa, 0x5cb0a9dc, 0x76f988da, 0x983e5152,
         0xa831c66d, 0xb00327c8, 0xbf597fc7, 0xc6e00bf3, 0xd5a79147,
         0x06ca6351, 0x14292967, 0x27b70a85, 0x2e1b2138, 0x4d2c6dfc,
         0x53380d13, 0x650a7354, 0x766a0abb, 0x81c2c92e, 0x92722c85,
         0xa2bfe8a1, 0xa81a664b, 0xc24b8b70, 0xc76c51a3, 0xd192e819,
         0xd6990624, 0xf40e3585, 0x106aa070, 0x19a4c116, 0x1e376c08,
         0x2748774c, 0x34b0bcb5, 0x391c0cb3, 0x4ed8aa4a, 0x5b9cca4f,
         0x682e6ff3, 0x748f82ee, 0x78a5636f, 0x84c87814, 0x8cc70208,
         0x90befffa, 0xa4506ceb, 0xbef9a3f7, 0xc67178f2)
  for (block in seq(1L, length(bs), by = 64L)) {
    W <- numeric(64L)
    for (i in 0:15) {
      W[i + 1L] <- sum(bs[block + 4L * i + 0:3] * c(2^24, 2^16, 2^8, 1))
    }
    for (i in 16:63) {
      w15 <- W[i - 15L + 1L]
      w2 <- W[i - 2L + 1L]
      s0 <- bx(bx(rotr(w15, 7), rotr(w15, 18)), shr(w15, 3))
      s1 <- bx(bx(rotr(w2, 17), rotr(w2, 19)), shr(w2, 10))
      W[i + 1L] <- (W[i - 16L + 1L] + s0 + W[i - 7L + 1L] + s1) %% 2^32
    }
    a <- H[1]
    b <- H[2]
    cc <- H[3]
    d <- H[4]
    e <- H[5]
    f <- H[6]
    g <- H[7]
    hh <- H[8]
    for (i in 0:63) {
      S1 <- bx(bx(rotr(e, 6), rotr(e, 11)), rotr(e, 25))
      ch <- bx(ba(e, f), ba(bn(e), g))
      T1 <- (hh + S1 + ch + K[i + 1L] + W[i + 1L]) %% 2^32
      S0 <- bx(bx(rotr(a, 2), rotr(a, 13)), rotr(a, 22))
      mj <- bx(bx(ba(a, b), ba(a, cc)), ba(b, cc))
      T2 <- (S0 + mj) %% 2^32
      hh <- g
      g <- f
      f <- e
      e <- (d + T1) %% 2^32
      d <- cc
      cc <- b
      b <- a
      a <- (T1 + T2) %% 2^32
    }
    H[1] <- (H[1] + a) %% 2^32
    H[2] <- (H[2] + b) %% 2^32
    H[3] <- (H[3] + cc) %% 2^32
    H[4] <- (H[4] + d) %% 2^32
    H[5] <- (H[5] + e) %% 2^32
    H[6] <- (H[6] + f) %% 2^32
    H[7] <- (H[7] + g) %% 2^32
    H[8] <- (H[8] + hh) %% 2^32
  }
  as.raw(as.vector(vapply(H, function(w)
    (w %/% 2^c(24, 16, 8, 0)) %% 256, numeric(4))))
}

#' .sech_hmac
#'
#' A step of the sechsh_native implementation. Called by \code{morie_sechsh_chain_entry}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param key A vector; its length is taken.
#' @param msg Passed to \code{as.raw}.
#' @return The value of \code{.sech_sha256}.
#' @export
.sech_hmac <- function(key, msg) {
  key <- as.raw(key)
  if (length(key) > 64L) key <- .sech_sha256(key)
  if (length(key) < 64L) key <- c(key, rep(as.raw(0x00), 64L - length(key)))
  # bitwXor() rejects raw vectors; XOR the byte VALUES and re-pack
  opad <- as.raw(bitwXor(as.integer(key), 0x5cL))
  ipad <- as.raw(bitwXor(as.integer(key), 0x36L))
  .sech_sha256(c(opad, .sech_sha256(c(ipad, as.raw(msg)))))
}

#' .sech_hexlify
#'
#' A step of the sechsh_native implementation. Called by \code{morie_sechsh_build_chain}, \code{morie_sechsh_verify_inclusion}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param bs Coerced to integer by the body, with \code{as.integer}.
#' @return A character value.
#' @export
.sech_hexlify <- function(bs) {
  paste(format(as.hexmode(as.integer(bs)), width = 2L), collapse = "")
}

#' .sech_cteq
#'
#' A step of the sechsh_native implementation. Called by \code{morie_sechsh_verify_chain}, \code{morie_sechsh_verify_inclusion}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param a A vector; its length is taken and its elements indexed.
#' @param b A vector; its length is taken and its elements indexed.
#' @return A logical value.
#' @export
.sech_cteq <- function(a, b) {
  a <- as.raw(a)
  b <- as.raw(b)
  if (length(a) != length(b)) return(FALSE)
  r <- as.integer(0)
  for (i in seq_along(a)) {
    r <- bitwXor(r, bitwXor(as.integer(a[i]), as.integer(b[i])))
  }
  r == 0L
}

# house entry point: the package exports one morie_<module>
morie_sechsh <- morie_sechsh_chain_entry
