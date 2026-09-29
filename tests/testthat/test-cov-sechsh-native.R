# Hash chains (Schneier & Kelsey 1999) and RFC 6962 Merkle trees, with
# every digest recomputed by openssl's SHA-256 / HMAC.

sh_h <- function(x) as.raw(openssl::sha256(x))
sh_hmac <- function(k, x) as.raw(openssl::sha256(x, key = k))
sh_root <- function(L) {
  if (length(L) == 0L) return(sh_h(raw(0)))
  if (length(L) == 1L) return(sh_h(c(as.raw(0), L[[1]])))
  k <- 2^floor(log2(length(L) - 1))
  sh_h(c(as.raw(1), sh_root(L[seq_len(k)]), sh_root(L[-seq_len(k)])))
}
sh_entries <- lapply(c("alpha", "beta", "gamma", "delta", "eps"), charToRaw)

test_that("chain entries hash previous || entry, or HMAC it with a key", {
  skip_if_not_installed("openssl")
  prev <- as.raw(0:31)
  e <- charToRaw("login alice")
  expect_identical(morie_sechsh_chain_entry(prev, e)$hash, sh_h(c(prev, e)))
  k <- charToRaw("secret-key")
  ke <- morie_sechsh_chain_entry(prev, e, key = k)
  expect_identical(ke$hash, sh_hmac(k, c(prev, e)))
  expect_true(ke$keyed)
  # character inputs are taken as their bytes in the restored API
  expect_identical(chain_entry(prev, "login alice")$hash, sh_h(c(prev, e)))
  expect_identical(chain_entry(prev, "login alice", key = k)$hash,
                   sh_hmac(k, c(prev, e)))
  long <- as.raw(1:100)
  expect_identical(morie_sechsh_chain_entry(prev, e, key = long)$hash,
                   sh_hmac(long, c(prev, e)))
})

test_that("build and verify chains, and locate the first tampered entry", {
  skip_if_not_installed("openssl")
  prev <- raw(32)
  ref <- list()
  for (e in sh_entries) {
    prev <- sh_h(c(prev, e))
    ref[[length(ref) + 1]] <- prev
  }
  for (bc in list(morie_sechsh_build_chain, build_chain)) {
    ch <- bc(sh_entries)
    expect_identical(ch$hashes, ref)
    expect_identical(ch$head, ref[[5]])
    expect_identical(tolower(ch$head_hex), paste(as.character(ref[[5]]), collapse = ""))
    expect_identical(bc(list())$head, raw(32))
  }
  for (vc in list(morie_sechsh_verify_chain, verify_chain)) {
    v <- vc(sh_entries, ref)
    expect_true(v$intact)
    expect_identical(v$verified_through, 5L)
    bad <- sh_entries
    bad[[3]] <- charToRaw("GAMMA")
    v2 <- vc(bad, ref)
    expect_false(v2$intact)
    expect_identical(v2$first_bad, 2L)
    expect_error(vc(sh_entries[-1], ref), "dropped")
  }
  k <- charToRaw("k")
  kc <- morie_sechsh_build_chain(sh_entries, key = k)
  expect_identical(kc$hashes[[1]], sh_hmac(k, c(raw(32), sh_entries[[1]])))
  expect_true(morie_sechsh_verify_chain(sh_entries, kc$hashes, key = k)$intact)
  expect_false(morie_sechsh_verify_chain(sh_entries, kc$hashes)$intact)
})

test_that("Merkle roots follow RFC 6962 section 2.1", {
  skip_if_not_installed("openssl")
  for (n in 0:7) {
    L <- sh_entries[seq_len(min(n, 5))]
    if (n > 5) L <- c(L, lapply(seq_len(n - 5), function(i) as.raw(i)))
    expect_identical(morie_sechsh_merkle_root(L), sh_root(L))
    expect_identical(merkle_root(L), sh_root(L))
  }
})

test_that("inclusion proofs verify for every leaf and reject tampering", {
  skip_if_not_installed("openssl")
  L <- c(sh_entries, list(as.raw(9), as.raw(10)))
  root <- sh_root(L)
  for (i in seq_along(L) - 1L) {
    pr <- morie_sechsh_inclusion_proof(L, i)
    expect_lte(pr$length, ceiling(log2(length(L))))
    v <- morie_sechsh_verify_inclusion(L[[i + 1]], i, length(L), pr$path, root)
    expect_true(v$valid)
    expect_identical(v$root, root)
    expect_identical(v$path_used, pr$length)
    pr2 <- inclusion_proof(L, i)
    expect_identical(pr2$path, pr$path)
    expect_true(verify_inclusion(L[[i + 1]], i, length(L), pr2$path, root)$valid)
  }
  # the sibling of leaf 0 in a 7-leaf tree is the root of the right subtree
  pr <- morie_sechsh_inclusion_proof(L, 0)
  expect_identical(pr$path[[1]], sh_root(L[5:7]))
  expect_false(morie_sechsh_verify_inclusion(charToRaw("x"), 0, 7, pr$path, root)$valid)
  expect_false(verify_inclusion(charToRaw("x"), 0, 7, pr$path, root)$valid)
  expect_error(morie_sechsh_inclusion_proof(L, 7), "outside")
  expect_error(inclusion_proof(L, -1), "outside")
  expect_error(morie_sechsh_verify_inclusion(L[[1]], 0, 7, pr$path[-1], root), "too short")
  expect_error(verify_inclusion(L[[1]], 9, 7, pr$path, root), "outside")
})
