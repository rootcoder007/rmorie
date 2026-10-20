# SPDX-License-Identifier: AGPL-3.0-or-later

capsule_dir <- function(env = parent.frame(), rows = 3L) {
  dir <- withr::local_tempdir(.local_envir = env)
  utils::write.csv(data.frame(id = seq_len(rows), value = seq_len(rows) / 2),
                   file.path(dir, "data.csv"), row.names = FALSE)
  dir
}

test_that("morie_capsule_write pins the CSV and verifies with rmoriebricklayer::vrfcps()", {
  dir <- capsule_dir()
  prov <- morie_capsule_write(dir, "data.csv", url = "https://example.org/data.csv",
                              name = "demo", retrieved = "2026-10-20")
  path <- file.path(dir, "data.csv")
  expect_identical(attr(prov, "path"), file.path(dir, "data_provenance.json"))
  back <- rmoriebricklayer::loadprov(attr(prov, "path"))
  expect_identical(back$name, "demo")
  expect_identical(back$resource$filename, "data.csv")
  expect_identical(back$resource$sha256, rmoriebricklayer::sha256file(path))
  expect_equal(back$resource$size_bytes, file.size(path))
  expect_equal(back$resource$row_count_data_rows, 3)
  expect_identical(back$source, list(url = "https://example.org/data.csv", retrieved = "2026-10-20"))
  chk <- rmoriebricklayer::vrfcps(dir)
  expect_true(chk$ok)
  expect_setequal(chk$checks$check, c("provenance_readable", "data_present", "data_sha256",
                                      "data_size_bytes", "data_readable", "data_row_count",
                                      "data_not_synthetic"))
  expect_identical(morie_capsule_verify(dir)$checks, chk$checks)
})

test_that("morie_capsule_write leaves source$url out without a url, and overwrites", {
  dir <- capsule_dir()
  morie_capsule_write(dir, "data.csv", url = "https://example.org/a.csv")
  prov <- morie_capsule_write(dir, "data.csv", provenance_file = "p.json")
  expect_named(prov$source, "retrieved")
  expect_identical(prov$source$retrieved, format(Sys.Date()))
  expect_identical(prov$name, basename(dir))
  expect_true(morie_capsule_verify(dir, provenance_file = "p.json")$ok)
  utils::write.csv(data.frame(id = 1:5), file.path(dir, "data.csv"), row.names = FALSE)
  morie_capsule_write(dir, "data.csv", provenance_file = "p.json")
  expect_equal(rmoriebricklayer::loadprov(file.path(dir, "p.json"))$resource$row_count_data_rows, 5)
})

test_that("morie_capsule_write refuses bad arguments", {
  dir <- capsule_dir()
  expect_error(morie_capsule_write(file.path(dir, "nope"), "data.csv"), "`dir` is not an existing directory")
  expect_error(morie_capsule_write(c(dir, dir), "data.csv"), "`dir` must be a single non-empty string")
  expect_error(morie_capsule_write(dir, "../data.csv"), "`data_file` must be a path relative to `dir`")
  expect_error(morie_capsule_write(dir, "/etc/hosts"), "`data_file` must be a path relative")
  expect_error(morie_capsule_write(dir, "C:/x.csv"), "`data_file` must be a path relative")
  expect_error(morie_capsule_write(dir, "a/../../b.csv"), "`data_file` must be a path relative")
  expect_error(morie_capsule_write(dir, NA_character_), "`data_file` must be a single non-empty string")
  expect_error(morie_capsule_write(dir, "data.csv", provenance_file = "../p.json"),
               "`provenance_file` must be a path relative")
  expect_error(morie_capsule_write(dir, "data.csv", url = 1), "`url` must be a single non-empty string")
  expect_error(morie_capsule_write(dir, "data.csv", name = ""), "`name` must be a single non-empty string")
  expect_error(morie_capsule_write(dir, "data.csv", retrieved = NULL), "`retrieved` must be")
  writeLines("x", file.path(dir, "notes.txt"))
  expect_error(morie_capsule_write(dir, "notes.txt"), "`data_file` must be a .csv file")
  expect_error(morie_capsule_write(dir, "missing.csv"), "`data_file` is not a file in `dir`")
  dir.create(file.path(dir, "sub.csv"))
  expect_error(morie_capsule_write(dir, "sub.csv"), "`data_file` is not a file in `dir`")
  file.create(file.path(dir, "empty.csv"))
  expect_error(morie_capsule_write(dir, "empty.csv"), "`data_file` could not be read as a CSV")
  expect_false(file.exists(file.path(dir, "data_provenance.json")))
})

test_that("a capsule written by rmoriebricklayer verifies with morie_capsule_verify", {
  dir <- capsule_dir()
  prov <- list(resource = list(filename = "data.csv",
                               sha256 = rmoriebricklayer::sha256file(file.path(dir, "data.csv")),
                               row_count_data_rows = 3),
               schema = list(expected_columns = c("id", "value")))
  writeLines(rmoriebricklayer::tojs(prov, auto_unbox = TRUE), file.path(dir, "data_provenance.json"))
  chk <- morie_capsule_verify(dir)
  expect_s3_class(chk, "bricklayer_capsule_check")
  expect_true(chk$ok)
  expect_true("schema_valid" %in% chk$checks$check)
  utils::write.csv(data.frame(id = 1:4, value = 1:4), file.path(dir, "data.csv"), row.names = FALSE)
  bad <- morie_capsule_verify(dir)
  expect_false(bad$ok)
  expect_setequal(bad$checks$check[!bad$checks$ok], c("data_sha256", "data_row_count"))
})

test_that("morie_capsule_verify passes data, manifest and script files through", {
  dir <- capsule_dir()
  morie_capsule_write(dir, "data.csv")
  file.copy(file.path(dir, "data.csv"), file.path(dir, "copy.csv"))
  writeLines("1 + 1", file.path(dir, "analysis.R"))
  suppressMessages(morie_capsule_manifest(list(script_sha256 = rmoriebricklayer::sha256file(file.path(dir, "analysis.R"))),
                                          data.frame(name = "n", observed = 3, expected = 3),
                                          path = file.path(dir, "results.json"), env = FALSE))
  chk <- morie_capsule_verify(dir, data_file = "copy.csv", manifest_file = "results.json",
                              script_file = "analysis.R")
  expect_true(chk$ok)
  expect_true(all(c("manifest_consistent", "script_sha256") %in% chk$checks$check))
  expect_match(chk$checks$detail[chk$checks$check == "data_present"], "copy.csv")
})

test_that("morie_capsule_verify refuses bad arguments", {
  dir <- capsule_dir()
  expect_error(morie_capsule_verify(file.path(dir, "nope")), "`dir` is not an existing directory")
  expect_error(morie_capsule_verify(1), "`dir` must be a single non-empty string")
  expect_error(morie_capsule_verify(dir, provenance_file = NA_character_), "`provenance_file` must be")
  expect_error(morie_capsule_verify(dir, data_file = 1), "`data_file` must be")
  expect_error(morie_capsule_verify(dir, manifest_file = c("a", "b")), "`manifest_file` must be")
  expect_error(morie_capsule_verify(dir, script_file = ""), "`script_file` must be")
  # a missing record is a failed row, not an error
  expect_false(morie_capsule_verify(dir)$ok)
})

test_that("morie_capsule_sign signs with every scheme rmoriebricklayer offers", {
  man <- morie_capsule_manifest(list(project = "demo"), env = FALSE)
  for (scheme in c("ML-DSA-44", "SLH-DSA-SHA2-128f")) {
    key <- rmoriebricklayer::fipskg(scheme)
    sig <- morie_capsule_sign(man, key, context = "release")
    expect_identical(sig$scheme, scheme)
    expect_true(rmoriebricklayer::cpsvrf(rmoriebricklayer::mnfdgst(man), sig, key, ctx = "release"))
    expect_false(rmoriebricklayer::cpsvrf(rmoriebricklayer::mnfdgst(man), sig, key, ctx = "other"))
  }
  key <- rmoriebricklayer::fipskg("ML-DSA-44")
  a <- morie_capsule_sign("m", key, deterministic = TRUE)
  expect_identical(a$signature, morie_capsule_sign("m", key, deterministic = TRUE)$signature)
  r <- morie_capsule_sign(charToRaw("m"), key, prehash = "sha256")
  expect_true(rmoriebricklayer::cpsvrf(charToRaw("m"), r, key))
  tag <- morie_capsule_sign("sha256:abc", "shared-secret", scheme = "hmac")
  expect_identical(tag$scheme, "hmac")
  expect_true(rmoriebricklayer::cpsvrf("sha256:abc", tag, "shared-secret"))
  withr::local_options(rmoriebricklayer.xmssidxfile = withr::local_tempfile())
  xk <- rmoriebricklayer::pqckg(height = 2)
  xs <- morie_capsule_sign("m", xk, scheme = "xmss")
  expect_identical(xs$scheme, "xmss-sha256")
  expect_true(rmoriebricklayer::cpsvrf("m", xs, rmoriebricklayer::signpubkey(xk)))
})

test_that("morie_capsule_sign refuses bad arguments", {
  key <- rmoriebricklayer::fipskg("ML-DSA-44")
  msg <- "`message` must be one string, a raw vector or a manifest"
  expect_error(morie_capsule_sign(1, key), msg)
  expect_error(morie_capsule_sign(NA_character_, key), msg)
  expect_error(morie_capsule_sign(c("a", "b"), key), msg)
  expect_error(morie_capsule_sign(data.frame(x = 1), key), msg)
  expect_error(morie_capsule_sign("m", key, scheme = 1), "`scheme` must be a single non-empty string")
  expect_error(morie_capsule_sign("m", key, deterministic = NA), "`deterministic` must be TRUE or FALSE")
  expect_error(morie_capsule_sign("m", "secret", scheme = "xmss"), "needs a key from pqckg")
})

test_that("morie_capsule_bundle signs the capsule and rmoriebricklayer::cpsbndlvrf() checks it", {
  dir <- capsule_dir()
  morie_capsule_write(dir, "data.csv")
  man <- suppressMessages(morie_capsule_manifest(list(project = "demo"),
                                                 data.frame(name = "n", observed = 3, expected = 3)))
  key <- rmoriebricklayer::fipskg("ML-DSA-44")
  b <- morie_capsule_bundle(dir, man, key, note = "as published", context = "capsule")
  expect_s3_class(b, "bricklayer_bundle")
  expect_identical(attr(b, "path"), file.path(dir, "capsule_bundle.json"))
  expect_setequal(vapply(b$files, `[[`, "", "path"), c("data.csv", "data_provenance.json"))
  res <- rmoriebricklayer::cpsbndlvrf(attr(b, "path"), dir, man = man,
                                      keyexp = rmoriebricklayer::fipspubkey(key)$public)
  expect_true(res$ok)
  # the manifest digest still matches once written to disk and read back
  mpath <- withr::local_tempfile(fileext = ".json")
  rmoriebricklayer::wrmnfjs(man, mpath)
  expect_true(rmoriebricklayer::cpsbndlvrf(attr(b, "path"), dir, man = rmoriebricklayer::loadprov(mpath))$ok)
  out <- withr::local_tempfile(fileext = ".json")
  b2 <- morie_capsule_bundle(dir, man, key, files = "data.csv", path = out)
  expect_identical(attr(b2, "path"), out)
  expect_identical(vapply(b2$files, `[[`, "", "path"), "data.csv")
  utils::write.csv(data.frame(id = 9), file.path(dir, "data.csv"), row.names = FALSE)
  expect_false(rmoriebricklayer::cpsbndlvrf(attr(b, "path"), dir)$ok)
})

test_that("morie_capsule_bundle refuses bad arguments", {
  dir <- capsule_dir()
  man <- morie_capsule_manifest(list(), env = FALSE)
  key <- rmoriebricklayer::fipskg("ML-DSA-44")
  expect_error(morie_capsule_bundle(file.path(dir, "nope"), man, key), "`dir` is not an existing directory")
  expect_error(morie_capsule_bundle(dir, "man", key), "`manifest` must be a manifest")
  expect_error(morie_capsule_bundle(dir, data.frame(x = 1), key), "`manifest` must be a manifest")
  expect_error(morie_capsule_bundle(dir, man, key, files = character()), "`files` must be file names")
  expect_error(morie_capsule_bundle(dir, man, key, files = NA_character_), "`files` must be file names")
  expect_error(morie_capsule_bundle(dir, man, key, files = 1), "`files` must be file names")
  expect_error(morie_capsule_bundle(dir, man, key, path = c("a", "b")), "`path` must be a single")
  expect_error(morie_capsule_bundle(dir, man, key, files = "missing.csv"), "these files are not in `dir`")
})

test_that("morie_capsule_drift compares the pinned CSV with a fresh one", {
  dir <- withr::local_tempdir()
  set.seed(1)
  ref <- data.frame(x = stats::rnorm(300), g = sample(c("a", "b"), 300, TRUE))
  utils::write.csv(ref, file.path(dir, "data.csv"), row.names = FALSE)
  morie_capsule_write(dir, "data.csv")
  same <- data.frame(x = stats::rnorm(300), g = sample(c("a", "b"), 300, TRUE))
  d <- morie_capsule_drift(dir, same)
  expect_s3_class(d, "bricklayer_drift")
  expect_false(d$anydrft)
  expect_identical(d, rmoriebricklayer::cpsdrft(utils::read.csv(file.path(dir, "data.csv")), same))
  moved <- same
  moved$x <- moved$x + 2
  moved$extra <- 1
  cur <- withr::local_tempfile(fileext = ".csv")
  utils::write.csv(moved, cur, row.names = FALSE)
  d2 <- morie_capsule_drift(dir, cur, alpha = 0.05, psi_threshold = 0.1, psi_min_n = 10L, bins = 5L,
                            id_levels = 50L, seed = 2L)
  expect_true(d2$anydrft)
  expect_identical(d2$added, "extra")
  expect_true(d2$columns$drifted[d2$columns$column == "x"])
})

test_that("morie_capsule_drift refuses bad arguments", {
  dir <- capsule_dir()
  expect_error(morie_capsule_drift(file.path(dir, "nope"), data.frame()), "`dir` is not an existing directory")
  expect_error(morie_capsule_drift(dir, data.frame(id = 1)), "no provenance record in `dir`")
  expect_error(morie_capsule_drift(dir, data.frame(id = 1), provenance_file = "../p.json"),
               "`provenance_file` must be a path relative")
  writeLines('{"resource": {}}', file.path(dir, "data_provenance.json"))
  expect_error(morie_capsule_drift(dir, data.frame(id = 1)), "records no resource\\$filename")
  writeLines('{"resource": {"filename": "../data.csv"}}', file.path(dir, "data_provenance.json"))
  expect_error(morie_capsule_drift(dir, data.frame(id = 1)), "`resource\\$filename` must be a path relative")
  writeLines('{"resource": {"filename": "gone.csv"}}', file.path(dir, "data_provenance.json"))
  expect_error(suppressWarnings(morie_capsule_drift(dir, data.frame(id = 1))),
               "`resource\\$filename` could not be read as a CSV")
  morie_capsule_write(dir, "data.csv")
  expect_error(morie_capsule_drift(dir, 1), "`current` must be a single non-empty string")
  expect_error(suppressWarnings(morie_capsule_drift(dir, file.path(dir, "gone.csv"))),
               "`current` could not be read as a CSV")
  expect_error(morie_capsule_drift(dir, data.frame(id = 1), alpha = 2), "`alpha` must be a single value")
})

test_that("morie_capsule_manifest records the checks and writes the manifest", {
  checks <- data.frame(name = c("n", "mean", "label"), observed = c(10, 5.6, NA),
                       expected = c(10, 5.5, NA), tol = c(0, 0.01, 0), group = c("a", "b", "c"))
  man <- suppressMessages(morie_capsule_manifest(list(project = "demo"), checks, env = FALSE))
  expect_identical(vapply(man$results, `[[`, "", "status"), c(n = "PASS", mean = "DIFFER", label = "INFO"))
  expect_identical(man$results$mean$group, "b")
  expect_identical(man$results$mean$tol, 0.01)
  expect_null(man$environment)
  plain <- suppressMessages(morie_capsule_manifest(checks = checks[1:2, c("name", "observed", "expected")],
                                                   env = FALSE))
  expect_identical(plain$results$mean$tol, 1e-4)
  expect_identical(plain$results$mean$group, "general")
  expect_identical(morie_capsule_manifest(env = FALSE)$results, list())
  for (canonical in c(FALSE, TRUE)) {
    path <- withr::local_tempfile(fileext = ".json")
    full <- suppressMessages(morie_capsule_manifest(list(project = "demo"), checks, path = path,
                                                    canonical = canonical))
    expect_identical(attr(full, "path"), path)
    expect_type(full$environment$packages, "list")
    # the digest survives the write and the read (rmoriebricklayer 1.0.0 named-vector packages did not)
    expect_identical(rmoriebricklayer::mnfdgst(rmoriebricklayer::loadprov(path)),
                     rmoriebricklayer::mnfdgst(full))
  }
  expect_length(readLines(path), 1L)
})

test_that("morie_capsule_manifest refuses bad arguments", {
  expect_error(morie_capsule_manifest("demo"), "`meta` must be a list")
  expect_error(morie_capsule_manifest(env = NA), "`env` must be TRUE or FALSE")
  expect_error(morie_capsule_manifest(canonical = "yes"), "`canonical` must be TRUE or FALSE")
  expect_error(morie_capsule_manifest(path = 1), "`path` must be a single non-empty string")
  expect_error(morie_capsule_manifest(checks = list(name = "n")), "`checks` must be a data frame")
  expect_error(morie_capsule_manifest(checks = data.frame(name = "n", observed = 1)), "`checks` must be a data frame")
  expect_error(morie_capsule_manifest(checks = data.frame(name = c("n", "n"), observed = 1, expected = 1)),
               "`checks` repeats a name")
})

test_that("morie_capsule_compare recomputes from a manifest or its file", {
  d <- data.frame(x = 1:10)
  checks <- data.frame(name = c("n", "mean_x"), observed = c(10, 5.5), expected = c(10, 5.5))
  path <- withr::local_tempfile(fileext = ".json")
  man <- suppressMessages(morie_capsule_manifest(list(), checks, path = path))
  stats <- list(n = nrow, mean_x = function(z) mean(z$x))
  res <- morie_capsule_compare(path, d, stats)
  expect_s3_class(res, "bricklayer_recompute")
  expect_true(res$ok)
  expect_identical(res, rmoriebricklayer::mnfrcmp(man, d, stats))
  part <- morie_capsule_compare(man, d, stats["n"])
  expect_false(part$ok)
  expect_identical(part$unchecked, "mean_x")
  moved <- morie_capsule_compare(man, data.frame(x = 1:11), stats, tol = 0)
  expect_identical(moved$results$status, c("DIFFER", "DIFFER"))
})

test_that("morie_capsule_compare refuses bad arguments", {
  dir <- withr::local_tempdir()
  stats <- list(n = nrow)
  expect_error(morie_capsule_compare(file.path(dir, "gone.json"), data.frame(), stats), "`manifest` is not a manifest file")
  expect_error(morie_capsule_compare(dir, data.frame(), stats), "`manifest` is not a manifest file")
  expect_error(morie_capsule_compare(c("a", "b"), data.frame(), stats), "`manifest` must be a single")
  expect_error(morie_capsule_compare(list(meta = list()), data.frame(), stats), "`man` must be a manifest with recorded results")
  expect_error(morie_capsule_compare(morie_capsule_manifest(env = FALSE), data.frame(), list(nrow)),
               "`stats` must be a non-empty named list")
})
