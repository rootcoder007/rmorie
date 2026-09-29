# Coverage for taphonomy .. thrtmt_native exports. Every expectation is
# recomputed in the test body.

test_that("morie_taphonomy_fetch_usgs_soil reads a cached archive without downloading", {
  skip_if(!nzchar(Sys.which("zip")), "no zip binary")
  dest <- file.path(tempdir(), "usgs_soil_cov")
  dir.create(dest, showWarnings = FALSE)
  df <- data.frame(site = c("a", "b", "c"), ph = c(6.1, 7.4, 5.8), cu_ppm = c(12, 30, 8))
  csv <- file.path(dest, "ngdbsoil.csv")
  utils::write.csv(df, csv, row.names = FALSE)
  zp <- file.path(dest, "soil.zip")
  old <- setwd(dest)
  on.exit(setwd(old), add = TRUE)
  utils::zip(zp, "ngdbsoil.csv", flags = "-q")
  r <- morie_taphonomy_fetch_usgs_soil(dest = dest, nrows = 2, url = "https://example.invalid/soil.zip")
  expect_equal(r$ph, df$ph[1:2])
  expect_equal(r$site, df$site[1:2])
  expect_equal(attr(r, "source"), "https://example.invalid/soil.zip")
  all3 <- morie_taphonomy_fetch_usgs_soil(dest = dest, nrows = 10, url = "https://example.invalid/soil.zip")
  expect_equal(all3$cu_ppm, df$cu_ppm)
})

test_that("morie_taphonomy_morphosource_search builds the query and parses the response", {
  testthat::skip_on_covr()
  seen <- new.env()
  fake <- function(url, query = NULL, headers = character(), ...) {
    seen$url <- url
    seen$query <- query
    seen$headers <- headers
    list(status_code = 200L,
         body = '{"response":{"media":[{"id":"000A","title":"skull"},{"id":"000B","title":"femur"}],"pages":{"total_pages":3}}}')
  }
  testthat::local_mocked_bindings(.morie_dataset_http_text_with_status = fake,
                                  .package = if (isNamespaceLoaded("rmorie")) "rmorie" else "morie")
  withr::local_envvar(MORPHOSOURCE_API_URL = "https://ms.invalid/api", MORPHOSOURCE_API_KEY = "")
  r <- morie_taphonomy_morphosource_search("femur", media_type = "Mesh", per_page = 5, page = 2, api_key = "tok")
  expect_equal(seen$url, "https://ms.invalid/api/media")
  expect_equal(seen$query, list(q = "femur", search_field = "all_fields", f.media_type = "Mesh",
                                per_page = 5L, page = 2L))
  expect_equal(seen$headers, "Authorization: tok")
  expect_equal(r$n, 2L)
  expect_equal(r$total_pages, 3L)
  expect_equal(r$df, data.frame(id = c("000A", "000B"), title = c("skull", "femur")))
  po <- morie_taphonomy_morphosource_search(type = "physical-objects")
  expect_equal(seen$url, "https://ms.invalid/api/physical-objects")
  expect_length(seen$headers, 0)
  expect_equal(po$n, 0L)
  testthat::local_mocked_bindings(.morie_dataset_http_text_with_status = function(...) list(status_code = 404L, body = ""),
                                  .package = if (isNamespaceLoaded("rmorie")) "rmorie" else "morie")
  expect_error(morie_taphonomy_morphosource_search("x"), "HTTP 404")
})

test_that("morie_taulep replays its Poisson firings", {
  nu <- rbind(c(-1, 1), c(1, -1))
  prop <- function(x) c(0.3 * x[1], 0.1 * x[2])
  r <- morie_taulep(nu, prop, c(20, 5), tau = 0.5, n_steps = 6, seed = 4)
  e <- .ghc_rng(4)
  pois <- function(lam) {
    if (lam <= 0) return(0)
    k <- 0
    acc <- 0
    repeat {
      u <- .ghc_unif(e, 1)
      while (u <= 0) u <- .ghc_unif(e, 1)
      acc <- acc - log(u)
      if (acc > lam) return(k)
      k <- k + 1
    }
  }
  x <- c(20, 5)
  path <- matrix(x, 1)
  fired <- c(0, 0)
  for (s in 1:6) {
    a <- prop(x)
    for (j in 1:2) {
      k <- pois(a[j] * 0.5)
      fired[j] <- fired[j] + k
      x <- x + k * nu[j, ]
    }
    x[x < 0] <- 0
    path <- rbind(path, x)
  }
  expect_equal(r$path, unname(path))
  expect_equal(r$firings, fired)
  expect_equal(r$times, (0:6) * 0.5)
  expect_equal(rowSums(r$path), rep(25, 7))
  expect_error(morie_taulep(nu, prop, c(1, 2, 3), 0.5, 2), "must match the state length")
  expect_error(morie_taulep(nu, prop, c(1, 2), 0, 2), "tau must be positive")
  expect_error(morie_taulep(nu, function(x) -1, c(1, 2), 1, 1), "non-negative rates")
})

test_that("Taxass scores root-to-leaf paths and breaks ties by LCA", {
  par <- list(`1` = 1, `2` = 1, `3` = 1, `4` = 2, `5` = 2, `6` = 3)
  r <- Taxass(c(4, 4, 5, 2, 6, 0), par)
  # paths 4-2-1, 5-2-1, 6-3-1 with hit weights 4:2, 5:1, 2:1, 6:1
  expect_equal(unlist(r$leaf_scores), c(`4` = 3L, `5` = 2L, `6` = 1L))
  expect_equal(r$taxon, 4L)
  expect_equal(r$n_hit, 5L)
  expect_equal(Taxass(c(4, 5), par)$taxon, 2L)
  expect_equal(Taxass(c(4, 6), par)$taxon, 1L)
  expect_equal(Taxass(c(0, 0), par)$taxon, 0L)
  expect_error(Taxass(9, par), "not in parent map")
  expect_error(Taxass(1, list(`1` = 1, `2` = 7)), "missing taxon 7")
})

test_that("morie_emd and morie_tcls measure t-closeness", {
  p <- c(0.5, 0.3, 0.2, 0)
  q <- c(0.25, 0.25, 0.25, 0.25)
  expect_equal(morie_emd(p, q, "equal"), 0.5 * sum(abs(p - q)), tolerance = 1e-12)
  expect_equal(morie_emd(p, q), sum(abs(cumsum(p - q))) / 3, tolerance = 1e-12)
  hier <- list(root = c("A", "B"), A = c("u", "v"), B = c("w", "x"))
  d <- p - q
  pm <- function(k) min(sum(k[k > 0]), -sum(k[k < 0]))
  hc <- 0.5 * pm(d[1:2]) + 0.5 * pm(d[3:4]) + pm(c(sum(d[1:2]), sum(d[3:4])))
  expect_equal(morie_emd(p, q, "hierarchical", hier, c("u", "v", "w", "x")), hc, tolerance = 1e-12)
  expect_error(morie_emd(p, q, "hierarchical"), "needs both")
  expect_error(morie_emd(p, q[-1]), "cells")
  expect_error(morie_emd(p, q, "manhattan"), "ground must be")
  sal <- c(3, 4, 5, 6, 8, 11, 7, 9, 10)
  zip <- c("476", "476", "476", "4790", "4790", "4790", "476", "4790", "476")
  r <- morie_tcls(seq_along(sal), zip, sal, t = 0.3)
  dom <- sort(unique(sal))
  Q <- rep(1 / 9, 9)
  P <- function(g) tabulate(match(sal[zip == g], dom), 9) / sum(zip == g)
  dd <- c(sum(abs(cumsum(P("476") - Q))) / 8, sum(abs(cumsum(P("4790") - Q))) / 8)
  expect_equal(r$class_distances, dd, tolerance = 1e-12)
  expect_equal(r$achieved_t, max(dd), tolerance = 1e-12)
  expect_equal(r$satisfies, max(dd) <= 0.3)
  expect_equal(r$class_sizes, c(5L, 4L))
  expect_error(morie_tcls(1:3, c("a", "b", "c"), 1:3, t = -1), "non-negative")
  expect_error(morie_tcls(1:3, c("a", "b", "c"), 1:3, t = 1, domain = 1:2), "absent from the table-wide domain")
})

test_that("morie_tdcvar_cheatsheet and Tfidf", {
  expect_match(morie_tdcvar_cheatsheet(), "over-adjusts")
  docs <- list(c("a", "b", "a"), c("b", "c"), c("a", "d", "d", "d"))
  r <- Tfidf(docs)
  voc <- c("a", "b", "c", "d")
  tf <- t(vapply(docs, function(d) as.numeric(table(factor(d, voc))), numeric(4)))
  dfq <- colSums(tf > 0)
  expect_equal(r$vocab, voc)
  expect_equal(r$idf, log(3 / dfq), tolerance = 1e-12)
  expect_equal(r$W, unname(sweep(tf, 2, log(3 / dfq), "*")), tolerance = 1e-12)
  s <- Tfidf(docs, smooth = TRUE, sublinear = TRUE)
  tfs <- ifelse(tf > 0, 1 + log(pmax(tf, 1)), 0)
  expect_equal(s$W, unname(sweep(tfs, 2, log(1 + 3 / dfq), "*")), tolerance = 1e-12)
  expect_true(is.nan(Tfidf(list())$estimate))
})

test_that("Thomp and morie_thomp replay Beta-Bernoulli Thompson sampling", {
  p <- c(0.2, 0.7, 0.5)
  e <- .ghc_rng(9)
  a <- b <- rep(1, 3)
  act <- rew <- numeric(15)
  for (t in 1:15) {
    th <- vapply(1:3, function(k) .ghc_beta1(e, a[k], b[k]), 0)
    k <- which.max(th)
    r <- as.numeric(.ghc_unif(e, 1) < p[k])
    a[k] <- a[k] + r
    b[k] <- b[k] + 1 - r
    act[t] <- k - 1
    rew[t] <- r
  }
  for (f in list(Thomp, morie_thomp)) {
    s <- f(p, 15, seed = 9)
    expect_equal(s$actions, act)
    expect_equal(s$rewards, rew)
    expect_equal(s$alpha, a)
    expect_equal(s$post_mean, a / (a + b), tolerance = 1e-12)
    expect_equal(s$estimate, which.max(a / (a + b)) - 1)
    expect_error(f(c(0.2, 1.5), 3), "p must lie")
    expect_error(f(p, 3, alpha0 = 1), "length K")
  }
})

test_that("thrtmt_blip_function projects the fitted blip onto V", {
  y <- c(2.1, 3.5, 1.2, 4.8, 2.9, 3.3, 1.7, 5.1)
  A <- c(0, 1, 0, 1, 1, 0, 0, 1)
  W <- cbind(c(0.5, 1.2, -0.3, 2.0, 0.8, 1.1, -0.6, 1.9), c(1, 0, 1, 1, 0, 0, 1, 1))
  r <- thrtmt_blip_function(y, A, W)
  f <- stats::lm(y ~ A * W)
  b <- stats::coef(f)
  blip <- b[["A"]] + as.numeric(W %*% b[c("A:W1", "A:W2")])
  # the normal equations carry an absolute 1e-8 ridge, amplified by the
  # conditioning of the 8 x 6 interaction design
  expect_equal(r$blip, blip, tolerance = 1e-6)
  expect_equal(r$info$q1 - r$info$q0, r$blip, tolerance = 1e-12)
  v <- thrtmt_blip_function(y, A, W, V = W[, 1])
  expect_equal(v$blip, unname(stats::fitted(stats::lm(blip ~ W[, 1]))), tolerance = 1e-6)
  n0 <- thrtmt_blip_function(y, A, NULL)
  expect_equal(n0$blip, rep(mean(y[A == 1]) - mean(y[A == 0]), 8), tolerance = 1e-7)
  expect_same_function(morie_thrtmt, thrtmt_blip_function)
  expect_error(thrtmt_blip_function(y, A + 1, W), "binary 0/1")
  expect_error(thrtmt_blip_function(y, A[-1], W), "8 outcomes but 7 treatments")
})
