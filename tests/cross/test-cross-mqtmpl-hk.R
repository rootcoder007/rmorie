# SPDX-License-Identifier: AGPL-3.0-or-later
test_that("Haley-Knott scan matches qtl::scanone(method = 'hk')", {
  skip_if_not_installed("qtl")
  set.seed(3)
  map <- qtl::sim.map(len = 100, n.mar = 11, include.x = FALSE, eq.spacing = TRUE)
  cr <- qtl::sim.cross(map, type = "bc", n.ind = 150, model = c(1, 43, 0.9))
  g <- qtl::pull.geno(cr) - 1L
  g[sample(length(g), 60)] <- NA
  cr$geno[[1]]$data <- g + 1L
  mk <- lapply(seq_len(ncol(g)), function(j) g[, j])
  pos <- as.numeric(map[[1]])
  sex <- rbinom(150, 1, 0.5)
  cr$pheno[, 1] <- cr$pheno[, 1] + 0.7 * sex
  cg <- qtl::calc.genoprob(cr, step = 2, error.prob = 1e-4, map.function = "haldane", off.end = 0)
  for (cv in list(NULL, sex)) {
    ref <- if (is.null(cv)) qtl::scanone(cg, method = "hk") else qtl::scanone(cg, method = "hk", addcovar = cv)
    r <- morie_mqtmpl_scanone(cr$pheno[, 1], mk, pos, method = "hk", step = 2,
                              covariates = if (is.null(cv)) list() else list(cv), error_rate = 1e-4)
    m <- match(round(ref$pos, 6), round(r$position, 6))
    expect_false(anyNA(m))
    expect_lt(max(abs(ref$lod - r$lod[m])), 1e-10)
  }
})
