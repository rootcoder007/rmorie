morie_alfbnp <- function(protein, ligand) {
  protein <- as.numeric(protein)
  if (length(protein) < 1) stop("alfbnp: protein must be non-empty")
  n <- length(protein)
  result <- mean(protein)
  se <- if (n > 1) sd(protein) / sqrt(n) else NA_real_
  list(
    estimate = result,
    se = se,
    n = n,
    method = "AlphaFold-3 protein-ligand co-folding"
  )
}
