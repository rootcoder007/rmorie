# GWAS quality control: the seven standard steps.
# 
# Marees, A. T., de Kluiver, H., Stringer, S., Vorspan, F., Curis, E., Marie-
# Claire, C., & Derks, E. M. (2018) "A tutorial on conducting genome-wide
# association studies: Quality control and statistical analysis",
# International Journal of Methods in Psychiatric Research 27(2), e1608.
# ... (full citation)

snpqc1_check <- function(genotypes) {
  # Convert to list of lists
  G <- lapply(genotypes, function(row) as.list(row))
  if (length(G) == 0 || length(G[[1]]) == 0) {
    stop("snpqc1: genotypes must be a non-empty individual x SNP matrix")
  }
  m <- length(G[[1]])
  for (i in seq_along(G)) {
    row <- G[[i]]
    if (length(row) != m) {
      stop("snpqc1: ragged genotype matrix")
    }
    for (g in row) {
      if (!is.null(g) && !is.na(g) && !g %in% c(0, 1, 2, 0.0, 1.0, 2.0)) {
        stop("snpqc1: genotypes must be 0, 1, 2 or NULL/NA")
      }
    }
  }
  list(G = G, n = length(G), m = m)
}
