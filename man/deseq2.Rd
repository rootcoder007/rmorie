% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{deseq2}
\alias{deseq2}
\title{DESeq2-style differential expression}
\usage{
deseq2(counts, design, contrast = NULL, size = NULL, beta_prior = TRUE, quantile_p = 0.05, alpha_init = 0.1, min_disp = 1e-08, log2 = TRUE)
}
\arguments{
\item{counts}{Gene-by-sample count matrix.}
\item{design}{Design matrix or vector of group labels.}
\item{contrast}{Optional contrast vector.}
\item{size}{Optional size factors.}
\item{beta_prior}{Whether to apply the LFC prior.}
\item{quantile_p}{\code{p} for the quantile matching that sets the   prior width.}
\item{alpha_init}{Initial dispersion for the starting GLM.}
\item{min_disp}{Floor on the final dispersion.}
\item{log2}{Whether to report log2 fold changes.}
}
\description{
Full pipeline: size factors, gene-wise dispersion, trend, MAP dispersion (with outliers left unshrunk), ridge-penalised LFC, Wald test, BH.
}
\value{
A list mirroring the Python \code{RichResult} payload.
}
\keyword{internal}
\examples{
set.seed(3)
design <- as.list(rep(c("A", "B"), each = 3))
counts <- lapply(1:30, function(g) {
  base <- exp(rnorm(1, 5, 0.4))
  fc <- if (g <= 8) 2.5 else 1
  c(rnbinom(3, mu = base, size = 15),
    rnbinom(3, mu = base * fc, size = 15))
})
r <- deseq2(counts, design)
str(r, max.level = 1)
}
