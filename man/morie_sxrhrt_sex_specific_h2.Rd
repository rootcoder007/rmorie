% Generated from the module source by ledger/gen_rd.R -- roxygen2 hangs
% on this package. Usage comes from args(), so it tracks the formals.
\name{morie_sxrhrt_sex_specific_h2}
\alias{morie_sxrhrt_sex_specific_h2}
\alias{morie_sxrhrt}
\title{Bivariate REML treating the sexes as two traits on}
\usage{
morie_sxrhrt_sex_specific_h2(y, sex, K, X = NULL, max_cycles = 60L, tol = 1e-09, male_label = 1)
}
\description{
R arm of sxrhrt -- bivariate REML treating the sexes as two traits on disjoint individuals, with the genetic correlation parameterised directly and bounded to (-1, 1).
}
\value{
A list with components \code{n}, \code{sv}, \code{theta}, \code{estimate}, \code{h2_male}, \code{h2_female}, \code{rg}, \code{sigma2_g_male}, \code{sigma2_g_female}, \code{sigma2_g_cross}, \code{sigma2_e_male}, \code{sigma2_e_female}, \code{coefficients}, \code{reml_loglik}, \code{reml_path}, \code{lrt_rg_equals_one}, \code{p_rg_equals_one}, \code{lrt_equal_h2}, \code{p_equal_h2}, \code{n_male}, \code{n_female}, \code{p}, \code{max_cross_sex_relatedness}, \code{cycles}, \code{converged}, \code{method}, \code{note}.
}
\references{
Yang, J. et al. (2011) Am. J. Hum. Genet. 88(1), 76-82 (GCTA);
Lee, S. H. et al. (2012) Bioinformatics 28(19), 2540-2542.
}
\keyword{internal}
