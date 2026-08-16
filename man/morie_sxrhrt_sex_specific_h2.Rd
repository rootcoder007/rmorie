% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_sxrhrt_sex_specific_h2}
\alias{morie_sxrhrt_sex_specific_h2}
\title{morie_sxrhrt_sex_specific_h2}
\usage{
morie_sxrhrt_sex_specific_h2(y, sex, K, X = NULL, max_cycles = 60L, tol = 1e-09, male_label = 1)
}
\arguments{
\item{y}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{sex}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{K}{Passed to \code{.sxrhrt_rows}.}
\item{X}{Optional; may be \code{NULL}. Passed to \code{.sxrhrt_rows}.}
\item{max_cycles}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{60L}.}
\item{tol}{Accepted by the signature and not used anywhere in the body. Defaults to \code{1e-09}.}
\item{male_label}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{1}.}
}
\description{
A step of the sxrhrt_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{h2_male}, \code{h2_female}, \code{rg}, \code{sigma2_g_male}, \code{sigma2_g_female}, \code{sigma2_g_cross}, \code{sigma2_e_male}, \code{sigma2_e_female}, \code{coefficients}, \code{reml_loglik}, \code{reml_path}, \code{lrt_rg_equals_one}, \code{p_rg_equals_one}, \code{lrt_equal_h2}, \code{p_equal_h2}, \code{n}, \code{n_male}, \code{n_female}, \code{p}, \code{max_cross_sex_relatedness}, \code{cycles}, \code{converged}, \code{method}, \code{note}.
}
\keyword{internal}
