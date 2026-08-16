% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{edgrn_tmm_factor}
\alias{edgrn_tmm_factor}
\title{edgrn_tmm_factor}
\usage{
edgrn_tmm_factor(counts_sample, counts_reference, trim_m = 0.3, trim_a = 0.05, lib_sample = NULL, lib_reference = NULL)
}
\arguments{
\item{counts_sample}{Passed to \code{.edgrn_vec}.}
\item{counts_reference}{Passed to \code{.edgrn_vec}.}
\item{trim_m}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0.3}.}
\item{trim_a}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0.05}.}
\item{lib_sample}{Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.}
\item{lib_reference}{Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.}
}
\description{
A step of the edgrn_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{factor}, \code{log2_factor}, \code{n_used}, \code{n_genes}, \code{trimmed_m}, \code{trimmed_a}, \code{note}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
edgrn_tmm_factor(V, V)
}
