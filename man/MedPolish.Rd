% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{MedPolish}
\alias{MedPolish}
\title{Tukey (1977), Exploratory Data Analysis.  NOT in Schabenberger &}
\usage{
MedPolish(values, grid = NULL, iters = 10L) 
}
\arguments{
\item{values}{A matrix; passed to \code{as.matrix}.}
\item{grid}{Optional; may be \code{NULL}. Coerced to integer by the body, with \code{as.integer}.}
\item{iters}{A count; the body uses it as \code{seq_len(...)}. Defaults to \code{10L}.}
}
\description{
Gotway -- a fixed-string search for "polish" returns nothing; the book\'s trend removal is the OLS trend surface of Sec. 5.3.1.  Median polish is resistant to outliers, which is why the geostatistical literature reaches for it first.  A sweep must run row-then-column in a FIXED order; median polish is not order-invariant.
}
\value{
A list with \code{overall}, \code{row}, \code{col}, \code{residuals}, \code{fitted}, \code{abs_residual_sum}, \code{sweeps}, \code{resistant_to_outliers}, \code{nrow}, \code{ncol}, \code{n}, \code{method}.
}
\keyword{internal}
\examples{
M <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2)
MedPolish(M)
}
