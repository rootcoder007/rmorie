% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.frwil_free_wilson}
\alias{.frwil_free_wilson}
\title{.frwil_free_wilson}
\usage{
.frwil_free_wilson(compounds, activity, constraint = "reference")
}
\arguments{
\item{compounds}{Passed to \code{.frwil_prep}.}
\item{activity}{Passed to \code{.frwil_prep}.}
\item{constraint}{Compared against \code{"sum_zero"}. Defaults to \code{"reference"}.}
}
\description{
A step of the frwil_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{coefficients}, \code{names}, \code{beta}, \code{fitted}, \code{residuals}, \code{rss}, \code{tss}, \code{r_squared}, \code{sigma}, \code{df_residual}, \code{n_parameters}, \code{occurrences}, \code{groups}, \code{reference}, \code{constraint}, \code{n_positions}, \code{method}.
}
\references{
----------
Free, S. M. & Wilson, J. W. (1964) "A mathematical contribution to
structure-activity studies", Journal of Medicinal Chemistry 7(4),
395-399, doi:10.1021/jm00334a001. The additive model above, the
indicator design over substituent positions, least-squares fitting,
and the singularity that forces a constraint.
}
\keyword{internal}
