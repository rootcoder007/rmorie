% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.schab_fit_reml}
\alias{.schab_fit_reml}
\title{.schab_fit_reml}
\usage{
.schab_fit_reml(coords, z, X, model = "exponential", start_ratio = 0.1, start_range = NULL, max_iter = 200L, tol = 1e-10)
}
\arguments{
\item{coords}{A matrix; passed to \code{nrow}.}
\item{z}{A vector; its length is taken.}
\item{X}{A matrix; passed to \code{nrow}.}
\item{model}{Passed to \code{.schab_profiled_reml}. Defaults to \code{"exponential"}.}
\item{start_ratio}{Passed to \code{.schab_logit}. Defaults to \code{0.1}.}
\item{start_range}{Optional; may be \code{NULL}. Numeric; passed to \code{log}.}
\item{max_iter}{A count; the body uses it as \code{seq_len(...)}. Defaults to \code{200L}.}
\item{tol}{Passed to \code{<}. Defaults to \code{1e-10}.}
}
\description{
A step of the schab_reml_shared implementation. Called by \code{spreml}. See the file header for the source the module follows.
}
\value{
A list with \code{nugget_ratio}, \code{range}, \code{sigma2}, \code{nugget}, \code{partial_sill}, \code{beta}, \code{neg2_restricted_loglik}, \code{converged}.
}
\keyword{internal}
\examples{
.schab_fit_reml(coords = c(1, 2, 3, 4, 5, 6, 7, 8), z = c(1, 2, 3, 4, 5, 6, 7, 8), X = c(1, 2, 3, 4, 5, 6, 7, 8))
}
