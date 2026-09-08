% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{sdid}
\alias{sdid}
\title{sdid}
\usage{
sdid(Y, treated, t_post, method = "sdid", zeta = NULL) 
}
\arguments{
\item{Y}{Passed to \code{.causscd_grid}.}
\item{treated}{Passed to \code{.causscd_grid}.}
\item{t_post}{A count; the body uses it as \code{seq_len(...)}.}
\item{method}{One of \code{"did"}, \code{"sc"}, \code{"sdid"}. Defaults to \code{"sdid"}.}
\item{zeta}{Passed to \code{unit_weights}.}
}
\description{
A step of the causscd_native implementation. Called by \code{causscd}. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{tau}, \code{unit_weights}, \code{time_weights}, \code{zeta}, \code{delta_treated}, \code{delta_control}, \code{method_name}, \code{n_treated}, \code{n_control}, \code{t_pre}, \code{t_post}, \code{method}, \code{note}.
}
\keyword{internal}
\examples{
set.seed(2)
Y <- lapply(1:8, function(i) rnorm(10, 10))
for (i in 7:8) Y[[i]][7:10] <- Y[[i]][7:10] + 3
treated <- c(rep(0L, 6), 1L, 1L)
r <- sdid(Y, treated, t_post = 6)
str(r, max.level = 1)
}
