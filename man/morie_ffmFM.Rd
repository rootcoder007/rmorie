% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_ffmFM}
\alias{morie_ffmFM}
\title{morie_ffmFM}
\usage{
morie_ffmFM(rows, labels, fields, n_features, n_fields, k_dim = 4, eta = 0.1, 
    lam = 2e-05, epochs = 10, seed = 0) 
}
\arguments{
\item{rows}{Passed to \code{fit_ffm}.}
\item{labels}{Passed to \code{fit_ffm}.}
\item{fields}{Passed to \code{fit_ffm}.}
\item{n_features}{Passed to \code{fit_ffm}.}
\item{n_fields}{Passed to \code{fit_ffm}.}
\item{k_dim}{Passed to \code{fit_ffm}. Defaults to \code{4}.}
\item{eta}{Passed to \code{fit_ffm}. Defaults to \code{0.1}.}
\item{lam}{Passed to \code{fit_ffm}. Defaults to \code{2e-05}.}
\item{epochs}{Passed to \code{fit_ffm}. Defaults to \code{10}.}
\item{seed}{Passed to \code{fit_ffm}. Defaults to \code{0}.}
}
\description{
A step of the ffmFM_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
The value of \code{fit_ffm}.
}
\keyword{internal}
\examples{
set.seed(3)
rows <- list(list(list(0L, 1), list(2L, 1)),
             list(list(1L, 1), list(3L, 1)))
labels <- c(1, -1)
r <- morie_ffmFM(rows, labels, fields = c(0L, 0L, 1L, 1L),
                 n_features = 4, n_fields = 2, k_dim = 2, epochs = 3)
str(r, max.level = 1)
}
