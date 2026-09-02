% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_geron_principal_components}
\alias{morie_geron_principal_components}
\title{Principal components via SVD, sign-fixed, reconstruction error reported (Geron Ch 7, hmpcac)}
\usage{
morie_geron_principal_components(X, n_components = NULL, center = TRUE, scale = FALSE) 
}
\arguments{
\item{X}{Data (m, p).}
\item{n_components}{Components to keep (default all).}
\item{center,scale}{Preprocessing flags.}
}
\description{
Principal components via SVD, sign-fixed, reconstruction error reported (Geron Ch 7, hmpcac)
}
\value{
A list with \code{components}, \code{scores}, \code{explained_variance}, \code{explained_variance_ratio}, \code{reconstruction_error}, \code{n_components}, \code{estimate}, \code{n}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
morie_geron_principal_components(V)
}
