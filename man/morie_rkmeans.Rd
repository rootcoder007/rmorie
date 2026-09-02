% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_rkmeans}
\alias{morie_rkmeans}
\title{morie_rkmeans}
\usage{
morie_rkmeans(X, k = 2, alpha = 0.1, penalty = "square", n_start = 20, max_iter = 100, 
    huber_c = 1.345, seed = 0, centers = NULL) 
}
\arguments{
\item{X}{A matrix; indexed by row and column.}
\item{k}{A count; the body uses it as \code{integer(...)}. Defaults to \code{2}.}
\item{alpha}{Numeric; combined arithmetically in the body. Defaults to \code{0.1}.}
\item{penalty}{Compared against \code{"huber"}. Defaults to \code{"square"}.}
\item{n_start}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{20}.}
\item{max_iter}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{100}.}
\item{huber_c}{Passed to \code{.rkmeans_concentrate}. Defaults to \code{1.345}.}
\item{seed}{Passed to \code{.ghc_rng}. Defaults to \code{0}.}
\item{centers}{Optional; may be \code{NULL}. A matrix; indexed by row and column.}
}
\description{
A step of the rkmeans_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{centers}, \code{labels}, \code{kept}, \code{outliers}, \code{criterion}, \code{distances}, \code{sizes}, \code{n_trimmed}, \code{n_kept}, \code{alpha}, \code{k}, \code{penalty}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
morie_rkmeans(V)
}
