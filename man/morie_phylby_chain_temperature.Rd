% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_phylby_chain_temperature}
\alias{morie_phylby_chain_temperature}
\title{morie_phylby_chain_temperature}
\usage{
morie_phylby_chain_temperature(j, lam = 0.2)
}
\arguments{
\item{j}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{lam}{Numeric; combined arithmetically in the body. Defaults to \code{0.2}.}
}
\description{
A step of the phylby_native implementation. Called by \code{morie_phylby}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
morie_phylby_chain_temperature(j = 5L)
}
