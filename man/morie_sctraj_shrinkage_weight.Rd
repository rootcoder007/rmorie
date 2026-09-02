% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_sctraj_shrinkage_weight}
\alias{morie_sctraj_shrinkage_weight}
\title{morie_sctraj_shrinkage_weight}
\usage{
morie_sctraj_shrinkage_weight(t, t_min, t_max, arg = "shifted") 
}
\arguments{
\item{t}{Numeric; combined arithmetically in the body.}
\item{t_min}{Numeric; combined arithmetically in the body.}
\item{t_max}{Numeric; combined arithmetically in the body.}
\item{arg}{One of \code{"as_printed"}, \code{"shifted"}. Defaults to \code{"shifted"}.}
}
\description{
A step of the sctraj_native implementation. Called by \code{morie_sctraj}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
morie_sctraj_shrinkage_weight(t = 5, t_min = 0, t_max = 10)
}
