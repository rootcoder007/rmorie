% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_sctraj_cosine_cdf}
\alias{morie_sctraj_cosine_cdf}
\title{CDF of a cosine kernel of bandwidth 1/2: density 1 + cos(2 pi u)}
\usage{
morie_sctraj_cosine_cdf(u) 
}
\arguments{
\item{u}{Numeric; combined arithmetically in the body.}
}
\description{
on [-1/2, 1/2], so F(u) = u + 1/2 + sin(2 pi u)/(2 pi).
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
morie_sctraj_cosine_cdf(u = 5L)
}
