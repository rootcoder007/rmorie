% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{calibration_hellinger}
\alias{calibration_hellinger}
\title{Hellinger distance \eqn{\|\sqrt p - \sqrt q\|_2 / \sqrt 2}.}
\usage{
calibration_hellinger(p, q)
}
\arguments{
\item{p}{Target distribution \eqn{p(g \mid u)}.}
\item{q}{Recommended distribution \eqn{q(g \mid u)}.}
}
\description{
Well defined at zeros and less brutal than KL where \eqn{p} is small -- the paper\'s named alternative.
}
\value{
Numeric scalar, the Hellinger distance.
}
\keyword{internal}
