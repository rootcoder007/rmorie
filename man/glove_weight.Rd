% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{glove_weight}
\alias{glove_weight}
\title{Eq. (9): the GloVe weighting function. f(0) = 0, non-decreasing,}
\usage{
glove_weight(x, x_max = 100, alpha = 0.75)
}
\arguments{
\item{x}{Numeric; combined arithmetically in the body.}
\item{x_max}{Numeric; combined arithmetically in the body. Defaults to \code{100}.}
\item{alpha}{Numeric; combined arithmetically in the body. Defaults to \code{0.75}.}
}
\description{
capped at 1.
}
\value{
A numeric value.
}
\references{
----------
Pennington, J., Socher, R. & Manning, C. D. (2014) "GloVe: Global
Vectors for Word Representation", Proceedings of EMNLP 2014,
1532-1543, doi:10.3115/v1/D14-1162.  Equations (8) and (9), Sec. 4.2
for the harmonic weighting and the AdaGrad training.

Duchi, J., Hazan, E. & Singer, Y. (2011) "Adaptive subgradient methods
for online learning and stochastic optimization", JMLR 12, 2121-2159
-- the AdaGrad the paper uses.
}
\keyword{internal}
\examples{
glove_weight(x = 5L)
}
