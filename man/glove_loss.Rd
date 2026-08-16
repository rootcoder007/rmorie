% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{glove_loss}
\alias{glove_loss}
\title{Eq. (8), evaluated over the nonzero entries only}
\usage{
glove_loss(X, W, Wt, b, bt, x_max = 100, alpha = 0.75)
}
\arguments{
\item{X}{A list; the body reads \code{$count}, \code{$i}, \code{$j} from it.}
\item{W}{A matrix; indexed by row and column.}
\item{Wt}{A matrix; indexed by row and column.}
\item{b}{A vector; indexed elementwise.}
\item{bt}{A vector; indexed elementwise.}
\item{x_max}{Defaults to \code{100}.}
\item{alpha}{Defaults to \code{0.75}.}
}
\description{
A step of the glove_native implementation. Called by \code{morie_glove}. See the file header for the source the module follows.
}
\value{
The value of \code{total}, as built in the body.
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
