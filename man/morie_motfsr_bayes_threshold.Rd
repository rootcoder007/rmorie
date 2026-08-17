% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_motfsr_bayes_threshold}
\alias{morie_motfsr_bayes_threshold}
\title{Bayes-optimal threshold}
\usage{
morie_motfsr_bayes_threshold(lambda1, loss = NULL)
}
\arguments{
\item{lambda1}{Component 1 weight.}
\item{loss}{Optional 2x2 loss matrix.}
}
\description{
Bayes-optimal threshold
}
\value{
The threshold t.
}
\keyword{internal}
\examples{
morie_motfsr_bayes_threshold(lambda1 = 0.3)
}
