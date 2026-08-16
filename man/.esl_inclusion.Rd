% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.esl_inclusion}
\alias{.esl_inclusion}
\title{(7.55): Pr{i in bootstrap sample b} = 1 - (1 - 1/n)^n -> 1 - e^-1}
\usage{
.esl_inclusion(n)
}
\arguments{
\item{n}{Numeric; combined arithmetically in the body.}
}
\description{
The exact finite-n value, not the limit; they differ by about 1\% at n = 20, which is the regime where anyone reaches for a bootstrap.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
.esl_inclusion(n = 5L)
}
