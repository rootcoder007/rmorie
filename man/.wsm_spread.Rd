% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.wsm_spread}
\alias{.wsm_spread}
\title{Silverman (3.30): A = min(standard deviation, interquartile}
\usage{
.wsm_spread(x)
}
\arguments{
\item{x}{Coerced to numeric by the body, with \code{as.numeric}.}
}
\description{
range / 1.34). The divisor is 1.34 as the book prints it; 1.349 is the normal-theory value it rounds. Using the plain standard deviation is what makes the rule oversmooth skewed and long-tailed data -- one outlier moves the standard deviation a long way and the interquartile range hardly at all.
}
\value{
One of two values, depending on the branch taken.
}
\keyword{internal}
