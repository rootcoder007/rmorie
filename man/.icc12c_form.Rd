% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.icc12c_form}
\alias{.icc12c_form}
\title{Normalise a form label to one of the six keys. Anything spelling out}
\usage{
.icc12c_form(model, k)
}
\arguments{
\item{model}{See Usage.}
\item{k}{See Usage.}
}
\description{
a form is accepted -- "ICC(2,k)", "2-k", "2k" -- and, because the paper labels its own table with the concrete number of judges (ICC(1,4) for k = 4), a trailing integer equal to k is read as the average-measure form.
}
\value{
The value of \code{s}, as built in the body.
}
\keyword{internal}
