% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.tf_mother}
\alias{.tf_mother}
\title{.tf_mother}
\usage{
.tf_mother(name, t, w0 = 5)
}
\arguments{
\item{name}{Coerced to character by the body, with \code{as.character}.}
\item{t}{A vector; its length is taken.}
\item{w0}{Numeric; combined arithmetically in the body. Defaults to \code{5}.}
}
\description{
A step of the rangayyan_tf implementation. Called by \code{.tf_cwt}. See the file header for the source the module follows.
}
\value{
Nothing; this branch always raises.
}
\keyword{internal}
