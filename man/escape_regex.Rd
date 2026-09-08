% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{escape_regex}
\alias{escape_regex}
\title{escape_regex}
\usage{
escape_regex(x) 
}
\arguments{
\item{x}{Character; passed to \code{gsub}.}
}
\description{
A step of the manifest implementation. Called by \code{morie_build_outputs_manifest}. See the file header for the source the module follows.
}
\value{
The value of \code{gsub}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
escape_regex(V)
}
