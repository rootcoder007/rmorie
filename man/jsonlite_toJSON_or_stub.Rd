% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{jsonlite_toJSON_or_stub}
\alias{jsonlite_toJSON_or_stub}
\title{Ponytail: no jsonlite dependency declared for this package; write()}
\usage{
jsonlite_toJSON_or_stub(x) 
}
\arguments{
\item{x}{Passed to \code{.s03json_toJSON}.}
}
\description{
only when a file path is actually given, and fall back to a minimal deparse so the optional side effect never hard-fails.
}
\value{
One of two values, depending on the branch taken.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
jsonlite_toJSON_or_stub(V)
}
