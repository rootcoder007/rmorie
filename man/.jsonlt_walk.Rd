% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.jsonlt_walk}
\alias{.jsonlt_walk}
\title{Structure only: a comma inside a string stays a comma, which is why}
\usage{
.jsonlt_walk(txt, open_pad, close_pad, comma_pad, colon_txt, keep_ws)
}
\arguments{
\item{txt}{Passed to \code{paste}.}
\item{open_pad}{Accepted by the signature and not used anywhere in the body.}
\item{close_pad}{Accepted by the signature and not used anywhere in the body.}
\item{comma_pad}{Accepted by the signature and not used anywhere in the body.}
\item{colon_txt}{Passed to \code{c}.}
\item{keep_ws}{A flag; the body branches on it.}
}
\description{
this walks characters instead of running a regex over the text.
}
\value{
A character value.
}
\keyword{internal}
\examples{
.jsonlt_walk(txt = c(1, 2, 3, 4, 5, 6, 7, 8), open_pad = c(1, 2, 3, 4, 5, 6, 7, 8), close_pad = c(1, 2, 3, 4, 5, 6, 7, 8), comma_pad = c(1, 2, 3, 4, 5, 6, 7, 8), colon_txt = c(1, 2, 3, 4, 5, 6, 7, 8), keep_ws = c(1, 2, 3, 4, 5, 6, 7, 8))
}
