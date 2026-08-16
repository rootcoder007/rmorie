% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_match_stratum_key}
\alias{.morie_match_stratum_key}
\title{Internal helper: stratum key}
\usage{
.morie_match_stratum_key(df, vars)
}
\description{
Internal helper: stratum key
}
\value{
The value of \code{do.call}.
}
\keyword{internal}
\examples{
.morie_match_stratum_key(df = data.frame(x = c(1, 2, 3, 4), y = c(2, 4, 5, 9)), vars = factor(c("lo", "hi", "lo", "hi")))
}
