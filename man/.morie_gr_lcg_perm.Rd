% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_gr_lcg_perm}
\alias{.morie_gr_lcg_perm}
\title{LCG Fisher-Yates over 0..n-1, exactly the Python loop (i from n-1}
\usage{
.morie_gr_lcg_perm(n, seed)
}
\arguments{
\item{n}{A count; the body uses it as \code{seq_len(...)}.}
\item{seed}{Coerced to numeric by the body, with \code{as.numeric}.}
}
\description{
down) Part of the geron_train_native implementation; see the file header for the source it follows.
}
\value{
The value of \code{perm}, as built in the body.
}
\keyword{internal}
\examples{
.morie_gr_lcg_perm(n = 5L, seed = 5L)
}
