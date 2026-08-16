% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_gr_bf16_bits}
\alias{.morie_gr_bf16_bits}
\title{Round the float32 bit pattern to the BF16 grid. mode is}
\usage{
.morie_gr_bf16_bits(u, mode = "nearest_even")
}
\arguments{
\item{u}{Numeric; combined arithmetically in the body.}
\item{mode}{Compared against \code{"nearest_even"}. Defaults to \code{"nearest_even"}.}
}
\description{
"nearest_even" or "truncate"; arithmetic is done in doubles and folded back mod 2^32, exactly reproducing numpy\\'s uint32/uint64 masking.
}
\value{
A numeric value.
}
\keyword{internal}
