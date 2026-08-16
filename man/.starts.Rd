% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.starts}
\alias{.starts}
\title{Several starting points for the smoothing parameters. Nelder-Mead}
\usage{
.starts(spec)
}
\arguments{
\item{spec}{A list; the body reads \code{$damped}, \code{$harmonics}, \code{$p}, \code{$periods}, \code{$q}, \code{$use_box_cox}, \code{$use_trend} from it.}
}
\description{
builds its initial simplex by nudging each coordinate by 5\% of its own value (the same rule as scipy), so a single start of alpha = 0.09 only ever explores a box 0.004 wide. Starting from a small grid instead is what makes the search actually search.
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
