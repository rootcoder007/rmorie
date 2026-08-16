% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_didfst_panel_differences}
\alias{morie_didfst_panel_differences}
\title{Post-minus-pre differences}
\usage{
morie_didfst_panel_differences(Y, event_time)
}
\arguments{
\item{Y}{Numeric matrix (n by T).}
\item{event_time}{Integer H in 1-based period numbers.}
}
\description{
Periods \code{1..H} are pre-treatment for every unit and \code{H+1..T} post, exactly as eq. (13.7) of Wager (2025) writes it.
}
\value{
Numeric vector of length n.
}
\keyword{internal}
