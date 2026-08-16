% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.evt_pwm}
\alias{.evt_pwm}
\title{Unbiased probability-weighted moment b_r (Greenwood et al. 1979;}
\usage{
.evt_pwm(x, r)
}
\arguments{
\item{x}{See Usage.}
\item{r}{A count; the body uses it as \code{seq_len(...)}.}
}
\description{
Hosking-Wallis-Wood 1985 Eq. 4) -- not the plotting-position approximation, whose small-sample bias is exactly what these methods exist to avoid.
}
\value{
A numeric value.
}
\keyword{internal}
