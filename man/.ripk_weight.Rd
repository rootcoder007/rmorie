% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.ripk_weight}
\alias{.ripk_weight}
\title{Internal: proportion of the circle of radius `rad` centred at (x, y)}
\usage{
.ripk_weight(x, y, rad, x0, x1, y0, y1)
}
\arguments{
\item{x}{Numeric; combined arithmetically in the body.}
\item{y}{Numeric; combined arithmetically in the body.}
\item{rad}{Numeric; combined arithmetically in the body.}
\item{x0}{Numeric; combined arithmetically in the body.}
\item{x1}{Numeric; combined arithmetically in the body.}
\item{y0}{Numeric; combined arithmetically in the body.}
\item{y1}{Numeric; combined arithmetically in the body.}
}
\description{
that lies inside the rectangle [x0, x1] x [y0, y1] -- Ripley\\'s edge correction weight w(s_i, s_j) (Schabenberger & Gotway 2005, p. 102). Two opposite sides can never both be crossed by the same angle, so the only overlaps between the four "outside" arcs are the four corners and there are no triple intersections; inclusion-exclusion is exact.
}
\value{
One of two values, depending on the branch taken.
}
\keyword{internal}
