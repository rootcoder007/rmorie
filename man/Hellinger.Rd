% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Hellinger}
\alias{Hellinger}
\title{H = sqrt(1 - BC), so H^2 = 1 - BC.  The Python arm delegates to}
\usage{
Hellinger(p1, p2)
}
\arguments{
\item{p1}{See Usage.}
\item{p2}{See Usage.}
}
\description{
morie.fn.helld.hellinger_dist; this is the same arithmetic, kept here because the R tree has no separate helld module.  Unlike the Bhattacharyya distance -ln BC, this is a TRUE METRIC: bounded in [0, 1], symmetric, and it satisfies the triangle inequality, which -ln BC does not.  That is the reason to reach for it -- anything needing a metric over distributions needs this and not D_B.  The 1/2 normalization is the usual one; unnormalized gives H^2 = 2(1 - BC), so the convention is reported rather than assumed.  NOT FROM RANGAYYAN (2024).
}
\value{
A list with \code{hellinger}, \code{squared}, \code{bhattacharyya_coefficient}, \code{identity_h2_equals_one_minus_bc}, \code{is_a_true_metric}, \code{satisfies_the_triangle_inequality}, \code{bhattacharyya_distance_does_not}, \code{normalization}, \code{in_unit_interval}, \code{reference}, \code{not_from_this_book}, \code{method}.
}
\keyword{internal}
