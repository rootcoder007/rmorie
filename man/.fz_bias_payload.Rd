% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.fz_bias_payload}
\alias{.fz_bias_payload}
\title{.fz_bias_payload}
\usage{
.fz_bias_payload(cm)
}
\arguments{
\item{cm}{A list; the body reads \code{$gp}, \code{$gpp}, \code{$tr} from it.}
}
\description{
A step of the fauzi_native implementation. Called by \code{morie_fauzi_b1_coefficient}, \code{morie_fauzi_b2_coefficient}, \code{morie_fauzi_b3_coefficient}. See the file header for the source the module follows.
}
\value{
A list with \code{g_prime}, \code{g_double_prime}, \code{bias_order}, \code{contrast}, \code{transform}.
}
\keyword{internal}
