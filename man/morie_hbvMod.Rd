% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_hbvMod}
\alias{morie_hbvMod}
\title{morie_hbvMod}
\usage{
morie_hbvMod(precip, temp, epot, params, init = NULL)
}
\arguments{
\item{precip}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{temp}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{epot}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{params}{A list; the body reads \code{$beta}, \code{$cfmax}, \code{$cfr}, \code{$fc}, \code{$k0}, \code{$k1}, \code{$k2}, \code{$lp}, \code{$maxbas}, \code{$perc}, \code{$tt}, \code{$uzl} from it.}
\item{init}{Optional; may be \code{NULL}. A list; the body reads \code{$slz}, \code{$snow}, \code{$soil}, \code{$suz}, \code{$swater} from it.}
}
\description{
Part of the hbvMod_mixedcase_native implementation; see the file header for the source it follows.
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
