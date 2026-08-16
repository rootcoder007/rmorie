% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_bx_istft}
\alias{.morie_bx_istft}
\title{.morie_bx_istft}
\usage{
.morie_bx_istft(re_f, im_f, nwin, hop, win, n)
}
\arguments{
\item{re_f}{A matrix; indexed by row and column.}
\item{im_f}{A matrix; indexed by row and column.}
\item{nwin}{A count; the body uses it as \code{seq_len(...)}.}
\item{hop}{Numeric; combined arithmetically in the body.}
\item{win}{A vector; indexed elementwise.}
\item{n}{A count; the body uses it as \code{numeric(...)}.}
}
\description{
A step of the rangayyan_class2 implementation. Called by \code{FecgNmf}. See the file header for the source the module follows.
}
\value{
The value of \code{ifelse}.
}
\keyword{internal}
