% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.wrd2v_neg_step}
\alias{.wrd2v_neg_step}
\title{Mikolov et al. (2013b) eq. 4, one positive and k noise draws}
\usage{
.wrd2v_neg_step(st, c, j, size, lr, k, draw_noise)
}
\arguments{
\item{st}{A list; the body reads \code{$O}, \code{$W} from it.}
\item{c}{See Usage.}
\item{j}{See Usage.}
\item{size}{A count; the body uses it as \code{numeric(...)}.}
\item{lr}{Numeric; combined arithmetically in the body.}
\item{k}{A count; the body uses it as \code{seq_len(...)}.}
\item{draw_noise}{See Usage.}
}
\description{
A step of the wrd2v_native implementation. Called by \code{morie_wrd2v_wrd2v}. See the file header for the source the module follows.
}
\value{
The value of \code{loss}, as built in the body.
}
\keyword{internal}
