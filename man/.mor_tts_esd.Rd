% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.mor_tts_esd}
\alias{.mor_tts_esd}
\title{.mor_tts_esd}
\usage{
.mor_tts_esd(res, k, alpha, hybrid = TRUE)
}
\arguments{
\item{res}{A vector; its length is taken.}
\item{k}{A count; the body uses it as \code{seq_len(...)}.}
\item{alpha}{Numeric; combined arithmetically in the body.}
\item{hybrid}{A flag; the body branches on it. Defaults to \code{TRUE}.}
}
\description{
A step of the ttsAn_native implementation. Called by \code{morie_ttsAn}. See the file header for the source the module follows.
}
\value{
A list with \code{anoms}, \code{stats}, \code{lams}.
}
\keyword{internal}
