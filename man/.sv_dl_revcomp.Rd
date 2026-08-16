% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.sv_dl_revcomp}
\alias{.sv_dl_revcomp}
\title{.sv_dl_revcomp}
\usage{
.sv_dl_revcomp(s)
}
\arguments{
\item{s}{Character; passed to \code{toupper}.}
}
\description{
A step of the sv_dl_native implementation. Called by \code{morie_sv_dl_deletion_type_reference}. See the file header for the source the module follows.
}
\value{
A character value.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.sv_dl_revcomp(V)
}
