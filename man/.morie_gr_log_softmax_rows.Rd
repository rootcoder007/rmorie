% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_gr_log_softmax_rows}
\alias{.morie_gr_log_softmax_rows}
\title{.morie_gr_log_softmax_rows}
\usage{
.morie_gr_log_softmax_rows(Z)
}
\arguments{
\item{Z}{A matrix; passed to \code{as.matrix}.}
}
\description{
A step of the geron_ml_native implementation. Called by \code{morie_geron_bert_nsp_loss}, \code{morie_geron_blip_itm_itc}, \code{morie_geron_classification_localization} and 9 others in the module. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.morie_gr_log_softmax_rows(V)
}
