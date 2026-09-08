% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_yolovx_simota_assign}
\alias{morie_yolovx_simota_assign}
\alias{morie_yolovx}
\title{Dynamic top-k, an approximation to optimal transport. k_g is the}
\usage{
morie_yolovx_simota_assign(costs, ious, top_q = 10, max_k = NULL) 

morie_yolovx(costs, ious, top_q = 10, max_k = NULL) 
}
\arguments{
\item{costs}{Passed to \code{.s03mat}.}
\item{ious}{Passed to \code{.s03mat}.}
\item{top_q}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{10}.}
\item{max_k}{Optional; may be \code{NULL}. Coerced to integer by the body, with \code{as.integer}.}
}
\description{
rounded sum of the q largest IoUs for that ground truth. Returns the assignment keyed by 0-based ground-truth index.
}
\value{
A list with \code{estimate}, \code{assignment}, \code{dynamic_k}, \code{n_positives}, \code{contested}, \code{method}, \code{note}.
}
\references{
----------
Ge, Z., Liu, S., Wang, F., Li, Z. & Sun, J. (2021) "YOLOX: Exceeding
YOLO Series in 2021", arXiv:2107.08430. Sec. 2.

Ge, Z., Liu, S., Li, Z., Yoshie, O. & Sun, J. (2021) "OTA: Optimal
Transport Assignment for Object Detection", CVPR 2021, 303-312,
arXiv:2103.14259.

Tian, Z., Shen, C., Chen, H. & He, T. (2019) "FCOS: Fully
Convolutional One-Stage Object Detection", ICCV 2019, 9627-9636,
arXiv:1904.01355.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
morie_yolovx_simota_assign(V, V)
}
