% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_yolovx_decoupled_head}
\alias{morie_yolovx_decoupled_head}
\title{morie_yolovx_decoupled_head}
\usage{
morie_yolovx_decoupled_head(channels, reduced = 256, n_classes = 80)
}
\arguments{
\item{channels}{Coerced to integer by the body, with \code{as.integer}.}
\item{reduced}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{256}.}
\item{n_classes}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{80}.}
}
\description{
A step of the yolovx_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{reduce_params}, \code{cls_params}, \code{reg_params}, \code{total}, \code{coupled_total}, \code{branches}, \code{extra_latency_ms}, \code{note}.
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
morie_yolovx_decoupled_head(channels = 5L)
}
