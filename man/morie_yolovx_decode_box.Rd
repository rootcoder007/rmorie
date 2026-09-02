% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_yolovx_decode_box}
\alias{morie_yolovx_decode_box}
\title{Back to corners. Inverts encode_box exactly}
\usage{
morie_yolovx_decode_box(ltrb, cx, cy, stride = 1) 
}
\arguments{
\item{ltrb}{Passed to \code{.s03vec}.}
\item{cx}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{cy}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{stride}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{1}.}
}
\description{
A step of the yolovx_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A vector, from \code{c}.
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
morie_yolovx_decode_box(ltrb = c(1, 2, 3, 4, 5, 6, 7, 8), cx = c(1, 2, 3, 4, 5, 6, 7, 8), cy = c(1, 2, 3, 4, 5, 6, 7, 8))
}
