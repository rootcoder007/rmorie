% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_yolovx_box_iou}
\alias{morie_yolovx_box_iou}
\title{Intersection over union of two corner boxes}
\usage{
morie_yolovx_box_iou(a, b)
}
\arguments{
\item{a}{A vector; indexed elementwise.}
\item{b}{A vector; indexed elementwise.}
}
\description{
A step of the yolovx_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
One of two values, depending on the branch taken.
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
morie_yolovx_box_iou(V, V)
}
