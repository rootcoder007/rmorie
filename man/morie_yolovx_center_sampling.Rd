% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_yolovx_center_sampling}
\alias{morie_yolovx_center_sampling}
\title{morie_yolovx_center_sampling}
\usage{
morie_yolovx_center_sampling(box, grid_w, grid_h, stride = 1, radius = 1.5)
}
\arguments{
\item{box}{See Usage.}
\item{grid_w}{See Usage.}
\item{grid_h}{See Usage.}
\item{stride}{Defaults to \code{1}.}
\item{radius}{Defaults to \code{1.5}.}
}
\description{
Part of the yolovx_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{in_box}, \code{in_center}, \code{candidates}, \code{n_candidates}, \code{single_center}, \code{note}.
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
morie_yolovx_center_sampling(box = c(1, 2, 3, 4, 5, 6, 7, 8), grid_w = c(1, 2, 3, 4, 5, 6, 7, 8), grid_h = c(1, 2, 3, 4, 5, 6, 7, 8))
}
