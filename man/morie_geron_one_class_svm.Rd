% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_geron_one_class_svm}
\alias{morie_geron_one_class_svm}
\title{One-class SVM (SMO) over an RBF kernel: boundary of the high-density region (Geron Ch 8, hmocsv)}
\usage{
morie_geron_one_class_svm(X, nu = 0.5, gamma = 1, max_iter = 2000, tol = 1e-09) 
}
\arguments{
\item{X}{Data (n, d).}
\item{nu}{Outlier-fraction knob in (0, 1\].}
\item{gamma}{RBF width.}
\item{max_iter,tol}{SMO controls.}
}
\description{
One-class SVM (SMO) over an RBF kernel: boundary of the high-density region (Geron Ch 8, hmocsv)
}
\value{
A list with \code{alpha}, \code{rho}, \code{decision}, \code{is_outlier}, \code{support_vectors}, \code{outlier_fraction}, \code{decision_function}, \code{n_iter}, \code{C}, \code{estimate}, \code{n}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
morie_geron_one_class_svm(V)
}
