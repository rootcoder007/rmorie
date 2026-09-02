% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_svm_decision_cpp}
\alias{morie_svm_decision_cpp}
\title{Decision values for new data given fitted SVM coefficients (compiled)}
\usage{
morie_svm_decision_cpp(SV, coef, rho, Xnew, kernel_type, gamma, coef0, degree) 
}
\description{
Decision values for new data given fitted SVM coefficients (compiled)
}
\arguments{
\item{SV}{Argument \code{SV}; see Usage.}
\item{coef}{Argument \code{coef}; see Usage.}
\item{rho}{Argument \code{rho}; see Usage.}
\item{Xnew}{Argument \code{Xnew}; see Usage.}
\item{kernel_type}{Argument \code{kernel_type}; see Usage.}
\item{gamma}{Argument \code{gamma}; see Usage.}
\item{coef0}{Argument \code{coef0}; see Usage.}
\item{degree}{Argument \code{degree}; see Usage.}
}
\value{
The value of \code{.Call}.
}
\keyword{internal}
