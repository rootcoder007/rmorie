% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{GaussOverlap}
\alias{GaussOverlap}
\title{NOT FROM THIS BOOK.  A full-text search of the 2024 third edition --}
\usage{
GaussOverlap(m1, m2, C1, C2) 
}
\arguments{
\item{m1}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{m2}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{C1}{A matrix; passed to \code{as.matrix}.}
\item{C2}{A matrix; passed to \code{as.matrix}.}
}
\description{
Rangayyan and Krishnan -- finds no occurrence of "Bhattacharyya", nor of Chernoff or Hellinger.  The book gives the KLD of eq (5.33) and the divergence of eqs (10.115)-(10.117), which is the symmetric sum of the two KLDs, and cites Swain for them.
}
\value{
A list with \code{bhattacharyya}, \code{mean_term}, \code{covariance_term}, \code{not_from_this_book}, \code{book_uses_divergence_eq_10_115}, \code{reference}, \code{method}.
}
\keyword{internal}
\examples{
CC1 <- matrix(c(2, 0.3, 0.3, 1), 2, 2)
CC2 <- matrix(c(1, -0.2, -0.2, 3), 2, 2)
GaussOverlap(c(0, 1), c(2, -1), CC1, CC2)
}
