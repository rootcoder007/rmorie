% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{pate}
\alias{pate}
\title{pate}
\usage{
pate(teacher_predicts, queries, gamma = 0.05, delta = 1e-05, n_classes = NULL, 
    student_train_fn = NULL, student_features = NULL, seed = 0, lambdas = NULL) 
}
\arguments{
\item{teacher_predicts}{A vector; its length is taken.}
\item{queries}{Coerced to list by the body, with \code{as.list}.}
\item{gamma}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0.05}.}
\item{delta}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{1e-05}.}
\item{n_classes}{Passed to \code{teacher_votes}.}
\item{student_train_fn}{Optional; may be \code{NULL}. Passed to \code{is.null}.}
\item{student_features}{Optional; may be \code{NULL}. Coerced to list by the body, with \code{as.list}.}
\item{seed}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0}.}
\item{lambdas}{Passed to \code{moments_accountant}.}
}
\description{
A step of the pate_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{labels}, \code{clean_labels}, \code{votes}, \code{agreement}, \code{epsilon}, \code{epsilon_accountant}, \code{epsilon_data_independent}, \code{accountant}, \code{delta}, \code{gamma}, \code{n_teachers}, \code{n_queries}, \code{student}, \code{note}, \code{method}.
}
\keyword{internal}
