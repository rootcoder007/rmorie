# morie native arm -- laplc
#
# The Laplace mechanism M(D) = f(D) + Lap(sensitivity / epsilon) already
# ships as morie_dp_laplace_mechanism (Dwork & Roth 2014, Definition 3.3
# and Theorem 3.6). This module is an alias, exactly as the Python arm
# aliases dpglap.dp_laplace_mechanism, so that there is one mechanism
# and not two that can drift apart.
#
# Dwork, C., McSherry, F., Nissim, K. & Smith, A. (2006) "Calibrating
# noise to sensitivity in private data analysis", Theory of Cryptography
# (TCC 2006), LNCS 3876, 265-284.
# Dwork, C. & Roth, A. (2014) "The algorithmic foundations of
# differential privacy", Foundations and Trends in Theoretical Computer
# Science 9(3-4), 211-487, Definition 3.3, Theorem 3.6.

morie_laplc <- function(y, sensitivity = 1, epsilon = 1, ...) {
  morie_dp_laplace_mechanism(y, sensitivity = sensitivity,
                             epsilon = epsilon, ...)
}
