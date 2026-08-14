# morie native arm -- locdp
#
# Local differential privacy: each respondent randomises before the
# collector sees anything. The canonical local mechanism -- and the one
# Kasiviswanathan et al. build on -- is Warner randomised response,
# which already ships as Rrand with the flip probability
# 1 / (1 + e^epsilon) that achieves epsilon-local-DP. This module
# aliases it, matching the Python arm's alias of
# rrand.randomized_response, rather than adding a second mechanism.
#
# Warner, S. L. (1965) "Randomized response: a survey technique for
# eliminating evasive answer bias", Journal of the American Statistical
# Association 60(309), 63-69.
# Kasiviswanathan, S. P., Lee, H. K., Nissim, K., Raskhodnikova, S. &
# Smith, A. (2011) "What can we learn privately?", SIAM Journal on
# Computing 40(3), 793-826, section 1.
# Dwork, C. & Roth, A. (2014) Foundations and Trends in Theoretical
# Computer Science 9(3-4), 211-487, section 3.2.

morie_locdp <- function(bit, epsilon = 1) {
  Rrand(bit, epsilon = epsilon)
}
