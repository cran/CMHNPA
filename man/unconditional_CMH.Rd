% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/func_unconditional_CMH.R
\name{unconditional_CMH}
\alias{unconditional_CMH}
\title{Unconditional CMH Test}
\usage{
unconditional_CMH(
  treatment,
  response,
  strata = NULL,
  U = NULL,
  V = NULL,
  a_ij = NULL,
  b_hj = NULL,
  test_OPA = TRUE,
  test_GA = TRUE,
  test_M = TRUE,
  test_C = TRUE
)
}
\arguments{
\item{treatment}{a factor vector representing the treatment applied.}

\item{response}{a factor vector giving the response category for the
corresponding elements of \code{treatment}.}

\item{strata}{a factor vector giving the strata or block for the
corresponding elements of \code{treatment} and \code{response}.}

\item{U}{The degree of assessment relating to the treatment.}

\item{V}{The degree of assessment relating to the response.}

\item{a_ij}{a t x b matrix of treatment scores. The matrix allows for
different scores to be used over different strata. If a t x 1 vector of
scores is provided, it is assumed that the scores are the same across strata
and a warning provided.}

\item{b_hj}{a c x b matrix of response scores. The matrix allows for
different scores to be used over different strata. If a c x 1 vector of
scores is provided, it is assumed that the scores are the same across strata
and a warning provided.}

\item{test_OPA}{TRUE or FALSE flag to include the calculation of the OPA
test values.}

\item{test_GA}{TRUE or FALSE flag to include the calculation of the GA
test values.}

\item{test_M}{TRUE or FALSE flag to include the calculation of the MS
test values. If response scores are not included, this test will not be
performed.}

\item{test_C}{TRUE or FALSE flag to include the calculation of the C
test values. If response scores and treatment scores are not included, this
test will not be performed.}
}
\value{
The unconditional CMH test results.
}
\description{
\code{unconditional_CMH} returns the test statistics and p-values for the
equivalent unconditional CMH tests.
}
\examples{
attach(job_satisfaction)
a_ij = matrix(rep(c(3,10,20,35),2),nrow=4)
b_hj = matrix(rep(c(1,3,4,5),2),nrow=4)
unconditional_CMH(treatment = income, response = satisfaction,
strata = gender, U = 2, V = 2, a_ij = a_ij, b_hj = b_hj)
}
\references{
Rayner, J.C.W and Livingston, G. C. (2022). An Introduction to Cochran-Mantel-Haenszel Testing and Nonparametric ANOVA. Wiley.
}
