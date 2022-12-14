% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/func_gen_cor.R
\name{gen_cor}
\alias{gen_cor}
\title{gen_cor Test}
\usage{
gen_cor(
  x,
  y,
  z = NULL,
  U,
  V,
  W = NULL,
  x_scores = NULL,
  y_scores = NULL,
  z_scores = NULL,
  n_perms = 0,
  perms_info = FALSE,
  rounding = 4
)
}
\arguments{
\item{x}{a numeric vector or factor, commonly a response variable.}

\item{y}{a numeric vector or factor, commonly a treatment variable.}

\item{z}{an optional numeric vector or factor, commonly a block variable.}

\item{U}{the maximum degree of correlation relating to the variable \code{x}.}

\item{V}{the maximum degree of correlation relating to the variable \code{y}.}

\item{W}{the maximum degree of correlation relating to the variable \code{z}.
Required when \code{z} is included.}

\item{x_scores}{optional scores related to the variable \code{x}.}

\item{y_scores}{optional scores related to the variable \code{y}.}

\item{z_scores}{optional scores related to the variable \code{z}.}

\item{n_perms}{an optional numeric value indicating the number of
permutations required.}

\item{perms_info}{a TRUE of FALSE flag to indicate whether information
regarding the progress on the number of permutations should be printed.}

\item{rounding}{the number of decimal places the output should be rounded to.
The default is 4.}
}
\value{
This function calculates the generalised correlations for up to three input
variables.
}
\description{
\code{gen_cor} returns the generalised correlations and associated p-values
together with tests of normality.
}
\details{
This function calculates up to three way generalised correlations. The
function calculates three tests by default to test if the correlations are
statistically significantly different from 0 with an option to run permuation
testing.
}
\examples{
attach(intelligence)
gen_cor(x = rank(score), y = age, U = 2, V = 2)
}
\references{
Rayner, J.C.W and Livingston, G. C. (2022). An Introduction to Cochran-Mantel-Haenszel Testing and Nonparametric ANOVA. Wiley.
}
