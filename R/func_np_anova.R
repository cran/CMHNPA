#' np_anova function
#'
#' \code{np_anova} performs a nonparametric ANOVA.
#'
#' @details
#' Nonparametric ANOVA is a methodology that is applicable where one as an
#' ordered response variable as well as both ordered response and predictor
#' variables.
#'
#'
#' @param ordered_vars a data frame for the ordered variables being assessed.
#' @param predictor_vars a data frame for the un-ordered variables being
#' assessed.
#' @param uvw the degree being assessed. This should be a vector with length
#' equal the number of elements in the \code{ordered_vars} data frame.
#'
#' @return
#' Where there is only one ordered variable, the function returns a type III
#' ANOVA table to test for differences of order \code{uvw} across the levels of
#' the \code{predictor_vars}.
#'
#' Where there is more than one ordered variable, the function returns a type
#' III ANOVA table to test for differences in generalised correlations of order
#' \code{uvw} across the levels of the \code{predictor_vars}.
#'
#' @references
#' Rayner, J.C.W and Livingston, G. C. (2022). An Introduction to Cochran-Mantel-Haenszel Testing and Nonparametric ANOVA. Wiley.
#'
#' @export
#' @importFrom car Anova
#' @examples
#' attach(jam)
#' np_anova(ordered_vars = sweetness, predictor_vars = data.frame(type,judge), uvw = 1)

np_anova = function(ordered_vars, predictor_vars, uvw){

  ordered_vars = data.frame(ordered_vars)

  if(!is.data.frame(ordered_vars) | !is.data.frame(predictor_vars)) {
    stop("ordered_vars and predictor_vars should be data frames")
  }

  n = dim(ordered_vars)[1]
  n_vars_response = dim(ordered_vars)[2]


  if(n_vars_response>3) stop("This function is not coded for greater than trivariate correlations at the moment")

  if(!is.vector(uvw)) stop("uvw should be a vector of degrees of correlation that correspond to the columns of ordered_vars")

  if(length(uvw) != n_vars_response) stop("uvw should be a vector of length equal to the number of columns in ordered_vars")


  ortho_poly_response = rep(1,n)

  for (i in 1:n_vars_response){
    ortho_poly_response = ortho_poly_response*poly(x = as.numeric(ordered_vars[,i]),degree = uvw[i])[,uvw[i]]
  }

  ret = Anova(lm(cbind(ortho_poly_response,predictor_vars)),type = 3)

  return(ret)


}


