#' Friedman Test
#'
#' \code{friedman} returns the test statistic and p-value for the Friedman test.
#'
#' @param y a numeric vector for the response variable.
#' @param groups a vector giving the group for the corresponding elements of
#' \code{y}.
#' @param blocks a vector giving the block for the corresponding elements of
#' \code{y}.
#' @param components a TRUE or FALSE flag to indicate if chi-squared or F
#' orthogonal component p-values are wanted.
#'
#' @return The Friedman test results statistic adjusted for ties together with the
#' associated p-value.
#'
#' @references
#' Rayner, J.C.W and Livingston, G. C. (2022). An Introduction to Cochran-Mantel-Haenszel Testing and Nonparametric ANOVA. Wiley.
#'
#' @export
#' @examples
#' attach(jam)
#' friedman(y = sweetness_ranks, groups = type, blocks = judge,
#' components = TRUE)

friedman = function(y,groups,blocks,components=FALSE){

  if(length(y) != length(groups) | length(y) != length(blocks) | length(blocks) != length(groups)){
    stop("Supplied vectors y, groups, and blocks should be the same lengths.")
  }

  groups_name = deparse(substitute(groups))
  response_name = deparse(substitute(y))
  blocks_name = deparse(substitute(blocks))
  groups_levels = levels(groups)

  t = length(levels(groups))
  b = length(levels(blocks))

  y = as.numeric(y)

  if(length(y) != t*b){
    stop("Number of treatments multiplied by the number of blocks is not the length of y. Not a RBD. Perhaps the data is a BIBD, if so, use Durbins test.")
  }


  rank_matrix = t(apply(X = matrix(unlist(split(y, blocks)), ncol = t, byrow = TRUE),1,rank))

  R_i. = colSums(rank_matrix)
  tsigma2 = sum(rank_matrix^2) - b*t*(t+1)^2/4
  Fdm_A = (t-1)*( sum(R_i.^2) - b^2*t*(t+1)^2/4 )/tsigma2
  df = t-1
  p.val = pchisq(q = Fdm_A, df = df, lower.tail = FALSE)

  chi_p_val_linear=NA
  chi_p_val_quad=NA
  chi_p_val_remain=NA
  F_p_val_overall=NA
  F_p_val_linear=NA
  F_p_val_quad=NA
  F_p_val_remain=NA

  chi_cont_linear=NA
  chi_cont_quad=NA
  chi_cont_remain=NA
  F_cont_overall=NA
  F_cont_linear=NA
  F_cont_quad=NA
  F_cont_remain=NA

  if(components==T){
    #
    # Contrast calculations
    #

    c_i = matrix(nrow=t,ncol=t)
    c_i[,1:(t-1)] = poly(1:t,t-1)[,]
    c_i[,t] = 1/sqrt(t)
    M = t(c_i)

    # chi-square
    Y_i = (R_i. - b*(t+1)/2)*sqrt((t-1)/tsigma2)

    chi_cont_linear = (M[1,]%*%Y_i)^2
    chi_cont_quad = (M[2,]%*%Y_i)^2
    chi_cont_remain = t(Y_i) %*% t(M)%*%M %*% Y_i - chi_cont_linear - chi_cont_quad
    if( chi_cont_remain < 0 ) {chi_cont_remain = 0}

    chi_p_val_linear = pchisq(q = chi_cont_linear,df = 1,lower.tail = F)
    chi_p_val_quad = pchisq(q = chi_cont_quad,df = 1,lower.tail = F)
    chi_p_val_remain = pchisq(q = chi_cont_remain,df = t-3,lower.tail = F)


    # F
    Z_i = (R_i. - b*(t+1)/2)/sqrt( b )

    MSerror = (anova(lm(y~groups+blocks))$'Mean Sq')[length(anova(lm(y~groups+blocks))$'Mean Sq')]
    edf = (anova(lm(y~groups+blocks))$'Df')[length(anova(lm(y~groups+blocks))$'Df')]

    F_cont_linear = (t(M[1,])%*%Z_i)^2/MSerror
    F_cont_quad = (t(M[2,])%*%Z_i)^2/MSerror

    if(t <= 3L) {F_cont_remain = 0L} else {
      F_cont_remain = ((t(Z_i)%*%Z_i - (t(M[1,])%*%Z_i)^2 - (t(M[2,])%*%Z_i)^2)/(t-3))/MSerror
    }

    if(F_cont_remain < 0L) {F_cont_remain = 0L}
    F_cont_overall = (t(Z_i)%*%(Z_i)/(t-1))/MSerror

    F_p_val_overall = pf(q = F_cont_overall,df1 = t-1,df2 = edf,lower.tail = F)
    F_p_val_linear = pf(q = F_cont_linear,df1 = 1,df2 = edf,lower.tail = F)
    F_p_val_quad = pf(q = F_cont_quad,df1 = 1,df2 = edf,lower.tail = F)
    if(t<=3) {F_p_val_remain = 1} else {
      F_p_val_remain = pf(q = F_cont_remain,df1 = t-3,df2 = edf,lower.tail = F)
    }


  }

  ret = list(Fdm_A = Fdm_A,p_value = p.val,df = df,  groups_name = groups_name,
             response_name=response_name,blocks_name=blocks_name,R_i.=R_i.,
             groups_levels=groups_levels,chi_p_val_linear=chi_p_val_linear,
             chi_p_val_quad=chi_p_val_quad,chi_p_val_remain=chi_p_val_remain,
             F_p_val_overall=F_p_val_overall,F_p_val_linear=F_p_val_linear,
             F_p_val_quad=F_p_val_quad,F_p_val_remain=F_p_val_remain,
             components=components,chi_cont_linear=chi_cont_linear,
             chi_cont_quad=chi_cont_quad,chi_cont_remain=chi_cont_remain,
             F_cont_linear=F_cont_linear,F_cont_quad=F_cont_quad,
             F_cont_remain=F_cont_remain,F_cont_overall=F_cont_overall)

  new("friedman_test", ret)

}
