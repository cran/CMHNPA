#' Durbin Test
#'
#' \code{durbin} returns the results of Durbin's Rank Sum test.
#'
#' @details
#' The test is a generalisation of Friedman's test that can be applied to BIBD.
#'
#'
#' @param y a numeric vector for the response variable.
#' @param groups a vector giving the group for the corresponding elements of
#' \code{y}.
#' @param blocks a vector giving the block for the corresponding elements of
#' \code{y}.
#' @param components a TRUE or FALSE flag to indicate if chi-squared or F
#' orthogonal component p-values are wanted.
#'
#' @return The Durbin test adjusted for ties results.
#'
#' @references
#' Rayner, J.C.W and Livingston, G. C. (2022). An Introduction to Cochran-Mantel-Haenszel Testing and Nonparametric ANOVA. Wiley.
#'
#' @importFrom stats anova lm pf poly
#' @export
#' @examples
#' attach(icecream)
#' durbin(y = rank, groups = variety, blocks = judge, components = TRUE)
durbin = function(y,groups,blocks,components=FALSE){

  if(length(y) != length(groups) | length(y) != length(blocks) | length(blocks) != length(groups)){
    stop("Supplied vectors y, groups, and blocks should be the same lengths.")
  }

  if(!is.factor(groups) | !is.factor(blocks)) stop("Both groups and blocks need to be factors")

  groups_name = deparse(substitute(groups))
  response_name = deparse(substitute(y))
  blocks_name = deparse(substitute(blocks))
  groups_levels = levels(groups)

  t = length(unique(groups))
  b = length(unique(blocks))

  if(mean(diff(table(blocks))) != 0){
    stop("Design is not balanced")
  } else {
    k = unname(table(blocks)[1])
  }
  if(mean(diff(table(groups))) != 0){
    stop("Design is not balanced")
  } else {
    r = unname(table(groups)[1])
  }

  y = as.numeric(y)

  rank_matrix = t(apply(matrix(unlist(split(y,blocks)),ncol = k,byrow=T),1,rank))
  design_mat = table(blocks,groups)

  rank_design = matrix(0,nrow = b,ncol=t)

  for (i in 1:b){
    rank_design[i,design_mat[i,]==1] = rank_matrix[i,]
  }

  R_i. = colSums(rank_design)

  D_A = (t-1)*( sum(R_i.^2) - r*b*k*(k+1)^2/4 )/(
    sum(rank_design^2) - b*k*(k+1)^2/4
  )
  df = t-1
  p_val = pchisq(q = D_A, df = df, lower.tail = FALSE)

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
    c = (t-1) / ( - b*k*(k+1)^2/4 + sum(rank_design^2) )
    Y_i = (R_i. - r*(k+1)/2)*sqrt(c)

    chi_cont_linear = (M[1,]%*%Y_i)^2
    chi_cont_quad = (M[2,]%*%Y_i)^2
    chi_cont_remain = t(Y_i) %*% t(M)%*%M %*% Y_i - chi_cont_linear - chi_cont_quad
    if(chi_cont_remain < 0) {chi_cont_remain = 0}

    chi_p_val_linear = pchisq(q = chi_cont_linear,df = 1,lower.tail = F)
    chi_p_val_quad = pchisq(q = chi_cont_quad,df = 1,lower.tail = F)
    chi_p_val_remain = pchisq(q = chi_cont_remain,df = t-3,lower.tail = F)


    # F
    Z_i = (R_i. - r*(k+1)/2)*sqrt( k/(t*r*(k-1)/(t-1)) )

    MSerror = (anova(lm(y~groups+blocks))$'Mean Sq')[length(anova(lm(y~groups+blocks))$'Mean Sq')]
    edf = (anova(lm(y~groups+blocks))$'Df')[length(anova(lm(y~groups+blocks))$'Df')]

    F_cont_linear = (t(M[1,])%*%Z_i)^2/MSerror
    F_cont_quad = (t(M[2,])%*%Z_i)^2/MSerror
    F_cont_remain = ((t(Z_i)%*%Z_i - (t(M[1,])%*%Z_i)^2 - (t(M[2,])%*%Z_i)^2)/(t-3))/MSerror
    if(F_cont_remain < 0) F_cont_remain = 0
    F_cont_overall = (t(Z_i)%*%(Z_i)/(t-1))/MSerror

    F_p_val_overall = pf(q = F_cont_overall,df1 = t-1,df2 = edf,lower.tail = F)
    F_p_val_linear = pf(q = F_cont_linear,df1 = 1,df2 = edf,lower.tail = F)
    F_p_val_quad = pf(q = F_cont_quad,df1 = 1,df2 = edf,lower.tail = F)
    if(t<=3) {F_p_val_remain = 1} else {
      F_p_val_remain = pf(q = F_cont_remain,df1 = t-3,df2 = edf,lower.tail = F)
    }

  }

  ret = list(D_A = D_A,p_value = p_val,df = df,  groups_name = groups_name,
             response_name=response_name,blocks_name=blocks_name,R_i.=R_i.,
             groups_levels=groups_levels,chi_p_val_linear=chi_p_val_linear,
             chi_p_val_quad=chi_p_val_quad,chi_p_val_remain=chi_p_val_remain,
             F_p_val_overall=F_p_val_overall,F_p_val_linear=F_p_val_linear,
             F_p_val_quad=F_p_val_quad,F_p_val_remain=F_p_val_remain,
             components=components,chi_cont_linear=chi_cont_linear,
             chi_cont_quad=chi_cont_quad,chi_cont_remain=chi_cont_remain,
             F_cont_linear=F_cont_linear,F_cont_quad=F_cont_quad,
             F_cont_remain=F_cont_remain,F_cont_overall=F_cont_overall)



  new("durbin_test", ret)

}


