ridge <- function(x, y, alpha = 0, family = "gaussian", cv = TRUE, lambda = 0, nfolds = 5){
  n <- nrow(x)
  p <- ncol(x)

  if(is.null(colnames(x))){
    colnames(x) <- paste0("x", 1:p)
  }

  if(cv){
    if(length(lambda) > 1){
      cv_model <- cv.glmnet(x, y, alpha = alpha, lambda = lambda, nfolds = nfolds, family = family)
    }else{
      cv_model <- cv.glmnet(x, y, alpha = alpha, nfolds = nfolds, family = family)
    }
    best_lambda <- cv_model$lambda.min
  }else{
    if(is.null(lambda) | length(lambda) >1){
      stop("Need only one value of lambda for non-cross validation senarior!")
    }
    best_lambda <- lambda
  }

  best_model <- glmnet(x, y, alpha = alpha, lambda = best_lambda, family = family)
  beta.final <- coef(best_model)

  type <- ifelse(alpha == 0, "ridge", "lasso" )
  return(list(type = type, family = family, coef = beta.final,
         lambda = best_lambda, df = sum(abs(beta.final[!is.na(beta.final)])>0) - 1,
         nobs = n, dim = p))
}



