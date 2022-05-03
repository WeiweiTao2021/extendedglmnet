random.lasso <- function(x, y, family = "guassian", cv, lambda = 0, nfolds = 5, B, q1, q2){
  n <- nrow(x)
  p <- ncol(x)

  if(q1<=1 | q1>p){
    stop(paste("q1 should greater than 1 and no greater than number of predictors in X", p,"."))
  }

  if(q2<=1 | q2>p){
    stop(paste("q2 should greater than 1 and no greater than number of predictors in X", p,"."))
  }

  if(is.null(colnames(x))){
    colnames(x) <- paste0("x", 1:p)
  }

  predictors <- colnames(x)

  betas <- matrix(0, B, p)
  colnames(betas) <- predictors

  ## Generate importance score
  for(i in 1:B){
    id.boot <- sample(1:n, n, replace = TRUE)
    cid.boot <- sample(1:p, q1, replace = FALSE)
    x.boot <- x[id.boot, cid.boot]
    y.boot <- y[id.boot]

    if(cv){
      if(length(lambda) > 1){
        cv_model <- cv.glmnet(x.boot, y.boot, alpha = 1, lambda = lambda, nfolds = nfolds, family = family)
      }else{
        cv_model <- cv.glmnet(x.boot, y.boot, alpha = 1, nfolds = nfolds, family = family)
      }
      best_lambda <- cv_model$lambda.min
    }else{
      if(is.null(lambda) | length(lambda) >1){
        stop("Need only one value of lambda for non-cross validation senarior!")
      }
      best_lambda <- lambda
    }

    best_model <- glmnet(x.boot, y.boot, alpha = 1, lambda = best_lambda, family = family)
    res <- coef(best_model)

    vars <- res@Dimnames[[1]][-1]
    beta <- as.vector(res)[-1]

    for(j in 1:length(vars)){
      betas[i, which(predictors == vars[j])] <- beta[j]
    }
  }
  importance <- abs(colMeans(betas))
  probs <- importance/sum(importance)

  ## calculate beta values
  predictors <- c("(Intercept)", colnames(x))

  betas <- matrix(0, B, p+1)
  colnames(betas) <- predictors
  for(i in 1:B){
    id.boot <- sample(1:n, n, replace = TRUE)
    cid.boot <- sample(1:p, q2, replace = FALSE, prob = probs)
    x.boot <- x[id.boot, cid.boot]
    y.boot <- y[id.boot]

    if(cv){
      if(length(lambda) > 1){
        cv_model <- cv.glmnet(x.boot, y.boot, alpha = 1, lambda = lambda, nfolds = nfolds, family = family)
      }else{
        cv_model <- cv.glmnet(x.boot, y.boot, alpha = 1, nfolds = nfolds, family = family)
      }
      best_lambda <- cv_model$lambda.min
    }else{
      if(is.null(lambda) | length(lambda) >1){
        stop("Need only one value of lambda for non-cross validation senarior!")
      }
      best_lambda <- lambda
    }

    best_model <- glmnet(x.boot, y.boot, alpha = 1, lambda = best_lambda,  family = family)
    res <- coef(best_model)

    vars <- res@Dimnames[[1]]
    beta <- as.vector(res)

    for(j in 1:length(vars)){
      betas[i, which(predictors == vars[j])] <- beta[j]
    }
  }

  beta.final <- colMeans(betas)

  return(list(type = "random lasso", family = family, coef = beta.final,
              lambda = best_lambda, df = sum(abs(beta.final[!is.na(beta.final)])>0) - 1,
              nobs = n, dim = p))
}

