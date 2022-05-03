regression <- function(x, y, family = "gaussian"){
  n <- nrow(x)
  p <- ncol(x)

  if(is.null(colnames(x))){
    colnames(x) <- paste0("x", 1:p)
  }

  dat <- as.data.frame(cbind(y, x))

  if(family == "gaussian"){
    fit = lm(y ~ ., data = dat)
    print(fit)
  }else if(family == "binomial"){
    dat$y <- as.factor(dat$y)
    fit <- glm(y ~ ., data = dat, family = "binomial")
  }

  beta.final <- coef(fit)

  type <- ifelse(family == "gaussian", "linear regression", "logistic regression")
  return(list(type = type, family = family, coef = beta.final,
         lambda = 0, df = sum(abs(beta.final[!is.na(beta.final)])>0) - 1,
         nobs = n, dim = p))
}



