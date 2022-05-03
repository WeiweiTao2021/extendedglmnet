#' an extension to glmnet package with options of random lasso method
#'
#' This package is an extension of glmnet package. Users can either fit a linear model or generalized linear model depending the response variable types.
#' It can support ridge, lasso and random lasso methods. User can choose to use cross validation to find the optimal regularization parameter lambda or use user specified lambda.
#' Can deal with both continuous and binary responses.
#'
#' Lasso is known to be suffering from that it can select only one or a few of a set of highly correlated important variables.
#' If several independent data sets were generated from the same distribution, then we would expect lasso to select nonidentical
#' subsets of those highly correlated important variables from different data sets. Random Lasso proposed by Sijian Wang et al. solved this issue
#' by using bootstrap, which will provide more stable results than lasso and ridge when number of predictors are greater than number of observations.
#'
#' @param x input matrix, of dimension nobs x nvars; each row is an observation
#' vector.
#' @param y y response variable. For \code{family="gaussian"}, y can be any real values. For \code{family="binomial"}
#' should be a factor with two levels.
#' @param family a character string representing one of the built-in families gaussian or binomial.
#' @param type a character string representing one of the built-in type of regression methods regression, ridge, lasso or random lasso.
#' @param cv an indicator variable to specify whether cross validation should be used to pick optimal lambda value. Default is TRUE.
#' @param lambda A user supplied \code{lambda} sequence. Typical usage is to have the program compute its own \code{lambda} sequence. Supplying a value of
#' \code{lambda} overrides this.
#' @param nfolds number of folds - default is 5. Although nfolds can be as large as the sample size (leave-one-out CV), it is not recommended for large datasets. Smallest value allowable is nfolds=3
#' @param B number of bootstrap samples in random lasso. One can take B = 500 or B = 1000. The default is 200.
#' @param q1 number of candidate variables to be included in each bootstrap sample during generating importance measures in random lasso. This value need to be tuned to ensure best performance.
#' The value should be positive and no greater than number of predictors in x.
#' @param q2 number of candidate variables to be included in each bootstrap sample during variable selection in random lasso. This value need to be tuned to ensure best performance.
#' The value should be positive and no greater than number of predictors in x.
#'
#' @return a list containing fitting results.
#' \item{type}{the type of regression performed based upon user specified \code{type}.}
#' \item{family}{the regression family based upon response variable type.}
#' \item{coef}{a list of estimated coefficient.}
#' \item{lambda}{the actual sequence of \code{lambda} values used. It is always 0 for regression models without regulation.}
#' \item{df}{number of nonzero coefficients for each value of \code{lambda}.}
#' \item{nobs}{number of observations.}
#' \item{dim}{dimension of coefficient matrix.}
#'
#' @author Weiwei Tao \email{tww101928@@gmail.com}
#' @seealso \code{\link{print.extendglmnet}}, \code{\link{predict.extendglmnet}} and \code{\link{evaluation}} methods.
#'
#' @references Wang, S., Nan, B., Rosset, S., & Zhu, J. (2011).
#' \emph{Random lasso,  The annals of applied statistics, 5(1), 468.}\cr
#'Friedman J, Hastie T, Tibshirani R (2010).
#'\emph{Regularization Paths for Generalized Linear Models via Coordinate Descent.
#'Journal of Statistical Software, 33(1), 1–22. doi: 10.18637/jss.v033.i01, https://www.jstatsoft.org/v33/i01/.}
#'
#' @keywords models regression
#'
#' @examples
#' # a binary response example
#' size = 200
#' x = matrix(rnorm(210*size),ncol=210)
#' z = 1 + 2*x[,1] + 3*x[,40] - 4*x[,50] + 3*x[,210]
#' pr = 1/(1+exp(-z))
#' y = rbinom(size,1,pr)
#'
#' index = sample(1:size, 0.7*size)
#' trainx = x[index,]
#' testx = x[-index,]
#' trainy = y[index]
#' testy = y[-index]
#'
#' # logistic regression
#' fit.glm <- extendedglmnet(trainx, trainy, family = "binomial", type = "regression")
#' print.extendglmnet(fit.glm)
#' ypred <- predict.extendglmnet(fit.glm, testx, family = "binomial", type="class")
#' evaluation(testy, ypred, "accuracy")
#'
#' # lasso
#' fit.lasso <- extendedglmnet(trainx, trainy, family = "binomial", type = "lasso")
#' print.extendglmnet(fit.lasso)
#' ypred <- predict.extendglmnet(fit.lasso, testx, family = "binomial", type="class")
#' evaluation(testy, ypred, "accuracy")
#'
#' # ridge
#' fit.ridge <- extendedglmnet(trainx, trainy, family = "binomial", type = "lasso")
#' print.extendglmnet(fit.ridge)
#' ypred <- predict.extendglmnet(fit.ridge, testx, family = "binomial", type="class")
#' evaluation(testy, ypred, "accuracy")
#'
#' # random lasso
#' fit.rl <- extendedglmnet(trainx, trainy, family = "binomial", type = "random lasso", q1 = 150, q2 = 150)
#' print.extendglmnet(fit.rl)
#' ypred <- predict.extendglmnet(fit.rl, testx, family = "binomial", type="class")
#' evaluation(testy, ypred, "accuracy")
#'
#' # continuous response example
#' size = 30
#' x = matrix(runif(35*size),ncol=35)
#' y = 10*x[,1] + 3*x[,2] + 20*(x[,3]-0.5)**2 + 10*x[,4] + 5*x[,35] + runif(1,0,1)
#'
#' index = sample(1:size, 0.7*size)
#' trainx = x[index,]
#' testx = x[-index,]
#' trainy = y[index]
#' testy = y[-index]
#'
#' fit.lr<- extendedglmnet(trainx, trainy, family = "gaussian", type = "regression")
#' print.extendglmnet(fit.lr)
#' ypred <- predict.extendglmnet(fit.lr, testx, family = "gaussian")
#' evaluation(testy, ypred, "MSE")
#'
#' fit.lasso <- extendedglmnet(trainx, trainy, family = "gaussian", type = "lasso")
#' print.extendglmnet(fit.lasso)
#' ypred <- predict.extendglmnet(fit.lasso, testx, family = "gaussian")
#' evaluation(testy, ypred, "MSE")
#'
#' fit.ridge <- extendedglmnet(trainx, trainy, family = "gaussian", type = "ridge")
#' print.extendglmnet(fit.ridge)
#' ypred <- predict.extendglmnet(fit.ridge, testx, family = "gaussian")
#' evaluation(testy, ypred, "MSE")
#'
#' fit.rl <- extendedglmnet(trainx, trainy, family = "gaussian", type = "random lasso")
#' print.extendglmnet(fit.rl)
#' ypred <- predict.extendglmnet(fit.rl, testx, family = "gaussian")
#' evaluation(testy, ypred, "MSE")
#'
#' @import glmnet
#' @importFrom stats lm coef glm
#' @export

extendedglmnet <- function(x, y, family = c("gaussian", "binomial"),
                           type = c("regression", "ridge", "lasso", "random lasso"),
                           cv = TRUE, lambda = 0, nfolds = 5,
                           B = 200, q1 = 5, q2 = 5){

  ##check dims
  np=dim(x)
  if(is.null(np)|(np[2]<=1))
    stop("x should be a matrix with 2 or more columns")

  nobs=as.integer(np[1])
  nvars=as.integer(np[2])

  dimy=dim(y)
  nrowy=ifelse(is.null(dimy),length(y),dimy[1])
  if(nrowy!=nobs)
    stop(paste("number of observations in y (",nrowy,") not equal to the number of rows of x (",nobs,")",sep=""))

  if(is.null(colnames(x))){
    colnames(x) <- paste0("x", 1:nvars)
  }

  if(!(type %in% c("regression", "ridge", "lasso", "random lasso")))
      stop("type can only be regression, ridge, lasso or random lasso'")

  if(family == "gaussian"){
    fit=switch(type,
               "regression"=regression(x, y, family = "gaussian"),
               "ridge"=ridge(x, y, 0, family = "gaussian", cv, lambda, nfolds),
               "lasso"=ridge(x, y, 1, family = "gaussian", cv, lambda, nfolds),
               "random lasso"=random.lasso(x, y, family = "gaussian", cv, lambda, nfolds, B, q1, q2),
    )
  }else if(family == "binomial"){
    # check the response variable
    y <- as.factor(y)
    ntab <- table(y)
    minclass = min(ntab)
    if(minclass<=1)
      stop("one binomial class has 1 or 0 observations; not allowed")
    if(minclass<8)
      warning("one binomial class has fewer than 8  observations; dangerous ground")

    fit=switch(type,
               "regression"=regression(x, y, family = "binomial"),
               "ridge"=ridge(x, y, 0, family = "binomial", cv, lambda, nfolds),
               "lasso"=ridge(x, y, 1, family = "binomial", cv, lambda, nfolds),
               "random lasso"=random.lasso(x, y, family = "binomial", cv, lambda, nfolds, B, q1, q2)
    )

  }else{
    stop("family should be gaussian or binomial")
  }

  return(fit)
}


