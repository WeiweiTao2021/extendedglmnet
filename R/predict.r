#' make predictions from an extendedglmnet object.
#'
#' Similar to other predict methods, this functions predicts fitted values and classes
#' (for binary endpoint only) from a fitted \code{"extendedglmnet"} object.
#'
#' @aliases extendedglmnet.predict
#'
#' @param fit Fitted \code{"extendedglmnet"} model object.
#' @param newx Matrix of new values for \code{x} at which predictions are to be
#' made. Must be a matrix.
#' @param type Type of prediction required. For continuous y's, only predicted responses will be returned.
#' Type \code{"link"} gives the linear predictors for \code{"binomial"} models.
#' Type \code{"response"} gives the fitted probabilities for \code{"binomial"} models.
#' Type \code{"class"} applies only to \code{"binomial"} models, and produces the class label corresponding to
#' the maximum probability.
#' @param family can be binomial or gaussian depending on y types.
#' @param levels specify the binary response variable levels of factors.
#'
#' @return fitted values.
#'
#' @author Weiwei Tao \email{tww101928@@gmail.com}
#'
#' @seealso \code{\link{extendedglmnet}}, and \code{\link{print.extendglmnet}}, and \code{\link{evaluation}} methods.
#'
#' @examples
#'
#' size = 200
#' x = matrix(rnorm(210*size),ncol=210)
#' z = 1 + 2*x[,1] + 3*x[,40] - 4*x[,50] + 3*x[,210]
#' pr = 1/(1+exp(-z))
#' y = rbinom(size,1,pr)
#'
#' fit.rl <- extendedglmnet(x, y, family = "binomial", type = "random lasso", q1 = 150, q2 = 150)
#' ypred.rl <- predict.extendglmnet(fit.rl, x, type="class", family = "binomial")
#'
#' @method extendedglmnet predict
#' @export predict.extendglmnet
#'
predict.extendglmnet <- function(object, newx,
                                  type=c("link","class","response"),
                                  family = "gaussian",
                                  levels = c(0, 1)){


  beta <- object$coef

  if(!(family %in% c("gaussian", "binomial")))
    stop("family can only be gaussian, or binomial.")


  dx=dim(newx)
  p = length(beta)-1
  if(is.null(dx))
    newx=matrix(newx,1,byrow=TRUE)

  if(ncol(newx) != p)
    stop(paste0("The number of variables in newx must be ",p))

  if(sum(is.na(beta)) >0){
    warning("prediction from a rank-deficient fit may be misleading")
    beta[is.na(beta)] <- 0
  }
  nfit=as.matrix(cbind2(1,newx)%*%beta)

  if(family == "gaussian"){
    return(nfit)
  }else if(family == "binomial"){
    if(type == "response"){
      pp=exp(-nfit)
      return(1/(1+pp))
    }else if(type == "class"){
      cnum=ifelse(nfit>0,levels[2], levels[1])
      return(cnum)
    }else if(type == "link"){
      pp=exp(-nfit)
      return(pp)
    }else{
      stop("response type can only be class, link, or response")
    }
  }
}

