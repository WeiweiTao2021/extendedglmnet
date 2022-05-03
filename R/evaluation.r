#' generate evaluation metrics based on predicted values
#'
#' This functions generate evaluation metrics depending on observed responses and predicted response values.
#'
#' @param y actual response variable.
#' @param ypred predicted response variable.
#' @param metrics can be MSE, MAE, RMSE and accuracy. accuracy is only applicable to binary responses.
#'
#' @return evaluation metrics.
#'
#' @author Weiwei Tao \email{tww101928@@gmail.com}

#' @seealso \code{\link{extendedglmnet}}, \code{\link{predict.extendglmnet}} and \code{\link{print.extendglmnet}} methods.
#'
#'
#' @export
#'
evaluation <- function(y, ypred, metrics=c("MSE","MAE", "RMSE","accuracy")){

  if(length(y) != length(ypred))
    stop(paste("number of observations in y (",length(y),") not equal to the number of rows of ypredict (",length(ypred),")",sep=""))

  res=switch(metrics,
             "MSE"=mean((y - ypred)^2),
             "RMSE"=sqrt(mean((y - ypred)^2)),
             "MAE"=mean(abs(y - ypred)),
             "accuracy"=sum(diag(table(y, ypred)))/length(y))

  return(res)
}
