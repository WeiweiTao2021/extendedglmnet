#' print an extendedglmnet fitting results
#'
#' @details
#' The call that the fitting results from extendedglmnet is printed.
#' The coefficients of the fit followed by degrees of freedom (nobs - 1) and
#' the residual degree of freedom (nobs - number of nonzero coefficients).
#'
#' @aliases extendedglmnet.print
#'
#' @param fit fitted extendedglmnet object
#'
#' @seealso \code{\link{extendedglmnet}}, \code{\link{predict.extendglmnet}} and \code{\link{evaluation}} methods.
#'
#' @author Weiwei Tao \email{tww101928@@gmail.com}
#'
#' @examples
#'
#' size = 30
#' x = matrix(runif(35*size),ncol=35)
#' y = 10*x[,1] + 3*x[,2] + 20*(x[,3]-0.5)**2 + 10*x[,4] + 5*x[,35] + runif(1,0,1)
#' fit.regression <- extendedglmnet(x, y, family = "gaussian", type = "regression")
#' print.extendglmnet(fit.regression)
#'
#' @method extendedglmnet print
#' @export print.extendglmnet

print.extendglmnet<- function(x){
    cat("\nCall:", x$type, "for", x$family, "reponses\n\n")
    print(x$coef)

    cat("Degrees of Freedom:", x$nobs-1, "Total (i.e. Null);",  x$nobs - x$df - 1,  "Residual\n\n")
}
