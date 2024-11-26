#' Find confidence interval
#'
#' @param data a data set
#' @param alpha default value is 0.05
#'
#' @return a confidence interval
#' @export
#'
#' @examples
#' \dontrun{myci(d)}
myci <- function(data, alpha = 0.05) {
  n = length(data)
  t=qt(1-alpha/2,n-1)
  ci=c()
  ci[1]=mean(data)-t*sd(data)/sqrt(n)
  ci[2]=mean(data())+t*sd(data)/sqrt(n)
  ci
}
