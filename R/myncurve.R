#' Title    Normal Distribution Curve
#'
#' @param mu  The mean value
#' @param sigma The standard deviation
#' @param a   The limit on the right x
#'
#' @return  The shaded area of probability P(X<=a)
#' @export
#'
#' @examples
#' \dontrun{myncurve(mu=10,sigma=5,a=6)}
myncurve = function(mu, sigma, a){
  curve(dnorm(x,mean=mu,sd=sigma), xlim = c(mu-3*sigma, mu + 3*sigma))
  list(mu = mu, sigma = sigma)

  # Find the area between the curve and x axis from x=-∞ to x=a
  # x values corresponding to the x - cords of points on the curve
  xcurve=seq(mu-3*sigma,a,length=1000)

  # Y values corresponding to the x values
  ycurve=dnorm(xcurve,mean=mu,sd=sigma)

  # Fill in the polygon with the given vertices
  polygon(c(mu-3*sigma,xcurve,a),c(0,ycurve,0),col="Red")

  # Put in the text with the appropriate area
  # Area
  prob=pnorm(a,mean=mu,sd=sigma)
  print(prob)
}
