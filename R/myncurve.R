#' myncurve
#'
#' @param mu
#' @param sigma
#' @param a
#'
#' @return curve and list
#' @export
#'
#' @examples
#' myncurve(mu=10,sigma=2)
myncurve = function(mu, sigma, a){
  curve(dnorm(x,mean=mu,sd=sigma), xlim = c(mu-3*sigma, mu + 3*sigma))
  xcurve=seq(6*sigma,a,length=1000)

  # Y values corresponding t0 the x values
  ycurve=dnorm(xcurve,mu,sigma)

  # Fill in the polygon with the given vertices
  polygon(c(6*sigma,xcurve,a),c(0,ycurve,0),col="Red")

  # Put in the text with the appropriate area

  # Area
  prob=pnorm(a, mu, sigma)
  prob=round(prob,4)

  list(mu = mu, sigma = sigma, prob=prob)
}
