#' myncurve
#'
#' @param mu
#' @param sigma
#'
#' @return curve and list
#' @export
#'
#' @examples
#' myncurve(mu=10,sigma=2)
myncurve = function(mu, sigma){
  curve(dnorm(x,mean=mu,sd=sigma), xlim = c(mu-3*sigma, mu + 3*sigma))
  list(mu = mu, sigma = sigma)
}
