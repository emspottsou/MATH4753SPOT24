#' Title
#'
#' @param regression model object
#'
#' @return plot
#' @export
#'
#' @examples
#' residplot(quad.lm)
residplot=function(model){
  plot(fitted(model),residuals(model))
  }
