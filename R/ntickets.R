#' ntickets
#'
#' @param N numeric vector
#' @param gamma numeric vector
#' @param p numeric vector
#'
#' @importFrom stats pbinom
#' @importFrom stats pnorm
#' @importFrom stats uniroot
#'
#' @return list
#' @export
#'
#' @examples ntickets(N=400, gamma=0.02, p=0.95)
ntickets=function(N, gamma, p){
  # Using the discrete binomial distribution:
  binomfind <- function(N, gamma, p, nd){
    pbinom(q=N, size=nd, prob=p, lower.tail=FALSE)
  }
  nd = N # not possible to overbook if less tickets sold than seats
  while(binomfind(N=N, gamma=gamma, p=p, nd=nd+1) - gamma < 0){ # check if next value of n is too large
    nd = nd + 1 # if n is not too large, n is increased and checked again
  }

  plot(N:(nd+20), binomfind(N=N, gamma=gamma, p=p, nd=N:(nd+40)) - gamma, type="b", xlab="n", ylab="Objective", pch=19, main="Objective function to find n using binomial distribution")
  abline(h=0, v=nd)

  # Using the continuous normal approximation:
  approxnorm=function(n){
    pnorm(q=N+0.5, mean=p*n, sd=sqrt(n*p*(1-p)), lower.tail=FALSE) - gamma # probability of overbooking minus gamma
  }
  nc <- uniroot(approxnorm, interval=c(N, N+40))$root # value for which probability of overbooking is equal to gamma

  x <- NULL
  curve(approxnorm(x), from=N, to=(nc+40), xlab="n", ylab="Objective", main="Objective function to find n with normal approximation")
  abline(h=0,v=nc)

  list(nd=nd, nc=nc, N=N, gamma=gamma, p=p)
}
