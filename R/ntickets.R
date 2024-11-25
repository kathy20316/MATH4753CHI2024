#' Title    Calculate the number of tickets to be sold
#'
#' @param N   the number of seats in the flight
#' @param gamma    the probability a plane will be truly overbooked
#' @param p   the probability of a "show"
#'
#' @returns display a named list containing nd, nc, N, p and gamma; and
#' two plots for the discrete and for the continuous case to find the optimal tickets sold
#' @export
#'
#' @examples
#' \dontrun{ntickets(N=400,gamma = 0.02, p = 0.95)}
ntickets <- function(N=200, gamma = 0.02, p = 0.95) {
  # Make up possible discrete n values.
  n <- seq(N, floor(N + N/10), by = 1)

  # The objects for binomial distribution
  tempD <- 1 - gamma - pbinom(q = N, size = n, prob = p)
  # Find the index location of the minimum
  indexD <- which.min(abs(tempD))

  # Graphing
  plot(n, tempD, main = paste("Objective Vs n to find optimal tickets sold\n(", n[indexD], ") gamma=", gamma, "N=", N, "discrete"),
       xlab = "n", ylab = "Objective", col = "blue",
       type = "b", pch = 19, lwd = 1)

  abline(h=0, v=n[indexD], lwd = 2.5, col = "Red")

  # Normal approximation
  nn <- seq(N, floor(N + N/10), length = 1000)
  mu <- nn * p
  sigma <- sqrt(nn * p * (1 - p))

  # Temporary objects for normal approximation
  tempC <- 1 - gamma - pnorm(N, mu, sigma)
  # Find the index location of the minimum
  indexC <- which.min(abs(tempC))

  # Graphing
  plot(nn, tempC, main = paste("Objective Vs n to find optimal tickets sold\n(", nn[indexC], ") gamma=", gamma, "N=", N, "continuous"),
       xlab = "n", ylab = "Objective", col = "Black",
       type = "l", lwd = 2.5)

  abline(h=0, v=nn[indexC], lwd = 1.5, col = "Blue")

  list(nd = n[indexD], nc = nn[indexC], N = N, p = p, gamma = gamma)
}
