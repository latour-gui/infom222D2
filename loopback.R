# Title     : Loopback
# Created by: Guillaume <taz> Latour
# Created on: 02/11/2020

arrival <- function(m = 30, sd = 10) {
  rnorm(1, m, sd)
}

service <- function(m = 30, sd = 10) {
  rnorm(1, m, sd)
}

loopback <- function(p = 0.1) {
  runif(1) < p
}

ssqlb <- function(max_time, loopback_p = 0.1, arrival_mean = 30, arrival_sd = 10, service_mean = 30, service_sd = 10) {
  tho <- max_time
  arrival_count <- service_count <- l <- 0
  a <- arrival(arrival_mean, arrival_sd)
  c <- Inf

  while ((a < tho) | (l > 0)) {
    t <- min(a, c)
    if (t == a) {
      # add job to server
      arrival_count <- arrival_count + 1
      l <- l + 1

      # compute next job arrival
      a <- a + arrival(arrival_mean, arrival_sd)
      if (a > tho) {
        a <- Inf
      }

      # if no queue, compute next job completion time
      if (l == 1) {
        c <- t + service(service_mean, service_sd)
      }
    }
    if (t == c) {
      service_count <- service_count + 1
      # job may be quitting server
      if (!loopback(loopback_p)) {
        l <- l - 1
      }

      # compute next job completion time
      if (l > 0) {
        c <- t + service(service_mean, service_sd)
      } else {
        c <- Inf
      }
    }
  }

  data.frame(arrival_count, service_count)
}

ssqlb(5000)
