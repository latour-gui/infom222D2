# Title     : Loopback
# Created by: Guillaume <taz> Latour
# Created on: 02/11/2020

arrival <- function(n = 1, m = 30, sd = 10) {
  rnorm(n, m, sd)
}

service <- function(m = 30, sd = 10) {
  rnorm(1, m, sd)
}

loopback <- function(p = 0.1) {
  runif(1) < p
}

partial_sum <- function(elems) {
  tab <- numeric(length(elems))
  for (i in seq_along(elems)) {
    tab[i] <- sum(elems[1:i])
  }
  tab
}

ssqlb <- function(n, loopback_p = 0.1, arrival_mean = 30, arrival_sd = 10, service_mean = 30, service_sd = 10) {
  arrivals <- arrival(n, arrival_mean, arrival_sd)
  queue <- arrivals %>% partial_sum
  services <- delays <- numeric(0)
  loop <- busy <- 0
  while (length(queue) > 0) {
    # sort queue
    queue <- queue[order(queue)]

    # pop first element
    t <- queue[1]
    queue <- queue[-1]

    # if server is busy
    if (t < busy) {
      # count the delay
      delays <- c(delays, busy - t)
      # fastforward to the time when the server is not busy anymore
      t <- busy
    }

    # now the service time of this job can be calculated
    s <- service(service_mean, service_sd)
    services <- c(services, s)
    # so the server will be busy untill this time
    busy <- t + s

    # if there is a loopback, the job is queued right after quitting the server
    if (loopback(loopback_p)) {
      loop <- loop + 1
      queue <- c(queue, busy)
    }
  }

  print(sprintf('arrivals : %s', length(arrivals)))
  print(sprintf('services : %s', length(services)))
  print(sprintf('loops : %s', loop))
  delays
}

ssqlb(10)
