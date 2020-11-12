# Title     : Relation between arithmetic average and mean
# Created by: Guillaume <taz> Latour
# Created on: 02/11/2020

# allow the use of pipes for easy function composition
library(magrittr)

#' Plot of n draws of a normal distribution according to the given parameters
#'
#' This plot comes without y axis
#'
#' @param normal_mean float               : The \mu parameter for the normal distribution
#' @param normal_standart_deviation float : The \sigma parameter for the normal distribution
#' @param n integer                       : The number of draws that have to be plotted
plot_simple_example <- function(normal_mean, normal_standart_deviation, n = 50) {
  values <- rnorm(n, normal_mean, normal_standart_deviation) %>%  # get n draws of normal distribution
    data.frame(., 1)                                              # store as a dataframe

  plot(values, type = 'o', ylab = '', yaxt = 'n', main = 'Normal law draw example')
}


#' Plot 2 graphs
#'  - Arithmetic average of the draws of the normal distribution
#'  - Proximity (or distance) between the aritmetic average and the mean of the normal distribution
#' considered
#'
#' The normal law is defined by the two paremeters `normal_mean` and `normal_standart_deviation`.
#'
#' The x axis is generated for every `step` from `min` to `max` and represent the number of draws.
#'
#' @param min integer                     : The minimum number of draws that will be made for the plot
#' @param max integer                     : The maximum number of draws that will be made for the plot
#' @param step integer                    : The value of increment to go from `min` to `max`.
#'                                          There will be (`max` - `min`)/`step` points on the plot
#' @param normal_mean float               : The \mu parameter for the normal distribution
#' @param normal_standart_deviation float : The \sigma parameter for the normal distribution
draw_average_iteration_relation <- function(min, max, step, normal_mean, normal_standart_deviation) {
  # Impossible to create the y array through vectorization because rnorm `n` argument cannot be a vector
  # of number of iterations.
  # So the y vector is preallocated to avoid a new copy at every iteration of the for loop.
  x <- seq(min, max, step)
  y <- numeric(length(x))
  for (i in seq_along(x)) {
    y[i] <- rnorm(x[i], normal_mean, normal_standart_deviation) %>%
      mean
  }

  plot(x, y, pch = '.', ylab = 'Arithmetic average', xlab = 'Iterations')
  plot(x, abs(normal_mean - y), pch = '.', ylab = 'Mean proximity', xlab = 'Iterations')
}

# Beginning of the script
# quick configuration of the normal law parameters
m <- 10
sd <- 4

plot_simple_example(m, sd)
draw_average_iteration_relation(10, 6000, 10, m, sd)
draw_average_iteration_relation(5000, 10000000, 1000, m, sd) # /!\ this will take a while

print("everything went fine")