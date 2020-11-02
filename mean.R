# Title     : Relation between arithmetic average and mean
# Created by: Guillaume <taz> Latour
# Created on: 02/11/2020


m <- 10
sd <- 5

draw_simple_example <- function(normal_mean, normal_standart_deviation) {
  values <- rnorm(50, normal_mean, normal_standart_deviation) %>%
    data.frame(., 1)

  plot(values, type = 'o', ylab = '', yaxt = 'n', main = 'Normal law draw example')
}

draw_average_iteration_relation <- function(min, max, step, normal_mean, normal_standart_deviation) {
  # Impossible to create the y array through vectorization because
  # rnorm `n` argument cannot be a vector of number of iterations.
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

draw_simple_example(m, sd)
print("done with example")
draw_average_iteration_relation(10, 6000, 10, m, sd)
print("done with first iteration")
draw_average_iteration_relation(5000, 1000000, 1000, m, sd)
print("done with second iteration")

print("everything went fine")