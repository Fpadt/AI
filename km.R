library(ggplot2)
library(data.table)
library(magrittr)

set.seed(123)
players <-  11
SD      <- 1.5

data <-
  rbind(
    data.table(
      x = rnorm(n = players, mean = 3, sd = SD),
      y = rnorm(n = players, mean = 5, sd = SD)
    ),
    data.table(
      x = rnorm(n = players, mean = 10, sd = SD),
      y = rnorm(n = players, mean = 3 , sd = SD)
    )
  )

ggplot(data, aes(x=x, y=y, color = "red"), shape = 4) +
  geom_point(alpha = 1.00, size=3) +
  xlab("x") +
  ylab("y") +
  xlim(0, 12.5) +
  ylim(0, 10)

km.out <- kmeans(data, centers = 2, nstart = 20)
km.out

data$cluster_id <- factor(km.out$cluster)

ggplot(data, aes(x=x, y=y, color = cluster_id), shape = 4) +
  geom_point(alpha = 1.00, size = 3) +
  xlab("x") +
  ylab("y") +
  geom_rect(
    aes(
      xmin=0, xmax=12.5,
      ymin=0, ymax=10
      ),
    fill="green", alpha =0.01) +
  xlim(0, 12.5) +
  ylim(0, 10)
