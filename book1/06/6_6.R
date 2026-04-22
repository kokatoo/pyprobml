library(tidyverse)
library(patchwork)

set.seed(0)

rotation <- function(xy, t) {
  R <- matrix(
    c(cos(t), -sin(t),
      sin(t),  cos(t)),
    2, 2
  )

  xy %*% R
}

mysubplot <-
  function(df,
           xlim = c(-4,4),
           ylim = c(-4,4),
           title = "") {

  r <- round(cor(df$x, df$y), 1)

  ggplot(df, aes(x, y)) +
    geom_point(shape = ".") +
    coord_cartesian(xlim = xlim, ylim = ylim) +
    ggtitle(paste0("r=", r, "\nMIC=NA")) +
    theme_void() +
    theme(
      plot.title = element_text(size = 9)
    )
}

mvnormal <- function(n = 800) {

  cors <- c(1, 0.8, 0.4, 0, -0.4, -0.8, -1)

  plots <- lapply(
    cors,
    function(cor) {
      cov <- matrix(c(1, cor, cor, 1), 2, 2)
      xy <- MASS::mvrnorm(n, c(0,0), cov)

      mysubplot(
        data.frame(x = xy[,1], y = xy[,2])
      )
    }
  )

  wrap_plots(plots, ncol = 7)
}

rotnormal <- function(n = 200) {

  ts <- c(
    0,
    pi/12,
    pi/6,
    pi/4,
    pi/2 - pi/6,
    pi/2 - pi/12,
    pi/2
  )

  cov <- matrix(
    c(1,1,1,1),
    2, 2
  )

  xy <- MASS::mvrnorm(n, c(0,0), cov)

  plots <- lapply(
    ts,
    function(t) {
      xy_r <- rotation(xy, t)

      mysubplot(
        data.frame(x = xy_r[,1], y = xy_r[,2])
      )
    }
  )

  wrap_plots(plots, ncol = 7)
}

others <- function(n = 800) {

  x <- runif(n, -1, 1)
  plots <- list()

  y1 <- 4*(x^2 - 0.5)^2 +
    runif(n, -1, 1)/3

  plots[[1]] <- mysubplot(
    data.frame(x=x, y=y1),
    xlim=c(-1,1),
    ylim=c(-1/3,1+1/3)
  )

  y2 <- runif(n,-1,1)
  xy <- cbind(x,y2)
  xy <- rotation(xy, -pi/8)

  plots[[2]] <- mysubplot(
    data.frame(x=xy[,1], y=xy[,2]),
    xlim=c(-1.5,1.5),
    ylim=c(-1.5,1.5)
  )

  xy <- rotation(xy, -pi/8)

  plots[[3]] <- mysubplot(
    data.frame(x=xy[,1], y=xy[,2]),
    xlim=c(-sqrt(2),sqrt(2)),
    ylim=c(-sqrt(2),sqrt(2))
  )

  y3 <- 2*x^2 + runif(n,-1,1)

  plots[[4]] <- mysubplot(
    data.frame(x=x,y=y3),
    xlim=c(-1,1),
    ylim=c(-1,3)
  )

  y4 <- (x^2 + runif(n,0,0.5)) *
    sample(c(-1,1), n, TRUE)

  plots[[5]] <- mysubplot(
    data.frame(x=x,y=y4),
    xlim=c(-1.5,1.5),
    ylim=c(-1.5,1.5)
  )

  y <- cos(x*pi) + runif(n,0,1/8)
  x2 <- sin(x*pi) + runif(n,0,1/8)
  plots[[6]] <- mysubplot(
    data.frame(x=x2,y=y),
    xlim=c(-1.5,1.5),
    ylim=c(-1.5,1.5)
  )

  xy1 <- MASS::mvrnorm(n/4, c(3,3), diag(2))
  xy2 <- MASS::mvrnorm(n/4, c(-3,3), diag(2))
  xy3 <- MASS::mvrnorm(n/4, c(-3,-3), diag(2))
  xy4 <- MASS::mvrnorm(n/4, c(3,-3), diag(2))

  xy <- rbind(xy1,xy2,xy3,xy4)

  plots[[7]] <- mysubplot(
    data.frame(x=xy[,1], y=xy[,2]),
    xlim=c(-7,7),
    ylim=c(-7,7)
  )

  wrap_plots(plots, ncol = 7)
}

mvnormal(800) / rotnormal(200) / others(800)
