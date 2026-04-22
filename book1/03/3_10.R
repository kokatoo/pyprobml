library(tidyverse)
library(patchwork)

# -----------------------
# Transform circle into an ellipse shaped exactly like the covariance structure.
# -----------------------
cov_to_ellipse <- function(cov, alpha = 0.95) {

  theta <- seq(0, 2 * pi, length.out = 100)
  circle <- rbind(cos(theta), sin(theta))

  sv <- svd(cov)

  scale <- sqrt(qchisq(alpha, df = 2))
  A <- sv$u %*% diag(scale * sqrt(sv$d))
  ## A <- sv$u %*% diag(2.447 * sqrt(sv$d))  # 95%

  pts <- A %*% circle

  pts
}

# -----------------------
# posterior
# -----------------------
calc_posterior <- function(prior, likelihood, W, y_obs) {

  S_inv  <- solve(likelihood$sigma)
  S0_inv <- solve(prior$sigma)

  post_sigma <- solve(
    S0_inv + t(W) %*% S_inv %*% W
  )

  post_mu <- post_sigma %*% (
    t(W) %*% S_inv %*% (y_obs - likelihood$bias) +
      S0_inv %*% prior$mu
  )

  list(
    mu = post_mu,
    sigma = post_sigma
  )
}

# -----------------------
# plotting
# -----------------------
gauss_plot2d <- function(plot_dict, show_legend = FALSE) {

  df_pts <- imap_dfr(
    plot_dict,
    function(val, nm) {
      tibble(
        x = as.numeric(val$mu[1]),
        y = as.numeric(val$mu[2]),
        key = nm,
        color = val$color,
        label = val$label
      )
    }
  )

  df_ell <- imap_dfr(
    plot_dict,
    function(val, nm) {

      pts <- cov_to_ellipse(val$sigma)

      tibble(
        x = pts[1, ] + as.numeric(val$mu[1]),
        y = pts[2, ] + as.numeric(val$mu[2]),
        key = nm,
        color = val$color,
        label = val$label
      )
    }
  )

  p <- ggplot() +
    geom_path(
      data = df_ell,
      aes(x = x, y = y, group = key, color = label),
      linewidth = 1
    ) +
    geom_point(
      data = df_pts,
      aes(x = x, y = y, color = label),
      shape = 4,
      size = 3
    ) +
    labs(x = "x", y = "y", color = "") +
    theme_minimal()

  if (!show_legend) {
    p <- p + theme(legend.position = "none")
  }

  p
}

# -----------------------
# main
# -----------------------
gen_data <- function(sensorf_parm_dict) {

  plots <- imap(
    sensorf_parm_dict,
    function(value, nm) {

      prior <- list(
        mu = c(0, 0),
        sigma = 1e10 * diag(2)
      )

      y1 <- c(0, -1)
      y2 <- c(1, 0)

      y_all <- c(y1, y2)

      likelihood <- list(
        bias = rep(0, 4),
        sigma = as.matrix(
          Matrix::bdiag(
            value$sigma[[1]], value$sigma[[2]]
          )
        )
      )

      W <- rbind(diag(2), diag(2))

      posterior <- calc_posterior(prior, likelihood, W, y_all)

      plot_dict <- list(
        y1 = list(
          mu = y1,
          sigma = value$sigma[[1]],
          color = "red",
          label = "y2"
        ),
        y2 = list(
          mu = y2,
          sigma = value$sigma[[2]],
          color = "green",
          label = "y1"
        ),
        posterior = list(
          mu = as.vector(posterior$mu),
          sigma = posterior$sigma,
          color = "black",
          label = "posterior"
        )
      )

      gauss_plot2d(
        plot_dict, show_legend = (nm == "Fig(b)")
      )
    })

  wrap_plots(plots, nrow = 1)
}

# -----------------------
# input
# -----------------------
sensorf_parm_dict <- list(
  "Fig(a)" = list(
    sigma = list(
      0.01 * diag(2),
      0.01 * diag(2)
    )
  ),
  "Fig(b)" = list(
    sigma = list(
      0.05 * diag(2),
      0.01 * diag(2)
    )
  ),
  "Fig(c)" = list(
    sigma = list(
      0.01 * matrix(c(10,1,1,1), 2),
      0.01 * matrix(c(1,1,1,10), 2)
    )
  )
)

# run
gen_data(sensorf_parm_dict)
