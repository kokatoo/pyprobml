library(purrr)
library(tidyverse)
library(patchwork)

plot_data_r <- function(x, pdf_dict, var_prior) {

    df <- imap_dfr(
      pdf_dict,
      function(val, nm) {
        tibble(
          x = x,
          y = val$pdf,
          type = nm,
          color = val$color,
          linetype = val$linetype
        )
      })

    ylim <- c(0, 0.6)
    xlim <- c(-7, 7)

    ggplot(
      df,
      aes(x = x, y = y, group = type)
    ) +
      geom_line(
        aes(color = type),
        linewidth = 1.2
      ) +
      scale_color_manual(
        values = c(
          prior = "blue",
          likelihood = "red",
          posterior = "black"
        )
      ) +
      ## scale_linetype_manual(
      ##   values = c(
      ##     prior = "solid",
      ##     likelihood = "dotted",
      ##     posterior = "dotdash"
      ##   )
      ## ) +
      coord_cartesian(xlim = xlim, ylim = ylim) +
      labs(
        title = paste0("Prior variance = ", var_prior),
        x = "x",
        y = "p(x)",
        color = "Distribution"#,
        ## linetype = "Distribution"
      ) +
      theme_minimal()
  }

noisy_y <- 3
x <- seq(-10, 10, by = 0.1)

z_prior_variance <- c(1, 5)
z_prior_mean <- c(0, 0)
observed_variance <- c(1, 1)

plots <- vector("list", length(z_prior_variance))

for (i in seq_along(z_prior_variance)) {

  zv <- z_prior_variance[i]
  zm <- z_prior_mean[i]
  yv <- observed_variance[i]

  # prior: Normal(zm, zv)
  prior_pdf <- dnorm(
    x,
    mean = zm,
    sd = sqrt(zv)
  )

  # likelihood: Normal(mean(noisy_y), yv)
  lik_mu <- mean(noisy_y)

  likelihood_pdf <- dnorm(
    x,
    mean = lik_mu,
    sd = sqrt(yv)
  )

  n <- length(noisy_y)

  # posterior variance:
  # 1 / (1/zv + n/yv)
  post_variance <- 1 / (1 / zv + n / yv)

  # posterior mean:
  # post_var * (zm/zv + n*lik_mu/yv)
  post_mu <- post_variance *
    ((zm / zv) + (n * lik_mu / yv))

  # posterior: Normal(post_mu, post_variance)
  posterior_pdf <- dnorm(
    x,
    mean = post_mu,
    sd = sqrt(post_variance)
  )

  pdf_dict <- list(
    prior = list(
      pdf = prior_pdf,
      color = "blue",
      linetype = "solid"
    ),
    likelihood = list(
      pdf = likelihood_pdf,
      color = "red",
      linetype = "dotted"
    ),
    posterior = list(
      pdf = posterior_pdf,
      color = "black",
      linetype = "dotdash"
    )
  )

  plots[[i]] <- plot_data_r(x, pdf_dict, zv)
}

# -----------------------
# side-by-side plot
# -----------------------
wrap_plots(plots, nrow = 1)
