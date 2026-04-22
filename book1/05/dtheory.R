library(tidyverse)

make_loss_fun <- function(loss_drug = 15) {

  loss_covid_young <- 60
  loss_covid_old <- 10

  loss_mat <- matrix(0, nrow = 4, ncol = 2)

  loss_mat[1, ] <- c(0, loss_drug)
  loss_mat[2, ] <- c(loss_covid_young, loss_drug)
  loss_mat[3, ] <- c(0, loss_drug)
  loss_mat[4, ] <- c(loss_covid_old, loss_drug)

  loss_mat
}

normalize <- function(x) x / sum(x)

posterior_covid <-
  function(observed,
           prevalence = NULL,
           sensitivity = NULL) {

    if (is.null(sensitivity))
      sensitivity <- 0.875

    specificity <- 0.975

    TPR <- sensitivity
    FNR <- 1 - TPR
    TNR <- specificity
    FPR <- 1 - TNR

    likelihood_fn <- matrix(
      c(TNR, FPR,
        FNR, TPR),
      nrow = 2,
      byrow = TRUE
    )

    if (is.null(prevalence))
      prevalence <- 0.1

    prior <- c(1 - prevalence, prevalence)

    likelihood <- likelihood_fn[, observed + 1]

    normalize(prior * likelihood)
  }

# Returns a length-4 belief vector over joint states:
# (No COVID, Young), (COVID, Young), (No COVID, Old), (COVID, Old)
compute_belief <-
  function(test_result,
           age,
           prevalence = NULL,
           sensitivity = NULL) {

    post <- posterior_covid(
      test_result,
      prevalence,
      sensitivity
    )

    bel <- rep(0, 4)

    if (age == 0) {
      bel[1:2] <- post
    } else {
      bel[3:4] <- post
    }

    bel
  }

make_table <-
  function(cost_drug,
           prevalence = NULL,
           sensitivity = NULL) {

    test_vals <- 0:1
    age_vals <- 0:1

    records <- list()
    idx <- 1

    loss_mat <- make_loss_fun(cost_drug)

    for (test_result in test_vals) {
      for (age in age_vals) {

        bel <- compute_belief(
          test_result,
          age,
          prevalence,
          sensitivity
        )

        # posterior expected loss/risk
        expected_loss <- as.vector(
          bel %*% loss_mat
        )

        action <- which.min(expected_loss) - 1

        pr_covid <- if (age == 0) {
          bel[2]
        }  else {
          bel[4]
        }

        records[[idx]] <- tibble(
          test = test_result,
          age = age,
          pr_covid = pr_covid,
          cost_noop = expected_loss[1],
          cost_drugs = expected_loss[2],
          action = action
        )

        idx <- idx + 1
      }
    }

    bind_rows(records)
  }

df <- make_table(cost_drug = 8)
df

df <- make_table(cost_drug = 5)
df

df <- make_table(
  cost_drug = 8,
  sensitivity = 0.975
)
df
