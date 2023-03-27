#' @name split_data
#' @title Split Data
#' @description Split data into training and test data
#' @param data Dataframe
#' @param outcome Outcome variable
#' @param size Number of observations in test data
#' @return List of training and test data
#' @export
split_data <- function(data, outcome, size) {
  if (typeof(outcome) == "character") {
    outcome <- as.name(outcome)
  }
  test_data <- data |>
    # mutate({{ outcome }} := factor({{ outcome }}), levels = seq(-3,3,1)) |>
    group_by({{ outcome }}) |>
    sample_n(size) |>
    ungroup()
  train_data <- data |>
    # mutate({{ outcome }} := factor({{ outcome }}), levels = seq(-3,3,1)) |>
    anti_join(test_data, by = "user_id")

  test_n <- test_data[[outcome]] |>
    unique() |>
    length()
  train_n <- train_data[[outcome]] |>
    unique() |>
    length()
  if (test_n != train_n) {
    stop(
      "The number of unique values in the outcome variable is",
      " not the same in the training and test data. ",
      "Reduce the size value"
    )
  }

  return(list(
    train_data = train_data, 
    test_data = test_data))
}

#' @name get_metrics
#' @title Get Metrics
#' @description Get metrics and confusion matrix
#' @param actual Actual values
#' @param predicted Predicted values
#' @return List of confusion matrix, balanced accuracy, chance level, and binomial test
#' @export
metrics <- function(actual, predicted) {

  cnf_table <- tibble(actual, predicted)
  cnf_matrix <- table(cnf_table)
  w <- 1
  balanced_accuracy <- (sum(diag(cnf_matrix) / (cnf_matrix |> 
    rowSums()) * w) / (cnf_matrix |> 
      nrow()))
  chance <- 1 / (cnf_matrix |> nrow())

  trials <- sum(cnf_matrix)
  successes <- sum(diag(cnf_matrix))
  h_test <- binom.test(successes, trials, alternative = "greater", conf.level = 0.95)
  return(list(
    confusion_matrix = cnf_matrix,
    balanced_accuracy = balanced_accuracy,
    chance_level = chance,
    binomial_test = h_test
  ))
}

