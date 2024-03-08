# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'

inf_ci_proportion <- function(df, res_var, res_success) {
  total_sample <- nrow(df) # total samples of the USA
  prop_sample <- nrow(df[df[[res_var]] == res_success,]) # total number of responses
  p_hat <- prop_sample / total_sample # p-hat of responses in the sample proportion

  # check CI condition
  if (p_hat*total_sample >=10 | (1-p_hat)*total_sample >=10){
    print("Condition met for CI")
    inference(y = response, data = df, statistic = "proportion", type = "ci", method = "theoretical", success = res_success)
  } else{
    print("Condition not met for CI")
  }
}

inf_test <-function(df, col, prop_value) {
  print()
}
