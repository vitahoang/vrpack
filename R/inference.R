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



#' Inference Confidence Interval for Proportion
#'
#' @param df The data frame
#' @param res_var response value
#' @param res_success success value response
#'
#' @return An inference vector contains inference information
#' @export
#'
#' @examples
#' data("atheism")
#' us12 <- atheism %>%
#'   filter(nationality == "United States" , atheism$year == "2012")
#' inf_ci_proportion(us12, "response", "atheist")
inf_ci_proportion <- function(df, res_var, res_success) {
  total_sample <- nrow(df) # total samples of the USA
  prop_sample <- nrow(df[df[[res_var]] == res_success,]) # total number of responses
  p_hat <- prop_sample / total_sample # p-hat of responses in the sample proportion

  # check CI condition
  if (p_hat*total_sample >=10 | (1-p_hat)*total_sample >=10){
    print("Condition met for CI")
    inf <- inference(y = response, data = df, statistic = "proportion", type = "ci", method = "theoretical", success = res_success)
    return(inf)
  } else{
    print("Condition not met for CI")
    return(NULL)
  }
}

