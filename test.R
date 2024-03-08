library(ggplot2)
library(statsr)
library(dplyr)

data("atheism")
us12 <- atheism %>%
  filter(nationality == "United States" , atheism$year == "2012")
sum_us_12 <- summary(us12)

inf_ci_proportion(us12, "response", "atheist")
