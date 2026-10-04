library(dplyr)
library(tidyr)
library(stringr)

# PITCHING
# Load data sets from https://www.baseball-reference.com/teams/LAD/2026.shtml
ladodgerspdf <- read.csv("C:/Users/shane/OneDrive/Desktop/LA Dodgers Pitching and Batting/LADodgersPitching.csv")

# Remove Bad Data (-9999 and NAs)
ladodgerspdf <- na.omit(ladodgerspdf%>%
                        filter_out(Player.additional == -9999)%>%
                        rename(PlayerID = Player.additional))

# Separating Players based off of Handedness
ladodgerspdf <- ladodgerspdf %>% 
  mutate(Handedness = factor(case_when(
    str_ends(Player, fixed("*")) ~ "Left",
    str_ends(Player, fixed("#")) ~ "Both",
    TRUE                         ~ "Right"
  )))

# Removes ambidextrous people
ladodgerspdf <- ladodgerspdf%>%
  filter_out(Handedness == "Both")


pvaldf <- data.frame()

# Getting numeric columns to avoid errors in loop
numeric_cols <- colnames(ladodgerspdf)[sapply(ladodgerspdf, is.numeric)]

# Generates a  value for all columns based on Handedness
for (i in numeric_cols) {
  # must create a formula as the parameter for the t.test function
  formula <- as.formula(paste(i, "~ Handedness"))
  p_val <- t.test(formula, var.equal = FALSE, data = ladodgerspdf)$p.value
  
  # Append to your data frame
  pvaldf <- rbind(pvaldf, data.frame(Variable = i, P_Value = p_val))
}

# Filters pvalues that are less than 0.05 (significant)
sigpvaldf <- pvaldf%>%
  filter(P_Value < 0.05)
sigpvaldf



# EXTRA Logistic Regression Model to Predict Handedness
# Use all variables in sigpvaldf initially
logistic <- glm(Handedness ~ RAA + WAA + X162WL., data = ladodgerspdf, family = "binomial")
summary(logistic)

# Backwards regression yields that only using RAA is the best predictor for Handedness
logistic <- glm(Handedness ~ RAA, data = ladodgerspdf, family = "binomial")
summary(logistic)







# BATTING
# Load data sets from https://www.baseball-reference.com/teams/LAD/2026.shtml
ladodgersbdf <- read.csv("C:/Users/shane/OneDrive/Desktop/LA Dodgers Pitching and Batting/LADodgersBatting.csv")

# Remove Bad Data (-9999 and NAs)
ladodgersbdf <- na.omit(ladodgersbdf%>%
                          filter_out(Player.additional == -9999)%>%
                          rename(PlayerID = Player.additional))

# Separating Players based off of Handedness
ladodgersbdf <- ladodgersbdf %>% 
  mutate(Handedness = factor(case_when(
    str_ends(Player, fixed("*")) ~ "Left",
    str_ends(Player, fixed("#")) ~ "Both",
    TRUE                         ~ "Right"
  )))

# Removes ambidextrous people
ladodgersbdf <- ladodgersbdf%>%
  filter_out(Handedness == "Both")


pvaldf <- data.frame()

# Getting numeric columns to avoid errors in loop
numeric_cols <- colnames(ladodgersbdf)[sapply(ladodgersbdf, is.numeric)]

# Generates a  value for all columns based on Handedness
for (i in numeric_cols) {
  # must create a formula as the parameter for the t.test function
  formula <- as.formula(paste(i, "~ Handedness"))
  p_val <- t.test(formula, var.equal = FALSE, data = ladodgersbdf)$p.value
  
  # Append to your data frame
  pvaldf <- rbind(pvaldf, data.frame(Variable = i, P_Value = p_val))
}

# Filters pvalues that are less than 0.05 (significant)
sigpvaldf <- pvaldf%>%
  filter(P_Value < 0.05)
sigpvaldf

# Handedness is not a good predictor of any of the variables for batters
