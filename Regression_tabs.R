###############################################################################
# FULL REGRESSION TABLES 
################################################################################

library(flextable)
library(huxtable)


########################################################################
# REGRESSION TABLES FOR SWEDEN
########################################################################

# LOOP TO CREATE ESTIMATE MODELS 


for (i in 1:8) {
  
  # convert to weeks
  j <- i*7 
  
  # create a list of different models 
  models <- list(
    "Trust" = lm(Trust ~ Treatment, data = ESSSE |> filter(Bandwith <= j)),
    "Obligation" = lm(Obligation ~ Treatment, data = ESSSE |> filter(Bandwith <= j)),
    "Moral \nAlignment" = lm(MoralAlignment ~ Treatment, data = ESSSE |> filter(Bandwith <= j)),
    "Lawfulness" = lm(Lawfulness ~ Treatment, data = ESSSE |> filter(Bandwith <= j)),
    "Procedural Fairness" = lm(ProceduralFairness ~ Treatment, data = ESSSE |> filter(Bandwith <= j)),
    "Police \nEffectiveness" = lm(Effectiveness ~ Treatment, data = ESSSE |> filter(Bandwith <= j)))
  
  # Create Regression Table using huxtable
  models <- models|>
    huxreg(
      statistics = c(N = "nobs", R2 = "r.squared"),
      borders = 0) |>
    set_bold(row = 1, col = everywhere) |>
    set_bottom_border(row = 1, col = everywhere) 
  
  titles <- models[1,]
  
  models <- models[1:7, ]
  
  models[1,] <-""
  
  models[1,1] <- paste0(i," Week Window ")
  
  assign(paste0("day_",j,"_ESSSE"), models)
}



# BIND RESULTS INTO A TABLE 

# 1-4 Week Bandwith 

tab <- rbind(titles,
             day_7_ESSSE,
             day_14_ESSSE,
             day_21_ESSSE,
             day_28_ESSSE
)

Regression_tables <- tab |> as_flextable()

# unload huxtable to avaoid funtion overlap 

detach("package:huxtable", unload = TRUE) 

Regression_tables <- Regression_tables |> 
  align(align = "center", part = "body") |>
  bold(i = c(2,9,16,23)) |>
  merge_at(i = 2, j = 1:2) |>
  merge_at(i = 9, j = 1:2) |>
  merge_at(i = 16, j = 1:2) |>
  merge_at(i = 23, j = 1:2) |>
  align(j = 1, align = "left") |>
  width(width = 1) |>
  add_footer_lines(values = as_paragraph(
    c("Note: Standard errors in parentheses. Unstandardized coefficients are given.",
      "*** p < 0.001;  ** p < 0.01;  * p < 0.")))

library(huxtable) # Reload 

Regression_tables_1_4_ESSSE <- Regression_tables
Regression_tables_1_4_ESSSE



# 5-8 Week windows 
tab <- rbind(titles,
             day_35_ESSSE,
             day_42_ESSSE,
             day_49_ESSSE,
             day_56_ESSSE
)

Regression_tables <- tab |> as_flextable()

# Unload to avoid overlp 
detach("package:huxtable", unload = TRUE)

Regression_tables <- Regression_tables |> 
  align(align = "center", part = "body") |>
  bold(i = c(2,9,16,22)) |>
  merge_at(i = 2, j = 1:2) |>
  merge_at(i = 9, j = 1:2) |>
  merge_at(i = 16, j = 1:2) |>
  merge_at(i = 23, j = 1:2) |>
  align(j = 1, align = "left") |>
  width(width = 1) |>
  add_footer_lines(values = as_paragraph(
    c("Note: Standard errors in parentheses. Unstandardized coefficients are given.",
      "*** p < 0.001;  ** p < 0.01;  * p < 0.")))

library(huxtable) # reload

Regression_tables_5_8_ESSSE <- Regression_tables
Regression_tables_5_8_ESSSE




########################################################################
# REGRESSION TABLES FOR RUSSIA
########################################################################

# LOOP TO CREATE ESTIMATE MODELS 


for (i in 1:8) {
  
  # convert to weeks
  j <- i*7 
  
  # create a list of different models 
  models <- list(
    "Trust" = lm(Trust ~ Treatment, data = ESSRU |> filter(Bandwith <= j)),
    "Obligation" = lm(Obligation ~ Treatment, data = ESSRU |> filter(Bandwith <= j)),
    "Moral \nAlignment" = lm(MoralAlignment ~ Treatment, data = ESSRU |> filter(Bandwith <= j)),
    "Lawfulness" = lm(Lawfulness ~ Treatment, data = ESSRU |> filter(Bandwith <= j)),
    "Procedural Fairness" = lm(ProceduralFairness ~ Treatment, data = ESSRU |> filter(Bandwith <= j)),
    "Police \nEffectiveness" = lm(Effectiveness ~ Treatment, data = ESSRU |> filter(Bandwith <= j)))
  
  # Create Regression Table using huxtable
  models <- models|>
    huxreg(
      statistics = c(N = "nobs", R2 = "r.squared"),
      note = "{stars}
    Note. ",
      borders = 0) |>
    set_bold(row = 1, col = everywhere) |>
    set_bottom_border(row = 1, col = everywhere) 
  
  titles <- models[1,]
  
  models <- models[1:7, ]
  
  models[1,] <-""
  
  models[1,1] <- paste0(i," Week Window ")
  
  assign(paste0("day_",j,"_ESSRU"), models)
}



# BIND RESULTS INTO A TABLE 

# 1-4 Week Bandwith 

tab <- rbind(titles,
             day_7_ESSRU,
             day_14_ESSRU,
             day_21_ESSRU,
             day_28_ESSRU
)

Regression_tables <- tab |> as_flextable()

# unload huxtable to avaoid funtion overlap 

detach("package:huxtable", unload = TRUE) 

Regression_tables <- Regression_tables |> 
  align(align = "center", part = "body") |>
  bold(i = c(2,9,16,23)) |>
  merge_at(i = 2, j = 1:2) |>
  merge_at(i = 9, j = 1:2) |>
  merge_at(i = 16, j = 1:2) |>
  merge_at(i = 23, j = 1:2) |>
  align(j = 1, align = "left") |>
  width(width = 1) |>
  add_footer_lines(values = as_paragraph(
    c("Note: Standard errors in parentheses. Unstandardized coefficients are given.",
      "*** p < 0.001;  ** p < 0.01;  * p < 0.")
  ))

library(huxtable) # Reload 

Regression_tables_1_4_ESSRU <- Regression_tables
Regression_tables_1_4_ESSRU



# 5-8 Week windows 
tab <- rbind(titles,
              day_35_ESSRU,
              day_42_ESSRU,
              day_49_ESSRU,
              day_56_ESSRU
)

Regression_tables <- tab |> as_flextable()

# Unload to avoid overlp 
detach("package:huxtable", unload = TRUE)

Regression_tables <- Regression_tables |> 
  align(align = "center", part = "body") |>
  bold(i = c(2,9,16,23)) |>
  merge_at(i = 2, j = 1:2) |>
  merge_at(i = 9, j = 1:2) |>
  merge_at(i = 16, j = 1:2) |>
  merge_at(i = 23, j = 1:2) |>
  align(j = 1, align = "left") |>
  width(width = 1) |>
  add_footer_lines(values = as_paragraph(
    c("Note: Standard errors in parentheses. Unstandardized coefficients are given.",
      "*** p < 0.001;  ** p < 0.01;  * p < 0.")))

library(huxtable) # reload

Regression_tables_5_8_ESSRU <- Regression_tables
Regression_tables_5_8_ESSRU

