# PUZZLE DU JOUR OF THE DAY

library(tidyverse)

# We have an incomplete series with values (colB), and the growth rate for the last value (colC).
# Below is the tibble with 

# test tibble; 10% compound annual growth rate
table_of_data <- tribble(
  ~colA, ~colB, ~colC,
  "a", 100, NA,
  "b", 110, NA,
  "c", NA, 0.1
)

# this works...
table_of_data %>% 
  mutate(colB = case_when(
    is.na(colB) ~ lag(colB) * (1 + colC),
    TRUE ~ colB
  ))

#### --- 

# Your challenge--make it work with more than one value. 
# That is, the 121 in the third line above 
# becomes the input that is now grown 

# test tibble; 10% compound annual growth rate
table_of_data <- tribble(
  ~colA, ~colB, ~colC,
  "a", 100, NA,
  "b", 110, NA,
  "c", NA, 0.1,
  "d", NA, 0.2
)

# this does not work...
table_of_data %>% 
  mutate(colB = case_when(
    is.na(colB) ~ lag(colB) * (1 + colC),
    TRUE ~ colB
  ))

# tidyr::fill() will repeat the value in row 2 but...no arithmetic allowed
table_of_data %>% 
  fill(colB)

# I also tried "cumprod" but couldn't figure out the appropriate syntax for the partial series

### a FOR loop!

# iteration 1
output <- vector("double", nrow(table_of_data))
for (i in 1:nrow(table_of_data)) {
  output[i] <- table_of_data$colB[[i]]
}

# iteration 2
output <- vector("double", nrow(table_of_data))
for (i in 1:nrow(table_of_data)) {
  output[i] <- ifelse(is.na(table_of_data$colB[[i]]),
                      output[[i - 1]],
                      table_of_data$colB[[i]])
}


# third time lucky
df_grow <- table_of_data
grow_var <- table_of_data$colB
# if growth is in column of variables
pct_var <- table_of_data$colC
# if growth is a single value to be repeated
pct_var <- rep(0.027, times = nrow(df_grow))

output <- vector("double", nrow(df_grow))
for (i in 1:nrow(df_grow)) {
  output[i] <- ifelse(is.na(grow_var[[i]]),
                      output[[i - 1]] * (1 + pct_var[[i]]),
                      grow_var[[i]])
}

table_of_data_augmented <- table_of_data %>% 
  mutate(new_val = output)
table_of_data_augmented
