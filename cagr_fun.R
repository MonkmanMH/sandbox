# CAGR
# Compound Annual Growth Rate

# the CAGR function 
# -- `val_var` is the variable to be used in the calculations
# -- `period` is the number of records in the variable; 
#     -- default is the length of the vector, but can be over-ridden
#     -- eg if just start and end monthly values of a year-over-year comparison, 
#     -- would be 13
calc_cagr <- function(val_var, period = length(val_var)) {
  period = period - 1 # adjust to number of steps in the sequence
  cagr = (((val_var[length(val_var)] / val_var[1])) ^ (1 / period)) - 1
  return(cagr)
}


### ----- 
### TESTS
### -----

# tidyverse
library(tidyverse)

# test vector: 10% compound annual growth rate
vector_data <- c(100, 110, 121, 133.1)

# test vector: 10% compound annual growth rate over 4 steps with only end values
vector_data_2 <- c(100, 133.1)


# test tibble; 10% compound annual growth rate
table_of_data <- tribble(
    ~colA, ~colB,
    "a", 100,
    "b", 110,
    "c", 121,
    "d", 133.1
  )

# function to produce a sequence of numbers starting at 100 and 
#   growing by 10% (i.e. times 1.1) for 3 steps (i.e. 4 numbers)
cumprod(c(100, rep(1.1, 3)))

# test tibble 2; set compound annual growth rate at 15% and length of 10
table_of_data_2 <- tibble(
  colA = letters[1:10],   # the letters from a to j
  colB = cumprod(c(100, rep(1.15, 9))) 
)      





# applied to vector; returns single value
calc_cagr(vector_data)

# applied to end-points-only vector of length 2; returns single value
calc_cagr(vector_data_2, 4)


# inside pipe & applied to a tibble; returns a tibble
table_of_data %>% 
  summarise(calc_cagr(colB))

# applied to a tibble; returns a single value
calc_cagr(table_of_data$colB)

# applied to a tibble; returns a single value
calc_cagr(table_of_data_2$colB, 10)

