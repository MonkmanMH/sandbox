# applying Stephanie's solution to growing a series

# test tibble; 10% compound annual growth rate
df_data <- tribble(
  ~alpha, ~omega, ~val_var,
  "a", "x", 100,
  "a", "y", 200,
  "b", "x", NA,
  "b", "y", NA,
  "c", "x", NA,
  "c", "y", NA,
  "d", "x", NA,
  "d", "y", NA
)

growth_pct <- 0.1

df_data %>%
  group_by(omega) %>%
  # fill growth percentage column if the value variable is NA
  mutate(colC = ifelse(is.na(val_var), 1 + growth_pct, 1)) %>%
  # now fill value variable with constant value
  fill(val_var) %>%
  # calculate the cumulative growth rate (colD) at each step, then multiply that by the original value
  mutate(colD = cumprod(colC),
         new_val = val_var * colD)

