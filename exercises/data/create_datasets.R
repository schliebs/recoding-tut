library(tidyverse)

wide1 <- tribble(
  ~company, ~`2010`, ~`2011`,
  "Alice Intl", 20, 50,
  "Bob Industries", 72, 65,
  "Charlie Plc", 40, 38
)
write_csv(wide1, "wide1.csv")

wide2 <- tribble(
  ~company, ~industry, ~`2010`, ~`2011`,
  "Alice Intl", "metals", 20, 50,
  "Bob Industries", "chemistry", 72, 65,
  "Charlie Plc", "finance", 40, 38
)
write_csv(wide2, "wide2.csv")

wide3 <- tribble(
  ~company, ~industry, ~employees, ~`2010`, ~`2011`,
  "Alice Intl", "metals", 200, 20, 50,
  "Bob Industries", "chemistry", 290, 72, 65,
  "Charlie Plc", "finance", 50, 40, 38
)
write_csv(wide3, "wide3.csv")
