library(tidyverse)
library(kableExtra)
library(texreg)
library(nycflights13)


df <- mtcars %>% slice(1:5) %>% select(mpg:wt)

df %>% 
  kable(format = "latex", booktabs = T) %>% 
  write_lines("base_table.tex")

df %>% 
  kable(format = "latex", booktabs = T) %>%  
  group_rows("Group 1", 1, 3) %>% 
  group_rows("Group 2", 3, 5) %>% 
  write_lines("table_groups.tex")

df %>% 
  kable(format = "latex", booktabs = T) %>% 
  add_header_above(c("", "Header 1[note]" = 2,
                     "Header 2[note]" = 3)) %>% 
  add_footnote(c("Header 1 related note", 
                 "Header 2 related note")) %>% 
  write_lines("table_headers.tex")


### texreg

reg <- lm(arr_delay ~ dep_delay, data = flights)
texreg(reg, file = "texreg_table1.tex", table = F)

formulae <- c(
  arr_delay ~ dep_delay,
  arr_delay ~ dep_delay + origin
)

# run the regressions
regs <- map(formulae, 
            ~lm(.x, data = flights))

texreg(regs, file = "texreg_table2.tex", table = F)


system("pdflatex tables.tex")




