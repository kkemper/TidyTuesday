# Tidy Tuesday - Student Loan Debt
library(tidyverse)
loans <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-11-26/loans.csv")

head(loans)
summary(loans)

loans <- loans %>%
  group_by(agency_name, year, quarter)
loans
view(loans)
