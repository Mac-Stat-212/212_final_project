library(tidyverse)
dat <- read.csv("212_final_project/data/All_Tickers_5_Year_Data.csv")



dat_clean <- dat[ , colSums(is.na(dat))==0]
dat_clean$Date <- as.Date(dat_clean$Date)



dat_pivot_longer <- pivot_longer(dat_clean, cols=2:35767, names_to="stock", values_to="values")



dat_separated <- dat_pivot_longer %>%
  mutate(stock_name = str_extract(stock, "[^_]+"), bench_mark = str_extract(stock, "(?<=_).*") )



dat_wider <- dat_separated %>%
  pivot_wider(names_from=bench_mark, values_from=values)

write_csv(dat_wider,file = '../data/Long_Data.csv')