library(tidyverse)
dat <- read.csv("../data/All_Tickers_5_Year_Data.csv")

dat_clean <- dat
#dat_clean <- dat[ , colSums(is.na(dat))==0]
dat_clean$Date <- as.Date(dat$Date)



dat_pivot_longer <- pivot_longer(dat_clean, cols= -Date, names_to="stock", values_to="values")



dat_separated <- dat_pivot_longer %>%
  mutate(stock_name = str_extract(stock, "[^_]+"), bench_mark = str_extract(stock, "(?<=_).*") )

dat_separated <- dat_separated %>%
  filter(!is.na(values)) %>%
  select(-stock)




rm(dat_pivot_longer,dat_clean,dat)

dat_wider <- dat_separated %>%
  pivot_wider(names_from=bench_mark, values_from=values)



write_csv(dat_wider,file = '../data/long_data.csv')

dat_wider <- read_csv('../data/long_data.csv')
dat_wider_sub <- dat_wider %>% slice_sample(n = 500000)

write_csv(dat_wider_sub,file = '../data/long_data_sub.csv')
