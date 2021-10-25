
library(stringr)
library(dplyr)
library(ggplot2)


summarydirectory <- "/projectnb/dietzelab/GOES_DataFTP/SummaryModel" #The summary files


summaryfiles <- list.files(summarydirectory)

df <- file.path(summarydirectory, summaryfiles[[1]]) %>% 
  read.csv()

df %>% 
  mutate(Date = as.Date(Date)) %>% 
  filter(Date < as.Date("2021-01-01")) %>% 
  ggplot(mapping = aes(x = Date, y= c.50.)) +
  ggthemes::theme_base() +
  scale_x_date(date_labels = "%b") +
  geom_errorbar(aes(ymin = c.2.5., ymax = c.97.5.)) +
  geom_point() +
  labs(title = "ASU 2020 Daily NDVI Estimates Time Series",
       y = "NDVI")
