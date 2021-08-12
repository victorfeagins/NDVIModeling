library(ggplot2)
library(dplyr)


summaryfile <- "/projectnb/dietzelab/GOES_DataFTP/SummaryModel/asuhighlands.csv"


df <- read.csv(summaryfile)

df %>% 
  filter(as.Date(Date)< "2021-01-01") %>% 
  ggplot(df, mapping = aes(x = as.Date(Date), y = c.50.))+
  geom_errorbar(aes(ymin = c.2.5., ymax = c.97.5.), color ="grey", alpha = .90)+
  geom_point()+
  scale_x_date(date_labels = "%m-%Y")+
  labs(title = "asuhighlands NDVI Time Series")+
  xlab("Time")+
  ylab("NDVI")
