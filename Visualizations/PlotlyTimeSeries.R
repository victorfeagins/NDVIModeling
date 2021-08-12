library(ggplot2)
library(stringr)
library(dplyr)
library(plotly)

summarydirectory <- "/projectnb/dietzelab/GOES_DataFTP/SummaryModel" #The summary files

sitecodebook <- "/projectnb/dietzelab/vfeagins/Programming/NVDI_Modeling/GOESdownloadSites.csv" #This is the site codebook




Sites <- read.csv(sitecodebook)

sitenames <- Sites$siteName

site <- sitenames[1]

summarydata <- summarydirectory %>% 
  list.files() %>% 
  str_subset(as.character(site)) %>% 
  file.path(summarydirectory, .) %>% 
  read.csv()


ggplot(summarydata, mapping = aes(x = as.Date(Date), y = c.50.))+
  geom_errorbar(aes(ymin = c.2.5., ymax = c.97.5.), color ="grey", alpha = .90)+
  geom_point()+
  scale_x_date(date_labels = "%m-%Y")+
  labs(title = str_c(as.character(site), "_NDVI Time Series"))+
  xlab("Time")+
  ylab("NDVI")


ggplot(summarydata, mapping = aes(x = as.Date(Date), y = c.50.))+
  geom_errorbar(aes(ymin = c.2.5., ymax = c.97.5.), color ="grey", alpha = .90)+
  geom_point()+
  scale_x_date(date_labels = "%m-%Y")


summarydata %>%
  filter_all(all_vars(!is.na(.))) %>%  #error y does not like missing values.
  plot_ly() %>% 
  add_markers(x = ~as.Date(Date), y = ~c.50.,
              error_y = ~list(symmetric = FALSE,
                              arrayminus = c.50. - c.2.5.,
                              array = c.97.5. - c.50.,
                              color = '#000000')) %>% 
  layout(title = str_c(site, "NDVI Time Series"),
         xaxis = list(title = 'Date'),
         yaxis = list(title = 'NDVI (95% CI)'))

