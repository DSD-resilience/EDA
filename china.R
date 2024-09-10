install.packages(c('tidyverse','tidyquant','timetk','ggthemes'))
install.packages('janitor')
install.packages('ggtext')


library(tidyverse)
library(tidyquant)
library(timetk)
library(ggthemes)
library(janitor)
library(ggtext)

#The China Fund, Inc. (quarterly) (YoY)
df_china_fund <- 
  tq_get("CHN") %>% 
  tq_transmute(select = close,
               mutate_fun = to.quarterly,
               col_rename = "chn") %>%
  #YoY returns
  mutate(chn = (chn / lag(chn, 4) - 1) %>% round(2)) %>% 
  select(date, chn) %>% 
  drop_na()


#China Gross Domestic Product (GDP) YoY
df_gdp_chn <- read_csv("https://raw.githubusercontent.com/mesdi/investingcom/main/china_gdp_yoy.csv")

#Tidy GDP data
df_gdp_tidy <- 
  df_gdp_chn %>% 
  janitor::clean_names() %>% 
  select(date = release_date, gdp = actual) %>% 
  mutate(date = #removing parentheses and the text within
           case_when(str_detect(date," \\(.*\\)") ~ str_remove(date," \\(.*\\)"), 
                     TRUE ~ date)) %>% 
  mutate(date = parse_date(date, format = "%b %d, %Y") %>% 
           #subtract a quarter from the date
           floor_date("quarter") %m-% months(3) %>% 
           as.yearqtr(.),
         gdp = str_remove(gdp, "%") %>% as.numeric() / 100)


#Merging all the data sets
df_merged <- 
  df_gdp_tidy %>% 
  left_join(df_china_fund) %>% 
  drop_na()


#Plot
df_merged %>% 
  filter(date >= 2023) %>% 
  ggplot(aes(date)) + 
  geom_line(aes(y = gdp), 
            size =1.5,
            color = "darkorange") +
  geom_area(aes(y = gdp), 
            fill = "darkorange", 
            alpha = 0.7) +
  geom_point(aes(y = gdp), 
             size = 3,
             color = "darkorange") +
  geom_bar(aes(y = chn), 
           stat = "identity",
           fill = "steelblue",
           alpha = 0.7) +
  scale_y_continuous(labels = scales::percent, 
                     limits = c(-0.30,0.10)) +
  scale_x_yearqtr(format = "%Y Q%q" , n = 6) +
  labs(x="", y ="",
       title = "China's Economic Performance",
       subtitle = "<span style = 'color:darkorange;'>China Gross Domestic Product (GDP)</span> <br> <span style = 'color:steelblue;'>The China Fund, Inc</span> <br> (Quarterly) (YoY)") +
  theme_wsj(base_family = "Bricolage Grotesque") +
  theme(plot.subtitle = ggtext::element_markdown(size = 12, face = "bold"))
