library(cricketdata)
library(dplyr)
library(purrr)
library(ggplot2)
source("data/AdjStatsMaker.R")
cric1 = fetch_cricsheet(type = "bbb", competition = "ipl", gender = "male")
head(cric1)
#running just on 2023 ipl w/out player considerations
ipl2023 <- cric1 %>% filter(season == "2023")
ipl2023_eff <- adjusted_efficiency(ipl2023)
bat.eff <- ipl2023_eff[[1]]
bowl.eff <- ipl2023_eff[[2]]

ipl2023_avg <- adjusted_average(ipl2023)
bat.avg <- ipl2023_avg[[1]]
#bat.avg <- subset(bat.avg, select = -c(balls, strike_rate))
bowl.avg <- ipl2023_avg[[2]]


bat_data <- bat.eff %>% left_join(bat.avg, on="striker")
bowl_data <- bowl.eff %>% left_join(bowl.avg, on="bowler")


dir.create("Data/IPL")
dir.create("Data/IPL/2023")
bat_data %>% write.csv('Data/IPL/2023/bat_data.csv')
bowl_data %>% write.csv('Data/IPL/2023/bowl_data.csv')
