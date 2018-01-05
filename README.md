# election_machine_data
This aggregates data from polling machines and analyzes the time taken to cast votes

#######################################################################################################################
## Objective is to clean the data from the Riverside county voting machines                                          ##
## we want to analyze the times of day that votes were cast                                                          ##
## we want to visualize the time taken to cast votes                                                                 ##
## keep the serial and counter nubmers because not all the machines had the same number of contest on the ballot     ##
## we may break it down by that factor later                                                                         ##
#######################################################################################################################

library(dplyr)
library(lubridate)
library(Hmisc)
library(ggplot2)
library(tidyverse)
library(readr)
library(stringr)
library(tidyr)
library(plotly)

setwd("E:/JordanBerninger/riverside_touch_screen")

dat <- read_table("Log_RIV_20161108.txt", col_names = FALSE)

data <- dat %>% separate(X1, into = c("box.serial", "counter.number", "junk", "date", "time"), sep = ",") %>% 
  separate(X2, into = c("time2", "junk2", "action"), sep = '"') %>%
  mutate(date = as.Date(substring(date, 2, 11), format = "%m/%d/%Y")) %>%
  mutate(time = substring(time, 2)) %>% 
  dplyr::select("box.serial", "counter.number", "action", "date", "time", "time2")

################################################
## this is clean data set ^                   ##
## trim it down to just the relevant actions  ##
################################################

data2 <- data %>% filter(action %in% c("Election - Activate", "Election - Cast Vote")) %>%
  filter(date == "2016-11-08")

# there were 588,119 rows before the date filter, 16,060 rows after

# table(data2$box.serial, data2$counter.number) %>% View()

###################################################################################################################################
## check out the frequencies of serial number and counter                                                                        ##
## need a way to connect the machine activation to the vote cast                                                                 ##
## not a perfect 1:1                                                                                                             ##
## example of a break: data2 %>% filter(box.serial == "42485" & counter.number ==  "3398") %>% View()                            ##
## potentiall filter just for the ones that have a 1:1 pair?                                                                     ##
## or grab all the votes cast, then grab one (first) of the activation actions with the same box.serial and counter.number       ##
###################################################################################################################################


###############################################################################################################################################
## not sure if this is correct bc th activate happens after the vote cast event which shouldnt be the case                                   ##
## there are some outliers that need to be removed anyway                                                                                    ##
## also need to filter out for just election day (11/08/16)                                                                                  ##
## its possible that the counter number increases after someone activates, so the "person" level action is split across 2 counter numbers    ##
###############################################################################################################################################

dat.activate <- data2 %>% filter(action == "Election - Activate") %>%
  mutate(counter.number = as.character(as.numeric(counter.number) + 1)) %>% 
  mutate(match.key = paste(box.serial, counter.number, sep = " "))

dat.vote <- data2 %>% filter(action == "Election - Cast Vote") %>% 
  mutate(match.key = paste(box.serial, counter.number, sep = " "))

dat.vote.activate <- left_join(dat.vote, distinct(dat.activate, match.key, .keep_all = T), by = "match.key")

dat.vote.activate <- dat.vote.activate %>% mutate(time.cast = 
                       as.difftime(time.x, format = "%H:%M:%S", units = "mins") - as.difftime(time.y, format = "%H:%M:%S", units = "mins")) %>%
                       mutate(hour = substring(time.y, 1, 2))

# ok things look much better now
# basic summary statistics

votes <- dat.vote.activate %>% select(1, 2, 3, 4, 5, 6, 7, 10, 12, 14, 15) %>% 
                      mutate(time.cast = as.numeric(time.cast)) %>%
                      mutate(hour = factor(hour, levels = c("07", "08", "09", "10", "11", "12", "01", "02", "03", "04", "05", "06"), ordered = TRUE))

###########################################################################################
## need to correct for the ones around noon (large negative values for time.cast)        ##
## 5 cases took longer than 100 mins - these should be removed as outliers               ##
## needed to convert hour into an ordered factor variable for the vis                    ##
###########################################################################################

noon.fix <- votes %>% filter(time.cast < 0) %>%
                  mutate(time.y = gsub("12", "00", time.y)) %>% 
                  mutate(time.cast = as.difftime(time.x, format = "%H:%M:%S", units = "mins") - as.difftime(time.y, format = "%H:%M:%S", units = "mins")) %>%
                  mutate(hour = 12)

# now, rbind votes and noon.fix, then filter out the negative values

votes2 <- rbind(votes, noon.fix) %>% filter(time.cast > 0) %>% filter(time.cast < 100)

###################################################
## We have data on 7,666 votes cast              ##
## 2,851 voted in the AM, 4,815 voted in the PM  ##
## votes2 now has 7666 rows (1 for each vote)    ##
## votes2 is cleaned enough for the analysis     ##
## grab the same visualization from SCruz        ##
###################################################

summary(votes2$time.cast)
# this has the median, mean, quantiles (note outliers were removed)

p1 <- ggplot(data = votes2, aes(x = time.cast, fill = "blue")) +
  geom_histogram(stat = 'bin', bins = 50, fill = "blue")
ggplotly(p1)
# histogram of time to cast votes

p2 <- ggplot(data = votes2, aes(x = time.cast, fill = "blue")) + 
  geom_histogram(stat = 'bin', bins = 50, fill = "blue") + 
  facet_grid(~time2.x)
ggplotly(p2)
# histogram of time to cast votes, split by morning and afternoon

p3 <- ggplot(data = votes2, aes(x = hour, fill = "blue")) + geom_bar()
ggplotly(p3)
# barchart of total votes cast in each hour block

p4 <- ggplot(data = votes2, aes(x = hour, fill = "blue")) + geom_bar(aes(y = (..count..)/sum(..count..)))
ggplotly(p4)
# barchart of percentage of votes cast in each hour block

################################################################################################
## Ben wanted more insight on the 'bad' data that was filtered out                            ##
## refer to the opening data frame and make a frequency table of the dates and votes cast     ##
################################################################################################

cast.votes <- data %>% dplyr::filter(action == "Election - Cast Vote")

bad.data <- table(cast.votes$date) %>% data.frame() %>% dplyr::select("date" = 1, "votes_cast" = 2)

