library(tidyverse)
r4ds <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-07-16/r4ds_members.csv")
head(r4ds)
summary(r4ds)

#Total membership over time
ggplot(data = r4ds) +
  geom_smooth(mapping = aes(x = date, y = total_membership)) + 
  geom_smooth(mapping = aes(x = date, y = daily_active_members)) +
  geom_smooth(mapping = aes(x = date, y = weekly_active_members))

#daily active members over time
ggplot(data = r4ds) +
  geom_line(mapping = aes(x = date, y = daily_active_members))

#Add column for days of the week
r4ds <- mutate(r4ds, weekday = weekdays(as.Date(date,'%Y-%m-%d')))

#Determine mean number of users per weekday
mean_members_per_day <- r4ds %>%
   group_by(weekday) %>%
   summarize(mean(daily_active_members))

 ggplot(data = r4ds) + 
   geom_boxplot(mapping = aes(x = weekday, y = daily_active_members, fill = weekday)) +  
   geom_boxplot(mapping = aes(x = weekday, y = messages_posted, fill = weekday))
           
(mean_messages_per_day <- r4ds %>%
   group_by(weekday) %>%
   summarize(mean(messages_posted)))  
 
# Determine messages per posting member per day 
 r4ds <- mutate(r4ds, messages_per_user = messages_posted/daily_members_posting_messages)

 r4ds %>%
   group_by(weekday) %>%
   summarize(mean(messages_per_user))

 select(r4ds, weekday, messages_posted, daily_active_members, daily_members_posting_messages, messages_per_user) 
 