library(dslabs)
library(dplyr)
library(lubridate)

data("reported_heights")

dat <- mutate(reported_heights, date_time = ymd_hms(time_stamp)) %>%
  filter(date_time >= make_date(2016, 01, 25) & date_time < make_date(2016, 02, 1)) %>%
  mutate(type = ifelse(day(date_time) == 25 & hour(date_time) == 8 & between(minute(date_time), 15,30),
                       "inclass", "online")) %>%
  select(sex, type)

y <- factor(dat$sex, c("Female", "Male"))
x <- dat$type

sum(x == "inclass")
sum(x == "online")

y_hat <- ifelse(x == "online", "Male", "Female") %>% factor(levels=levels(y))

mean(y_hat == y)

confusionMatrix(y_hat, y)

table(y_hat, y)

length(y_hat)
length(y)