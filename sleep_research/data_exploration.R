library("readxl")
library("dplyr")
library("ggplot2")
library("grid")
library("emmeans")

data <- read_xlsx("journal.pbio.3001733.s008.xlsx", sheet = "S3_Data_FigS2")

#Density plot sieht gut aus, wobei ich immer noch etwas verwirrt bin, dass im Original Masse unter 1 liegt
ggplot(data,aes(x = log(daily_donations_mean))) + geom_density(fill = "lightgreen")


data2 <- read_xlsx("journal.pbio.3001733.s008.xlsx", sheet = "S3_Data_Fig3B")
#mean ist komplett daneben 122 statt gewünschten 82. Processing sei allerdings schon passiert
mean(data2$daily_donations_mean)

lm3 <- glm(log(daily_donations_mean) ~ Type_of_week+Time_of_Day+ Weekend_day+ YEAR +as.factor(MONTH)+as.factor(DAY_OF_WEEK)+Holiday, family="gaussian", data=data2)
#Damit kriegen wir tatsächlich die korrekten marginal means. Auch die Signifikanz passt
emmeans(lm3, ~ Type_of_week)
summary(lm3)

lm1 <- lm(log(daily_donations_mean) ~ Type_of_week+Weekend_day + YEAR +as.factor(MONTH)+as.factor(DAY_OF_WEEK)+Holiday, data=data2)
#r-sq 0.3719 (ganzes model) oder 0.36 (ohne time_of_day)
summary(lm1)


###Plot C
#Mit selber Strategie wie zuvor kommen wir hier voran. Emmeans berechnen und die CIs davon nutzen. Das sieht gut aus
data_c1 <- read_excel("journal.pbio.3001733.s008.xlsx", sheet = "S3_Data_Fig3C_top")
data_c2 <- read_excel("journal.pbio.3001733.s008.xlsx", sheet = "S3_Data_Fig3C_bottom")
#Verschieben um die relevante Woche, damit es um 0 zentriert ist
w_dst = data_c1[data_c1$Type_of_week == "DST_Week",]$WEEK[1]
w_st = data_c2[data_c2$Type_of_week == "ST_Week",]$WEEK[1]

lm4 <- glm(log(daily_donations_mean) ~ as.factor(WEEK), family="gaussian", data=data_c1)
df_3c_up <- summary(emmeans(lm4, ~ WEEK))
lm5 <- glm(log(daily_donations_mean) ~ as.factor(WEEK), family="gaussian", data=data_c2)
df_3c_down <- summary(emmeans(lm5, ~ WEEK))

data_c1_transf <- data_c1 %>% mutate(weektype = WEEK == w_dst)
lm6 <- glm(log(daily_donations_mean) ~ weektype, family="gaussian", data=data_c1_transf)
summary(lm6)
data_c2_transf <- data_c2 %>% mutate(weektype = WEEK == w_st)
lm7 <- glm(log(daily_donations_mean) ~ weektype, family="gaussian", data=data_c2_transf)
summary(lm7)

p1 <- df_3c_up %>% 
  ggplot(aes(x = WEEK-w_dst,y = emmean)) + geom_line() +  
  geom_ribbon(aes(ymin = emmean, ymax = upper.CL), alpha = 0.1) +
  ylim(4.3,4.7)
p2 <- df_3c_down %>% 
  ggplot(aes(x = WEEK-w_st,y = emmean)) + geom_line() +  
  geom_ribbon(aes(ymin = emmean, ymax = upper.CL), alpha = 0.1) + 
  ylim(4.15,4.55)
grid.newpage()
grid.draw(rbind(ggplotGrob(p1), ggplotGrob(p2), size = "last"))


#'p2 <- data_c2 %>% group_by(WEEK) %>% summarize(mean = log(mean(daily_donations_mean))) %>% 
#'  ggplot(aes(x = WEEK-w_st,y = mean)) + geom_line() + geom_ribbon(aes(ymin = low, ymax = high), alpha = 0.1)

library(lubridate)
temp <- data %>% mutate(dayofyear = yday(donation_date))

temp2 <- aggregate(log(daily_donations_mean)~dayofyear,FUN = mean,data = temp)
plot(as.Date("2001-01-01")+temp2$dayofyear,temp2$`log(daily_donations_mean)`,type="l")
abline(v = as.Date("2001-03-15"),col = "red")
abline(v = as.Date("2001-11-15"),col = "blue")

x = as.Date("2001-01-01")

temp3 <- data %>% mutate(year = year(donation_date))
temp3 <- aggregate(log(daily_donations_mean)~year,FUN = mean,data = temp3)
plot(temp3$year,temp3$`log(daily_donations_mean)`,type="l")