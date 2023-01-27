library("readxl")
library("dplyr")
library("ggplot2")
library("grid")

data <- read_xlsx("journal.pbio.3001733.s008.xlsx", sheet = "S3_Data_Fig3B")

#Data between 2001 and 2016
min(data$donation_date)
max(data$donation_date)

#approx 250 for pre,post,and DST. 2200 regular
table(data$Type_of_week)

#Fig 3b. Alle means zu hoch
data %>% group_by(Type_of_week) %>% summarize(mean = log(mean(daily_donations_mean)))


#sieht nicht schlecht aus, aber der Tail ist zu heavy. Ich glaube der density plot wurde mit den 
#Daten auf Individual-Level gemacht, nicht mit daily averages (siehe Dichte unter 1)
ggplot(data,aes(x = log(daily_donations_mean))) + geom_density(fill = "lightgreen")

#mean ist komplett daneben 122 statt gewünschten 82. Fehlt hier das Processing auf den Daten?
mean(data$daily_donations_mean)
#3*sd abschneiden auf den normalen Daten reicht nicht ganz, aber sieht näher am Original aus. 
#Das hat ja aber nichts mit den 100k per individual observation zu tun, die genannt wurden
cut = mean(data$daily_donations_mean) + 3*sd(data$daily_donations_mean)

#Intuitives outlier removal verändert den mean kaum, nimmt aber 75 Datenpunkte raus.
mean(data$daily_donations_mean[-which(data$daily_donations_mean > cut)])
#Das sind ungefähr die 2%, die im Paper angegeben werden
sum(data$daily_donations_mean > cut)/nrow(data)

#Unintuitives outlier removal: mean berechnen nach der Log-transformation. Ungewöhlich, aber führt zu Ergebnissen, die näher am Original sind
exp(mean(log(data$daily_donations_mean[-which(data$daily_donations_mean > cut)])))


###Plot C
#Wurde mit diesen Daten outlier removal durchgeführt? Muss das noch passieren? 
data_c1 <- read_excel("journal.pbio.3001733.s008.xlsx", sheet = "S3_Data_Fig3C_top")
data_c2 <- read_excel("journal.pbio.3001733.s008.xlsx", sheet = "S3_Data_Fig3C_bottom")
#Verschieben um die relevante Woche, damit es um 0 zentriert ist
w_dst = data_c1[data_c1$Type_of_week == "DST_Week",]$WEEK[1]
w_st = data_c2[data_c2$Type_of_week == "ST_Week",]$WEEK[1]

p1 <- data_c1 %>% group_by(WEEK) %>% summarize(mean = log(mean(daily_donations_mean))) %>% 
  ggplot(aes(x = WEEK-w_dst,y = mean)) + geom_line()
p2 <- data_c2 %>% group_by(WEEK) %>% summarize(mean = log(mean(daily_donations_mean))) %>% 
  ggplot(aes(x = WEEK-w_st,y = mean)) + geom_line()

#Auch in Plot c) sind alle Werte zu hoch. 
grid.newpage()
grid.draw(rbind(ggplotGrob(p1), ggplotGrob(p2), size = "last"))