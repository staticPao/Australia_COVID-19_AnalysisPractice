#COMP 5070
#Zihao Huang

library(ggplot2)
library(dplyr)

#Load data,the data file need in the current working directory
load(file = "examdata.RData")
#print(getwd())

#Avoid scientific notation in charts
options(scipen = 10)

Sys.setlocale("LC_TIME","English")

###Question 1. ¡°New¡± pandemic of COVID-19

all_cases <- filter(all_cases,!is.na(NEW))
# Extract data for each state from the summary table(all_cases)
vic_cases <- subset(all_cases,STATE =="vic")
nsw_cases <- subset(all_cases,STATE =="nsw")
wa_cases <- subset(all_cases,STATE =="wa")

#Reverse the row order, the earliest date as the first row
vic_cases <- as.data.frame(apply(vic_cases, 2, rev))
nsw_cases <- as.data.frame(apply(nsw_cases, 2, rev))
wa_cases <- as.data.frame(apply(wa_cases, 2, rev))

#Calculate daily cumulative cases based on daily new cases (NEW) by using cumulative
vic_cases$CASES <- cumsum(vic_cases$NEW)
nsw_cases$CASES <- cumsum(nsw_cases$NEW)
wa_cases$CASES <- cumsum(wa_cases$NEW)

#At this time, the date of the table starts from the earliest. In order to facilitate merging with other tables later, 
#need to reverse the order againand use the latest date as the first row
#And restore the data type of each column
vic_cases <- as.data.frame(apply(vic_cases, 2, rev))
vic_cases$NEW <-  as.numeric(vic_cases$NEW)
vic_cases$CASES <-  as.numeric(vic_cases$CASES)
vic_cases$DATE <- as.Date(vic_cases$DATE)

nsw_cases <- as.data.frame(apply(nsw_cases, 2, rev))
nsw_cases$NEW <-  as.numeric(nsw_cases$NEW)
nsw_cases$CASES <-  as.numeric(nsw_cases$CASES)
nsw_cases$DATE <- as.Date(nsw_cases$DATE)

wa_cases <- as.data.frame(apply(wa_cases, 2, rev))
wa_cases$NEW <-  as.numeric(wa_cases$NEW)
wa_cases$CASES <-  as.numeric(wa_cases$CASES)
wa_cases$DATE <- as.Date(wa_cases$DATE)

all_cases <- rbind(vic_cases,nsw_cases,wa_cases)

#Mapping the development of daily new case counts in three states over time
ggplot(all_cases , aes(x = DATE, y = NEW, group = STATE, col = STATE)) + 
  ggtitle("Q1.Development of daily new cases for all periods") +
  geom_line(size = 0.8,na.rm = TRUE) + theme_bw() + labs(x = "Date",y = "New cases")
#Mapping the development of cumulative case counts in three states over time
ggplot(all_cases , aes(x = DATE, y = CASES, group = STATE, col = STATE)) + 
  ggtitle("Q1.Development of cumulative cases for all periods") +
  geom_line(size = 0.8,na.rm = TRUE) + theme_bw() + labs(x = "Date",y = "Cumulative cases")

#Define the range of the latest wave according to the trend shown above
cases_latest <- subset(all_cases,DATE>"2021-06-30")
#Mapping the evolution of daily new case counts in three states during the latest wave
ggplot(cases_latest , aes(x = DATE, y = NEW, group = STATE, col = STATE)) + 
  ggtitle("Q1.Development of daily new cases under the latest wave\nFrom 2021-07 to latest") +
  geom_line(size = 0.8,na.rm = TRUE) + theme_bw() + labs(x = "Date",y = "New cases")
#Mapping the evolution of cumulative case counts in three states during the latest wave
ggplot(cases_latest , aes(x = DATE, y = CASES, group = STATE, col = STATE)) + 
  ggtitle("Q1.Development of cumulative cases under the latest wave\nFrom 2021-07 to latest") +
  geom_line(size = 0.8,na.rm = TRUE) + theme_bw() + labs(x = "Date",y = "Cumulative cases")


#In order to compare the similarities and differences between the overall trajectories of cases in the three states, 
#it is feasible to use "cases per thousand".
#number of cases per 1000 people = (number of cases * 1000) / population
#this can be used to reflect new and cumulative cases

#Calculate the number of new cases per 1,000 people separately for each state
vic_cases$NEW_PRE_1000 <- (vic_cases$NEW * 1000)/6643100
nsw_cases$NEW_PRE_1000 <- (nsw_cases$NEW * 1000)/8186800
wa_cases$NEW_PRE_1000 <- (wa_cases$NEW * 1000)/2685200

#Calculate how many cumulative cases per 1,000 people for each state separately
vic_cases$CASES_PRE_1000 <- (vic_cases$CASES * 1000)/6643100
nsw_cases$CASES_PRE_1000 <- (nsw_cases$CASES * 1000)/8186800
wa_cases$CASES_PRE_1000 <- (wa_cases$CASES * 1000)/2685200

cases_pre <- rbind(vic_cases,nsw_cases,wa_cases)
cases_pre_latest <- subset(cases_pre,DATE>"2021-06-30")

#Mapping case comparisons in three states during the latest wave(per 1000 people)
#New cases
ggplot(cases_pre_latest , aes(x = DATE, y = NEW_PRE_1000, group = STATE, col = STATE)) + 
  ggtitle("Q1.Daily new cases per 1,000 people under the latest wave\nFrom 2021-07 to latest") +
  geom_line(size = 0.8,na.rm = TRUE) + theme_bw() + labs(x = "Date",y = "Daily new cases(pre 1,000)")
#Cumulative cases
ggplot(cases_pre_latest , aes(x = DATE, y = CASES_PRE_1000, group = STATE, col = STATE)) + 
  ggtitle("Q1.Cumulative new cases per 1,000 people under the latest wave\nFrom 2021-07 to latest") +
  geom_line(size = 0.8,na.rm = TRUE) + theme_bw() + labs(x = "Date",y = "Cumulative cases(pre 1,000)")


###Question 2. Vaccination rates

#Merge vaccine table and case table with two columns of information based on state and date
vaccinations_cases <- merge(all_vaccinations,all_cases,by = c("STATE","DATE"),all = FALSE)
#Convert Date to week (week of the year)
vaccinations_cases$WEEK <- format(vaccinations_cases$DATE,'%Y-%W')

vaccinations_cases <- vaccinations_cases[,c("STATE","WEEK","NET","NEW","CASES")]
vaccinations_cases$WEEK[which(vaccinations_cases$WEEK =="2022-00")] <- "2021-52"

#Group by state and week, and calculate total vaccinations and average cases for each group
options(dplyr.summarise.inform = FALSE)
weekly_vaccinations_cases <- vaccinations_cases%>% group_by(STATE,WEEK) %>% 
  summarise(SUM_DOSE_NET = sum(NET),SUM_NEW = sum(NEW),SUM_CASE = sum(CASES))

#Extract data for three states separately based on weekly_vaccinations_cases
vic_weekly_vc <- subset(weekly_vaccinations_cases,STATE =="vic")
nsw_weekly_vc <- subset(weekly_vaccinations_cases,STATE =="nsw")
wa_weekly_vc <- subset(weekly_vaccinations_cases,STATE =="wa")

#Add serial numbers to each week for plotting
vic_weekly_vc$WEEK_NUM<-1:nrow(vic_weekly_vc)
nsw_weekly_vc$WEEK_NUM<-1:nrow(nsw_weekly_vc)
wa_weekly_vc$WEEK_NUM<-1:nrow(wa_weekly_vc)

#Plot showing the distribution of weekly totals by state in relation to vaccination
#Victoria doses administered and new cases
ggplot(vic_weekly_vc) + 
  geom_line(aes(x=WEEK_NUM, y=SUM_DOSE_NET), color = "blue",size = 0.8,na.rm = TRUE) +
  geom_line(aes(x=WEEK_NUM, y=SUM_NEW), color = "orange",size = 0.8,na.rm = TRUE) +
  ggtitle("Q2.Weekly case distribution and vaccinations in VIC") +
  labs(x = "Week (week number) ",y = "Cases") + scale_x_continuous(breaks=seq(0,nrow(vic_weekly_vc),10))
  
#New South Wales doses administered and new cases
ggplot(nsw_weekly_vc) + 
  geom_line(aes(x=WEEK_NUM, y=SUM_DOSE_NET), color = "blue",size = 0.8,na.rm = TRUE) +
  geom_line(aes(x=WEEK_NUM, y=SUM_NEW), color = "orange",size = 0.8,na.rm = TRUE) +
  ggtitle("Q2.Weekly case distribution and vaccinations in NSW") +
  labs(x = "Week (week number) ",y = "Cases") + scale_x_continuous(breaks=seq(0,nrow(nsw_weekly_vc),10))

#West Australia doses administered and new cases
ggplot(wa_weekly_vc) + 
  geom_line(aes(x=WEEK_NUM, y=SUM_DOSE_NET), color = "blue",size = 0.8,na.rm = TRUE) +
  geom_line(aes(x=WEEK_NUM, y=SUM_NEW), color = "orange",size = 0.8,na.rm = TRUE) +
  ggtitle("Q2.Weekly case distribution and vaccinations in WA") +
  labs(x = "Week (week number) ",y = "Cases") + scale_x_continuous(breaks=seq(0,nrow(wa_weekly_vc),10))


###Question 3. Hospitalisations 

#Merge a table containing hospitalized cases and daily active cases based on two columns of information, state and date
hospital_active <- merge(all_hospitalised, all_active,by = c("STATE","DATE"),all = FALSE)
#Calculate the scale and store in a new column
hospital_active$PRO <- (hospital_active$HOSP / hospital_active$ACTIVE) * 100
hospital_active$PRO <- round(hospital_active$PRO,2)
#get week by date
hospital_active$WEEK <- format(hospital_active$DATE,'%Y-%W')
hospital_active$WEEK[which(hospital_active$WEEK =="2021-00")] <- "2020-52"
hospital_active$WEEK[which(hospital_active$WEEK =="2022-00")] <- "2021-52"

hospital_active <- filter(hospital_active,!is.na(PRO),!is.infinite(PRO))

#Group by state and week, and calculate the mean of each group's proportion
weekly_hospital_active <- hospital_active %>% group_by(STATE,WEEK) %>% 
  summarise(MEAN_PRO = mean(PRO))
  
#Extract data for three states based on the table above
vic_ha <- subset(hospital_active,STATE =="vic")
nsw_ha <- subset(hospital_active,STATE =="nsw")
wa_ha <- subset(hospital_active,STATE =="wa")
vic_weekly_ha <- subset(weekly_hospital_active,STATE =="vic")
nsw_weekly_ha <- subset(weekly_hospital_active,STATE =="nsw")
wa_weekly_ha <- subset(weekly_hospital_active,STATE =="wa")

#Add serial number to week
vic_weekly_ha$WEEK_NUM<-1:nrow(vic_weekly_ha)
nsw_weekly_ha$WEEK_NUM<-1:nrow(nsw_weekly_ha)
wa_weekly_ha$WEEK_NUM<-1:nrow(wa_weekly_ha)

#Plot can reflect the relationship between admissions and daily active cases (daily and weekly)
#Victoria
ggplot(vic_ha) + 
  geom_line(aes(x=DATE, y=PRO), color = "blue",size = 0.8,na.rm = TRUE) + 
  ggtitle("Q3.Proportion of hospitalized cases and daily active cases in VIC") +
  labs(x = "Date ",y = "Proportion( % )")
ggplot(vic_weekly_ha) + 
  geom_line(aes(x=WEEK_NUM, y=MEAN_PRO), color = "red",size = 0.8,na.rm = TRUE) + 
  ggtitle("Q3.Weekly proportion of hospitalized cases and daily active cases in VIC") +
  labs(x = "Week ",y = "Proportion( % )")

#New South Wales
ggplot(nsw_ha) + 
  geom_line(aes(x=DATE, y=PRO), color = "blue",size = 0.8,na.rm = TRUE) + 
  ggtitle("Q3.Proportion of hospitalized cases and daily active cases in NSW") +
  labs(x = "Date ",y = "Proportion( % )")
ggplot(nsw_weekly_ha) + 
  geom_line(aes(x=WEEK_NUM, y=MEAN_PRO), color = "red",size = 0.8,na.rm = TRUE) + 
  ggtitle("Q3.Weekly proportion of hospitalized cases and daily active cases in NSW") +
  labs(x = "Week ",y = "Proportion( % )")

#West Australia
ggplot(wa_ha) + 
  geom_line(aes(x=DATE, y=PRO), color = "blue",size = 0.8,na.rm = TRUE) + 
  ggtitle("Q3.Proportion of hospitalized cases and daily active cases in WA") +
  labs(x = "Date ",y = "Proportion( % )")
ggplot(wa_weekly_ha) + 
  geom_line(aes(x=WEEK_NUM, y=MEAN_PRO), color = "red",size = 0.8,na.rm = TRUE) + 
  ggtitle("Q3.Weekly proportion of hospitalized cases and daily active cases in WA") +
  labs(x = "Week ",y = "Proportion( % )")



