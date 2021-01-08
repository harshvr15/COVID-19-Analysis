################################################################################
################################################################################
###                                                                          ###
###                  DATA VISUALIZATION ASSIGNMENT 2                         ###
###                    COVID-19 DATASET ANALYSIS                             ###
###                                                                          ###
################################################################################
################################################################################

#libraries to install
install.packages("bannerCommenter")
install.packages("readr")
install.packages("stringr")
install.packages("sqldf")
install.packages("plyr")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("plotly")
install.packages("lubridate")
install.packages("Hmisc")
install.packages("gmodels")
install.packages("visdat")
install.packages("viridis")
install.packages("scales")
install.packages("psych")
install.packages("xts")
install.packages("dygraphs")
install.packages("DT")


#libraries to load
library(bannerCommenter)
library(readr)
library(stringr)
library(gmodels)
library(visdat)
library(sqldf)
library(plyr)
library(dplyr)
library(ggplot2)
library(plotly)
library(lubridate)
library(Hmisc)
library(psych)
library(scales)
library(xts)
library(dygraphs)
library(viridis)
library(DT)


###########################################################################
###########################################################################
###                                                                     ###
###                 DATA INPUT, CLEANING & WRANGLING                    ###
###                                                                     ###
###########################################################################
###########################################################################


#Files were loaded to my github repository and I'm fetching the data directly from the URL

a= 'https://raw.githubusercontent.com/harshvr15/DataVisualisation2/main/country_wise_latest.csv'
country_wise=read.csv(a,na.strings = "")

b= 'https://raw.githubusercontent.com/harshvr15/DataVisualisation2/main/covid_19_clean_complete.csv'
covid=read.csv(b,na.strings = "")

c= 'https://raw.githubusercontent.com/harshvr15/DataVisualisation2/main/full_grouped.csv'
full_grouped=read.csv(c,na.strings = "")

d= 'https://raw.githubusercontent.com/harshvr15/DataVisualisation2/main/worldometer_data.csv'
world=read.csv(d,na.strings = "")


#dimensions of all files used 
dim(country_wise)
dim(covid)
dim(full_grouped)
dim(world)


##previewing the data structure of all the files str() & summary()
str(country_wise)

detach("package:psych", unload = TRUE)

describe(country_wise$Country)
describe(country_wise$WHO_Region)

detach("package:Hmisc", unload = TRUE)
library(psych)

describe(country_wise$Confirmed)
describe(country_wise$Deaths)
describe(country_wise$Recovered)
describe(country_wise$Active)
describe(country_wise$New.cases)
describe(country_wise$New.deaths)
describe(country_wise$New.recovered)
describe(country_wise$DeathPer100)
describe(country_wise$RecoverPEr100)
describe(country_wise$DeathPer100Recover)
describe(country_wise$ConfirmedPrevWeek)
describe(country_wise$WeekChange)
describe(country_wise$WeekChangePerc)


str(covid)

str(full_grouped)

detach("package:psych", unload = TRUE)
library(Hmisc)

str(world)


#Changing name of the columns for better readability
names(world)[1]="Country"
names(world)[11]="SeriousCondition"
names(world)[12]="CasesPerMillion"
names(world)[13]="DeathsPerMillion"
names(world)[15]="TestsPerMillion"
names(world)[16]="WHO_Region"

names(country_wise)[1]="Country"
names(country_wise)[9]="DeathPer100"
names(country_wise)[10]="RecoverPEr100"
names(country_wise)[11]="DeathPer100Recover"
names(country_wise)[12]="ConfirmedPrevWeek"
names(country_wise)[13]="WeekChange"
names(country_wise)[14]="WeekChangePerc"
names(country_wise)[15]="WHO_Region"

names(covid)[1]="State"
names(covid)[2]="Country"
names(covid)[10]="WHO_Region"

names(full_grouped)[2]="Country"
names(full_grouped)[10]="WHO_Region"


#Checking for the missing vlaues in all the files
apply(is.na(country_wise),2,sum)
apply(is.na(covid),2,sum)
apply(is.na(full_grouped),2,sum)
apply(is.na(world),2,sum)

#visualization related to the missing values
vis_miss(world)
vis_miss(covid)

#Removing a column"Province"  as we already have Country as an identifier
covid = covid[-c(1)]

#Removing these columns as they were present in another dataframe
world = world[-c(5,7,9,11)]
full_grouped = full_grouped[-c(3,4,5,6,10)]
covid = covid[-c(9)]

#Converting datatypes -  char to Date 
covid$Date = as.Date(covid$Date)
full_grouped$Date = as.Date(full_grouped$Date)

#Filling the NA with "-" as nothing can be done for these missing values
world$Continent[is.na(world$Continent)]="-"
world$WHO_Region[is.na(world$WHO_Region)]="-"

#Checking for the country with NA population
sqldf("select Country,Population from world where Population is NULL ")
#As it's data can't be extracted filling it with 0
world$Population[is.na(world$Population)]=0

#Rest of the variables constitute less percentage of NA so we can fill them with 0
world[is.na(world)]=0

covid = covid%>%
  mutate(month = month(Date))

#Exploring the cases trend for four countries in news SouthKorea,China,Italy,Iran
Explore1 = covid%>%
  filter(Country %in% c("South Korea","China","Italy", "Iran"))%>%
  group_by(month,Country)%>%
  summarise(Confirmed=sum(Confirmed),Deaths= sum(Deaths),Recovered = sum(Recovered),Active = sum(Active))

ggplot(Explore1,aes(month))+
  geom_point(aes(y=Recovered,color = "green"),size=1)+
  geom_point(aes(y=Active,color = "darkblue"),size=1)+
  geom_point(aes(y=Deaths,color = "red"),size=1)+
  geom_point(aes(y=Confirmed,color = "brown"),size=1)+ facet_grid(~Country)+
  ggtitle("Cases over Time")+
  labs(y="Count of Cases",x = "Month")+
  scale_color_identity( name = "Types of Cases",
                        breaks = c("green","red","brown","darkblue"),
                        labels = c("Recovered","Death","Confirmed","Active"),
                        guide = "legend")+
  theme(axis.title.x = element_text(colour = "red"),axis.title.y = element_text(colour = "red"),legend.position = "right")


#Checking distribution of the categorical variable
CrossTable(country_wise$WHO_Region)


##Based on number of confirmed cases dividing the countries into 4 zones

summary(country_wise$Confirmed)

#creating subset with countries and their confirmed cases
confirmed_cases = country_wise%>%
  select(Country,Confirmed)%>%
  arrange(desc(Confirmed))

#mutate a column Zones depending on the number of cases with some conditions
confirmed_cases = confirmed_cases%>%
  mutate(Zones= if_else(Confirmed > 1000000, "Red",if_else(Confirmed > 100000,"Orange",if_else(Confirmed > 10000,"Yellow",if_else(Confirmed < 9999,"Green","Low")))))

#Description of various zones created  
CrossTable(confirmed_cases$Zones)

#Zonewise barplot
confirmed_cases%>%
  ggplot(aes(x=Zones))+geom_bar()

##Based on number of death cases dividing the countries into 4 zones

summary(country_wise$Deaths)

#creating subset with countries and their death cases
death_cases = country_wise%>%
  select(Country,Deaths)%>%
  arrange(desc(Deaths))

#mutate a column Zones depending on the number of cases with some conditions
death_cases = death_cases%>%
  mutate(DZones= if_else(Deaths > 50000, "Red",if_else(Deaths > 10000,"Orange",if_else(Deaths > 1000,"Yellow",if_else(Deaths > 500,"Green","Low")))))

#Description of various zones created  
CrossTable(death_cases$DZones)

#Zonewise barplot
death_cases%>%
  ggplot(aes(x=DZones))+geom_bar()

##Based on number of active cases dividing the countries into 4 hotspots

summary(country_wise$Active)

#creating subset with countries and their confirmed cases
active_cases = country_wise%>%
  select(Country,Active)%>%
  arrange(desc(Active))

#mutate a column Zones depending on the number of cases with some conditions
active_cases = active_cases%>%
  mutate(Hotspot= if_else(Active > 100000, "Red",if_else(Active > 10000,"Orange",if_else(Active > 1000,"Yellow",if_else(Active < 1000,"Green","Low")))))

#Description of various zones created  
CrossTable(active_cases$Hotspot)

#Zonewise barplot
active_cases%>%
  ggplot(aes(x=Hotspot))+geom_bar()

#Calculating Death% and Recovery% and mutate
world = world %>% mutate(Deathperc = round((TotalDeaths/TotalCases)*100,digits =2))
world = world %>% mutate(Recoverperc = round((TotalRecovered/TotalCases)*100,digits =2))

#calculating top10 countries with max deathperc with PLOT
deathper = world%>%
  select(Country,Deathperc,Population,DeathsPerMillion)%>%
  arrange(desc(Deathperc))%>%
  slice_max(Deathperc,n = 10)
ggplot(deathper,aes(x= Country, y=Deathperc))+geom_point()+scale_x_discrete(guide = guide_axis(angle = 90))

#Applying certain conditions for a better output
deathper1 = world%>%
  select(Country,Deathperc,Population,DeathsPerMillion)%>%
  filter(DeathsPerMillion >400 & Population > 10000000)%>%
  slice_max(Deathperc,n = 10)
ggplot(deathper1,aes(x=Country,y=Deathperc))+geom_bar(stat = "identity")+scale_x_discrete(guide = guide_axis(angle = 90))

#calculating top10 countries with max recoveryperc with PLOT
recoverper=world%>%
  select(Country,Recoverperc,Population)%>%
  slice_max(Recoverperc,n=10)

#applying conditions to the above df
recoverper1=world%>%
  select(Country,Recoverperc,Population)%>%
  filter(Population > 50000000 & Recoverperc > 90)%>%
  slice_max(Recoverperc,n=10)
ggplot(recoverper1,aes(x=Country,y=Recoverperc))+geom_point(color = "red")

worldstats_table = world %>%
  select(Country,CasesPerMillion,TestsPerMillion,DeathsPerMillion)
datatable(worldstats_table,options = list(pageLength = 5))

###########################################################################
###########################################################################
###                                                                     ###
###                             VISUALISATIONS                          ###
###                                                                     ###
###########################################################################
###########################################################################

#PLOT1
#Logarithmic chart for Confirmed,Deaths,Recovered,Active. cases for all countries.

a= sqldf('select Country, Date,Confirmed, Deaths, Recovered, Active from covid group by Date, Country')
q=a %>%
  group_by(Date)%>%
  summarise(Confirmed=sum(Confirmed),Deaths=sum(Deaths),Active=sum(Active),Recovered=sum(Recovered))

b=ggplot(q,aes(Date))+geom_point(aes(y=Recovered,color = "green"),size=1)+geom_point(aes(y=Active,color = "darkblue"),size=1)+geom_point(aes(y=Deaths,color = "red"),size=1)+geom_point(aes(y=Confirmed,color = "cyan"),size=1)
b+scale_y_continuous(trans = 'log2')+annotation_logticks()+
  ggtitle("Cases over Time")+
  labs(y="Count of Cases",x = "Month")+
  scale_color_identity( name = "Types of Cases",
                        breaks = c("green","red","cyan","darkblue"),
                        labels = c("Recovered","Death","Confirmed","Active"),
                        guide = "legend")+
  theme(axis.title.x = element_text(colour = "red"),axis.title.y = element_text(colour = "red"),legend.position = "right")

#PLOT2
#Graphical representation of confirmed cases across the globe

a = covid%>%
  select(Country,Confirmed)%>%
  group_by(Country)%>%
  summarise(Confirmed = sum(Confirmed))%>%
  arrange(desc(Confirmed))

a$Country= gsub("US","USA",a$Country)
a = a %>% mutate(Country = case_when(str_detect(Country,"Congo")~"Democratic Republic of the Congo",TRUE ~ Country))


w = map_data('world')
ggplot(a,aes(map_id= Country))+
  geom_map(aes(fill=Confirmed,colour = z1),map=w,color = "black")+ggtitle("Confirmed COVID cases over the globe")+
  labs(y="Longitude",x = "Latitude")+
  #scale_fill_gradient(high = "red",low = "green",labels = comma,n.breaks=8,guide = "colourbar")+
  scale_fill_viridis_c(option = 'viridis',trans = 'reverse',labels = comma,n.breaks=11,guide = "colourbar")+
  expand_limits(x= w$long,y=w$lat)
#theme(panel.background = element_rect(fill = "#2C3E4F",colour = "red"))


#PLOT3

a = full_grouped %>%
  select(Country,Date,New.cases,New.deaths,New.recovered)%>%
  filter(Country %in% "China")

ggplot(a,aes(x=Date,y=New.cases))+geom_point(size=0.8)+ggtitle("New cases in China")

f= xts(x=a$New.cases, order.by = a$Date)
China = dygraph(f,main = "China new cases")%>%
  dyOptions(labelsUTC = T,fillGraph = T,colors = "darkblue",fillAlpha = 0.8)%>%
  dyRangeSelector()%>%
  dyRoller(rollPeriod = 1)
China

b = full_grouped %>%
  select(Country,Date,New.cases,New.deaths,New.recovered)%>%
  filter(Country %in% "India")

ggplot(b,aes(x=Date,y=New.cases))+geom_point(size=0.8)+ggtitle("New cases in India")


f= xts(x=b$New.cases, order.by = b$Date)
India = dygraph(f,main = "India new cases")%>%
  dyOptions(labelsUTC = T,fillGraph = T,colors = "red",fillAlpha = 0.8)%>%
  dyRangeSelector()%>%
  dyRoller(rollPeriod = 1)
India

c = full_grouped %>%
  select(Country,Date,New.cases,New.deaths,New.recovered)%>%
  filter(Country %in% "US")

ggplot(c,aes(x=Date,y=New.cases))+geom_point(size=0.8)+ggtitle("New cases in USA")

f= xts(x=b$New.cases, order.by = b$Date)
USA = dygraph(f,main = "USA new cases")%>%
  dyOptions(labelsUTC = T,fillGraph = T,colors = "red",fillAlpha = 0.8)%>%
  dyRangeSelector()%>%
  dyRoller(rollPeriod = 1)
USA 

############################### END OF SCRIPT ##############################################
