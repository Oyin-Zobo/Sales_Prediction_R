
library(MASS)
library(broom)
library(caret)
library(fastDummies)
library(tidyverse)
library(dplyr)
library(funModeling) 
library(tidyverse) 
library(Hmisc)
library(e1071)
library(pls)
library(glmnet)
library(devtools)
library(ggbiplot)
library(mlbench)
library(tidyverse)
library(caret)
library(MASS)
library(stats)
library(dplyr)
library(ggplot2)
library(factoextra)
library(Rtsne)
library(umap)
library(FactoMineR)
library(Amelia)
library(mice)
library(VIM)
library(plyr)
library(magrittr)
library(naniar)
library(lubridate)
library(ggcorrplot)

# train_metric = read.csv(file = 'train.csv',
#                         header = TRUE, sep = ",",
#                         quote = "\"" , dec = ".",
#                         fill =TRUE, comment.char = "", na.strings = c(""," ", "NA"))


train_metric = read.csv(file = 'Train.csv')
missmap(data, legend = TRUE)  #missingness map
gg_miss_var(data, show_pct = TRUE)  #looking at the % of missingness for each variable

dfStat = df_status(train_metric)
write.csv(dfStat, "dfStat.csv", row.names=FALSE)

#this shows the number of 0s and 1s that the data has and also the number and 
#percentage of the null values in the data set.

missmap(train_metric, legend = TRUE) #missingness map
gg_miss_var(train_metric, show_pct = TRUE) #looking at the % of missingness for each variable
gg_miss_var(train_metric) # looking at the n of missingness for each variable
region_NA <- aggregate((train_metric),by=list(train_metric$region),function(x) sum(is.na(x)))
summary(train_metric) # to see the summary of statistic of each variable
# from the missingness map, it is seen that the variables
# with high missingness include: bounce, adcontents, adwords & newVisits




basic_eda <- function(data)
{
  glimpse(data)
  df_status(data)
  freq(data) 
  profiling_num(data)
  plot_num(data)
  describe(data)
}

basic_eda(train_metric)

freq(train_metric)
#shows the frequency of for each of the variables and barcharts are also plotted
#to show the counts of each variable. 

source = count(train_metric,'source') # count the number of occurrences 
source = data.frame(source) # converting to a data frame
source = arrange(source, desc(freq)) # arrange in descending order
source = head (source)
names(source)[1] <- "Traffic_Source"
names(source)[2] <- "freq"
ggplot(source)+
  geom_bar(mapping = aes(x = Traffic_Source, y = freq, fill = Traffic_Source), stat = "identity")


plot_num(train_metric)
#plots the counts of the numeric variable 

describe(data)

profiling_num(train_metric)
#stastitic distribution of the data and the quantile ranges of the data.

ggplot(train_metric)+
  geom_bar(mapping = aes(x = channelGrouping, fill = as.factor(isMobile)))   
#shows the distibution of the channel groupping and the mobile variable. 
#across all the channel groupings, majority of the user dont use mobile.

ggplot(train_metric)+
  geom_histogram(mapping = aes(x = pageviews))

ggplot(train_metric)+
  geom_histogram(mapping = aes(x = revenue))

#the revenue distribution is skewed with a high value. This goes to show that the majority of purchases from the site are from a few individuals 

ggplot(train_metric)+
  geom_bar(mapping = aes(x = continent, fill = continent))
#africa has the lowest distribtuion 

ggplot(train_metric)+
  geom_bar(mapping = aes(x = continent, y = revenue))
# this line of code above is not running

amer = train_metric %>%filter(continent == "Americas")
amer$pageviews
train_metric$continent
train_metric %>% group_by(continent) %>%
  dplyr::summarise(sum_pageview = sum(pageviews, na.rm = TRUE)) 



arrange(desc(sum_pageview))


data_pageview


ggplot(data = data_pageview, aes(x= continent, y=sum_pageview, 
                                 fill = continent)) + geom_bar(stat="identity")
#America has the higest view and Africa has the lowest views. 

data_rev  = train_metric %>% group_by(continent) %>%
  summarise(sum_revenue = sum(revenue)) %>%
  arrange(desc(sum_revenue))
data_rev

ggplot(data = data_rev, aes(x= continent, y=sum_revenue, fill = continent)) +
  geom_bar(stat="identity")
#the highest number of revenue is from america and the next is from asia 
#and then africa. 


ggplot(data = train_metric)+geom_bar(mapping = aes(channelGrouping))
ggplot(data = train_metric)+geom_bar(mapping = aes(isMobile))
ggplot(data = train_metric, mapping = aes(x = visitNumber))+geom_bar(mapping = aes(color = channelGrouping))

ggplot(train_metric , aes(x=visitNumber, y= timeSinceLastVisit, color=medium))+
  geom_point(size=3) +
  facet_wrap(~medium , dir="v") +
  theme(legend.position="none")
# there doesn't seem to be a good trend in comparing visit number with timeSinceLastVisit

ggplot(train_metric , aes(x=visitNumber, y= revenue, color=medium))+
  geom_point(size=3) +
  facet_wrap(~medium , dir="v") +
  theme(legend.position="none")

channelGrouping = train_metric %>% 
  group_by(channelGrouping) %>% dplyr::summarize(freq = n())

channelGrouping_rev = aggregate(data$revenue, by = list(data$channelGrouping), FUN = sum)
channelGrouping_rev$x = round(channelGrouping_rev$x)



channelGrouping = data.frame(channelGrouping_rev, channelGrouping)
#colnames(channelGrouping)
#renaming the columns
names(channelGrouping)[1]= "CG"
names(channelGrouping)[2]= "Rev"
names(channelGrouping)[4]= "freq"
channelGrouping = channelGrouping[,c(1,2,4)]
channelGrouping %>% arrange(desc(`Rev`))

channelGrouping$f_fraction = channelGrouping$freq / sum(channelGrouping$freq) #computing %
channelGrouping$f_ymax = cumsum(channelGrouping$f_fraction) # computing the cum %
channelGrouping$f_ymin = c(0, head(channelGrouping$f_ymax, n = -1))
channelGrouping$f_labelPosition = (channelGrouping$f_ymax + channelGrouping$f_ymin )/2

# Compute a good label
channelGrouping$f_label <- paste0(channelGrouping$channelGrouping, "\n value: ", channelGrouping$freq)



c_grp = train_metric %>% 
  filter(channelGrouping != "NA") %>% 
  group_by(channelGrouping) %>% 
  count() %>% 
  ungroup()%>% 
  arrange(desc(channelGrouping)) %>%
  mutate(percentage = round(freq/sum(freq),4)*100,
         lab.pos = cumsum(percentage)-.5*percentage)

c_grp = train_metric %>% 
  filter(browser != "NA") %>% 
  group_by(browser) %>% 
  count() %>% 
  ungroup()%>% 
  arrange(desc(browser)) %>%
  mutate(percentage = round(freq/sum(freq),4)*100,
         lab.pos = cumsum(percentage)-.5*percentage)



# a donut chart plot to see the data with the channel with highest traffic to the site
# ggplot(data = c_grp, 
#        aes(x = 2, y = percentage, fill = channelGrouping))+
#   geom_bar(stat = "identity")+
#   coord_polar("y", start = 200) +
#   geom_text(aes(y = lab.pos, label = paste(percentage,"%", sep = "")), col = "white") +
#   theme_void() +
#   scale_fill_brewer(palette = "Dark2")+
#   xlim(.2,2.5)

# This shows that the highest proportion of channel grouping come from organic search 

source = count(train_metric,'source') # count the number of occurrences 
source = data.frame(source) # converting to a data frame
source = arrange(source, desc(freq)) # arrange in descending order
# the source with the highest ranking frequency is google 

brows_rev = aggregate(train_metric$revenue, by = list(train_metric$browser), FUN = sum)
browser_NA <- aggregate((train_metric),by=list(train_metric$browser),function(x) sum(is.na(x)))
# trying to check if browser has empty values


browser = train_metric %>% 
  group_by(browser) %>% dplyr::summarize(n = n())

browser = data.frame(brows_rev, browser)
colnames(browser)
#renaming the columns
names(browser)[1]= "browser"
names(browser)[2]= "Tot. Rev"
names(browser)[4]= "freq"
browser = browser[,c(1,2,4)]
browser = browser %>% arrange(desc(`Tot. Rev`))
# from the analysis browser is an important variable, as is seen
# chrome has the highest frequency of occurence and shows total revenue

#Analysis for operating system
os_rev = aggregate(train_metric$revenue, by = list(train_metric$operatingSystem), FUN = sum)
#browser_NA <- aggregate((data),by=list(data$browser),function(x) sum(is.na(x)))
# trying to check if browser has empty values


os = train_metric %>% 
  group_by(operatingSystem) %>% dplyr::summarize(n = n())

os = data.frame(os_rev, os)
colnames(os)
#renaming the columns
names(os)[1]= "operatingSystem"
names(os)[2]= "Tot. Rev"
names(os)[4]= "freq"
os = os[,c(1,2,4)]
os = os %>% arrange(desc(`Tot. Rev`))
# from the analysis it is seen that no revenue comes from individuals who use
# devices other than computers

# I believe we can delete 2 of either isMobile, deviceCategory or operatingSystem

medium = train_metric %>% 
  group_by(medium) %>%dplyr:: summarize(n = n())

medium_rev = aggregate(train_metric$revenue, by = list(train_metric$medium), FUN = sum)
# medium has a lot of missing values, so it could  be removed
medium = data.frame(medium_rev, medium)
colnames(browser)
#renaming the columns
names(medium)[1]= "medium"
names(medium)[2]= "Tot. Rev"
names(medium)[4]= "freq"
medium = medium[,c(1,2,4)]
medium = medium %>% arrange(desc(`Tot. Rev`))

# I'm thinking of going with country, region and city 
# and removing metro

continent = train_metric %>% 
  group_by(continent) %>% dplyr::summarize(n = n())
continent_rev = aggregate(train_metric$revenue, by = list(train_metric$continent), FUN = sum)
continent = data.frame(continent_rev, continent)
colnames(continent)
#renaming the columns
names(continent)[1]= "continent"
names(continent)[2]= "Tot. Rev"
names(continent)[4]= "freq"
continent = continent[,c(1,2,4)]
continent = continent %>% arrange(desc(`Tot. Rev`))

country = train_metric %>% 
  group_by(country) %>% dplyr::summarize(n = n())
country_rev = aggregate(train_metric$revenue, by = list(train_metric$country), FUN = sum)
country = data.frame(country_rev, country)
colnames(country)
#renaming the columns
names(country)[1]= "country"
names(country)[2]= "Tot. Rev"
names(country)[4]= "freq"
country = country[,c(1,2,4)]
country = country %>% arrange(desc(`Tot. Rev`))

View(country)
channelGrouping
ggplot(data = channelGrouping, mapping = aes(x= CG, y = Rev, fill = CG))
+ geom_bar(stat="identity")

#data wrangling 
#creating new variables 
train_metric = train_metric %>% 
  separate (sessionId, into= c("Customer", "number of visit"), sep = 5)%>%
  separate (date, into = c("year", "month", "day"))

train_metric_new = train_metric[, c(1,2,3,4,5,6,7,10,11,
                                    12,13,15,16,17,24,25,27,35,36,37,38)]
#missing value impuatation. The NA here were chanaged to 0 as this means they 
#are bounced or they use mobile phones or not. 
train_metric_new$bounces[is.na(train_metric_new$bounces)]=0
#make the NA zero because NA here means
train_metric_new$newVisits[is.na(train_metric_new$newVisits)]=0
#make the NA zero because NA here means


#transformation 
hist (train_metric$revenue) # looking at the distribution of revenue before transformation
logRevenue = log(train_metric$revenue)
hist (logRevenue) # log transformation of revenue variable

hist (train_metric$pageviews) # looking at the distribution of pageViews before transformation
logpageViews = log(train_metric$pageviews)
hist (logpageViews) # log transformation of pageViews variable

# Using the boxcox function for transformation of customer page views

Box = boxcox(train_metric$pageviews~1, lambda = seq(-3,3,0.1)) 

cox  = data.frame(Box$x, Box$y)
cox2 = cox[with(cox, order(-cox$Box.y)),]
cox2[1,]

lambda = cox2[1,"Box.x"]
lambda
T_box = (train_metric$pageviews^ (lambda) - 1)/(lambda)
hist(T_box) # distribution after transformation of custpageviews variable


#outlier 

grubbs.test(train_metric$revenue)  #univariate test for 'revenue' outliers
outlier(train_metric$revenue)   #univariate test for 'reveune' outliers


#na.strings = c(""," ", "NA"

test_metric = read.csv(file = 'test.csv',
                       header = TRUE, sep = ",",
                       quote = "\"" , dec = ".",
                       fill =TRUE, comment.char = "")

train_metric = train_metric %>% 
  separate (sessionId, into= c("Customer", "number of visit"), sep = 5)%>%
  separate (date, into = c("year", "month", "day"))

test_metric = test_metric %>% 
  separate (sessionId, into= c("Customer", "number of visit"), sep = 5)%>%
  separate (date, into = c("year", "month", "day"))

train_metric_new = train_metric[, c(1,2,3,4,5,6,7,10,11,
                                    12,13,15,16,17,24,25,27,35,36,37,38)]

test_metric_new = test_metric[, c(1,2,3,4,5,6,7,10,11,
                                  12,13,15,16,17,24,25,27,35,36,37)]

train_metric_new$pageviews[is.na(train_metric_new$pageviews)]=0
train_metric_new$bounces[is.na(train_metric_new$bounces)]=0
test_metric_new$bounces[is.na(test_metric_new$bounces)]=0

#make the NA zero because NA here means
test_metric_new$pageviews[is.na(test_metric_new$pageviews)]=0
train_metric_new$newVisits[is.na(train_metric_new$newVisits)]=0
test_metric_new$newVisits[is.na(test_metric_new$newVisits)]=0
#make the NA zero because NA here means
is.na(test_metric_new$bounces)

train_data = train_metric_new %>% group_by(custId) %>%
  summarise(custRevenue = sum(revenue), custpageviews = sum(pageviews))
test_data = test_metric_new %>% group_by(custId) %>%
  summarise(custpageviews = sum(pageviews))

train_metric_coun = train_metric_new %>% group_by(custId, country) %>% 
  summarise(c = n()) %>% 
  filter(row_number(desc(c))==1) %>% 
  rename (mostfreqCountry = country)
train_metric_coun = train_metric_coun[,-3]

test_metric_coun = test_metric_new %>% group_by(custId, country) %>% 
  summarise(c = n()) %>% 
  filter(row_number(desc(c))==1) %>% 
  rename (mostfreqCountry = country)
test_metric_coun = test_metric_coun[,-3]

train_metric_group = train_metric_new %>% group_by(custId, channelGrouping) %>% 
  summarise(c = n()) %>% 
  filter(row_number(desc(c))==1) 

train_metric_group = train_metric_group[,-3]

train_metric_group = rename (train_metric_group, mostfreqchangroup = channelGrouping) 


test_metric_group = test_metric_new %>% group_by(custId, channelGrouping) %>% 
  summarise(c = n()) %>% 
  filter(row_number(desc(c))==1)

test_metric_group = test_metric_group[,-3]

test_metric_group = rename (test_metric_group, mostfreqchangroup = channelGrouping) 


train_metric_month = train_metric_new %>% group_by(custId, month) %>% 
  summarise(c = n()) %>% 
  filter(row_number(desc(c))==1) %>% 
  rename (mostfreqMonth = month)
train_metric_month = train_metric_month[,-3]

test_metric_month = test_metric_new %>% group_by(custId, month) %>% 
  summarise(c = n()) %>% 
  filter(row_number(desc(c))==1) %>% 
  rename (mostfreqMonth = month)
test_metric_month = test_metric_month[,-3]  

train_metric_mobile = train_metric_new %>% group_by(custId, isMobile) %>% 
  summarise(c = n()) %>% 
  filter(row_number(desc(c))==1) %>% 
  rename (mostfreqMobile = isMobile)
train_metric_mobile = train_metric_mobile[,-3]

test_metric_mobile = test_metric_new %>% group_by(custId, isMobile) %>% 
  summarise(c = n()) %>% 
  filter(row_number(desc(c))==1) %>% 
  rename (mostfreqMobile = isMobile)
test_metric_mobile = test_metric_mobile[,-3]

train_metric_browser = train_metric_new %>% group_by(custId, browser) %>% 
  summarise(c = n()) %>% 
  filter(row_number(desc(c))==1) %>% 
  rename (mostfreqBroswer = browser)
train_metric_browser = train_metric_browser[,-3]

test_metric_browser = test_metric_new %>% group_by(custId, browser) %>% 
  summarise(c = n()) %>% 
  filter(row_number(desc(c))==1) %>% 
  rename (mostfreqBroswer = browser)
test_metric_browser = test_metric_browser[,-3]

train_metric_mobile = train_metric_new %>% group_by(custId, isMobile) %>% 
  summarise(c = n()) %>% 
  filter(row_number(desc(c))==1) %>% 
  rename (mostfreqMobile = isMobile)
train_metric_mobile = train_metric_mobile[,-3]


test_metric_mobile = test_metric_new %>% group_by(custId, isMobile) %>% 
  summarise(c = n()) %>% 
  filter(row_number(desc(c))==1) %>% 
  rename (mostfreqMobile = isMobile)
test_metric_mobile = test_metric_mobile[,-3]

train_metric_Opsys = train_metric_new %>% group_by(custId, operatingSystem) %>% 
  summarise(c = n()) %>% 
  filter(row_number(desc(c))==1) %>% 
  rename (mostfreqOpeSys = operatingSystem)
train_metric_Opsys = train_metric_Opsys[,-3]

test_metric_Opsys = test_metric_new %>% group_by(custId, operatingSystem) %>% 
  summarise(c = n()) %>% 
  filter(row_number(desc(c))==1) %>% 
  rename (mostfreqOpeSys = operatingSystem)
test_metric_Opsys = test_metric_Opsys[,-3]

train_metric_cont = train_metric_new %>% group_by(custId, continent) %>% 
  summarise(c = n()) %>% 
  filter(row_number(desc(c))==1) %>% 
  rename (mostfreqCont = continent)
train_metric_cont = train_metric_cont[,-3]


test_metric_cont = test_metric_new %>% group_by(custId, continent) %>% 
  summarise(c = n()) %>% 
  filter(row_number(desc(c))==1) %>% 
  rename (mostfreqCont = continent)
test_metric_cont = test_metric_cont[,-3]
test_metric_cont

train_metric_newvisit = train_metric_new %>% group_by(custId, newVisits) %>% 
  summarise(c = n()) %>% 
  filter(row_number(desc(c))==1) %>% 
  rename (mostfreqnewvisit = newVisits)
train_metric_newvisit = train_metric_newvisit[,-3]


test_metric_newvisit = test_metric_new %>% group_by(custId, newVisits) %>% 
  summarise(c = n()) %>% 
  filter(row_number(desc(c))==1) %>% 
  rename (mostfreqnewvisit = newVisits)
test_metric_newvisit = test_metric_newvisit[,-3]
test_metric_newvisit

train_metric_bounces = train_metric_new %>% group_by(custId, bounces) %>% 
  summarise(c = n()) %>% 
  filter(row_number(desc(c))==1) %>% 
  rename (mostfreqnewbounces = bounces)
train_metric_bounces = train_metric_bounces[,-3]

test_metric_bounces = test_metric_new %>% group_by(custId, bounces) %>% 
  summarise(c = n()) %>% 
  filter(row_number(desc(c))==1) %>% 
  rename (mostfreqnewbounces = bounces)
test_metric_bounces = test_metric_bounces[,-3]


total = left_join(train_data, train_metric_coun,by = "custId")
total = left_join(total, train_metric_group,by = "custId")
total = left_join(total, train_metric_month,by = "custId")
total = left_join(total, train_metric_mobile,by = "custId")
total = left_join(total, train_metric_browser,by = "custId")
total = left_join(total, train_metric_Opsys,by = "custId")
total = left_join(total, train_metric_cont,by = "custId")
total = left_join(total, train_metric_bounces,by = "custId")
total = left_join(total, train_metric_newvisit,by = "custId")

total_test = left_join(test_data, test_metric_coun,by = "custId")
total_test = left_join(total_test, test_metric_group,by = "custId")
total_test = left_join(total_test, test_metric_month,by = "custId")
total_test = left_join(total_test, test_metric_mobile,by = "custId")
total_test = left_join(total_test, test_metric_browser,by = "custId")
total_test = left_join(total_test, test_metric_Opsys,by = "custId")
total_test = left_join(total_test, test_metric_newvisit,by = "custId")
total_test= left_join(total_test, test_metric_cont,by = "custId")
total_test = left_join(total_test, test_metric_bounces,by = "custId")
data_test = total_test

train_data = total %>% 
  mutate(total, targetRevenue = log((custRevenue+1)))

train_data$mostfreqCountry = fct_lump_n(train_data$mostfreqCountry, 10 ,
                                        other_level = "Other")
train_data$mostfreqBroswer = fct_lump_n(train_data$mostfreqBroswer, 9 ,
                                        other_level = "Other")

total_test$mostfreqCountry = fct_lump_n(total_test$mostfreqCountry, 10 ,
                                        other_level = "Other")
total_test$mostfreqBroswer = fct_lump_n(total_test$mostfreqBroswer, 9 ,
                                        other_level = "Other")

total_test$custpageviews = log((total_test$custpageviews)+1)
train_data$custpageviews = log((train_data$custpageviews)+1)

#OLS 

model1 = lm(data = train_data, targetRevenue ~ custpageviews
            + mostfreqBroswer + mostfreqMonth + mostfreqCountry +mostfreqCont
            + mostfreqchangroup + mostfreqMobile +
              mostfreqnewbounces + mostfreqnewvisit)

#total_test$custpageviews = log((total_test$custpageviews)+1)
plot(model1)


#try to see the data collienarity 



predict_rev = predict(model1, total_test)

submissionDf = data.frame(custId=total_test$custId, predRevenue = predict_rev)

summary(model1)

car::vif(model1)

write.csv(submissionDf, "submission1.csv", row.names=FALSE)

#ridge regression 
train.ridge <- lm.ridge(targetRevenue ~ custpageviews + mostfreqCountry + 
                          mostfreqchangroup + mostfreqMonth + mostfreqMobile +
                          mostfreqBroswer + 
                          mostfreqCont + mostfreqOpeSys + mostfreqnewbounces 
                        + mostfreqnewvisit, 
                        data=train_data, 
                        lambda = seq(0,30,0.1))


td = tidy(train.ridge)
head(td)
g = glance(train.ridge)
g
methods(class = 'ridgelm')
select(train.ridge)
const = as.numeric(names(which.min(train.ridge$GCV)))
total_test_new = total_test[,-c(1,8)]


dataf <- dummy_cols(total_test_new, select_columns = c('mostfreqCountry', 
                                                   'mostfreqchangroup', 
                                                   'mostfreqMonth', 
                                                    'mostfreqBroswer', 
                                                   'mostfreqCont'))
dataf = dataf[, -c(2,3,4,6,8)]

new  = dataf[,-c(46,5,16,24,36)]


predicted  = train.ridge$ym  + scale(new, center = train.ridge$xm, scale =
                                       train.ridge$scales) %*% 
  train.ridge$coef[,which.min(train.ridge$GCV)]

#as.numeric(predicted)

submissionDf2 = data.frame(custId=data_test$custId, predRevenue = predicted)
write.csv(submissionDf2, "submission2.csv", row.names=FALSE)

#ridge, lasso, elastic net 
total_test1 <- dummy_cols(total_test, select_columns = c('mostfreqCountry', 
                                                         'mostfreqchangroup', 
                                                         'mostfreqMonth', 
                                                         'mostfreqBroswer', 
                                                         'mostfreqCont'))

y = total_test$custId
dataf <- dummy_cols(train_data, select_columns = c('mostfreqCountry', 
                                                   'mostfreqchangroup', 
                                                   'mostfreqMonth', 
                                                   'mostfreqBroswer', 
                                                   'mostfreqCont'))

dataf1 = dataf[,-c(1,2,4,5,6,7,8,9,10,13)]
total_test1  =total_test1[,-c(1,3,4,5,6,7,8,10,13)]

train_cont <- trainControl(method = "repeatedcv",
                           number = 10,
                           repeats = 5,
                           search = "random",
                           verboseIter = TRUE)

# Train the model
elastic_reg <- train(targetRevenue ~  custpageviews + 
                       mostfreqCountry + 
                       mostfreqchangroup + mostfreqMonth + mostfreqMobile +
                       mostfreqBroswer + 
                       mostfreqCont + mostfreqnewbounces + mostfreqnewvisit,
                     data = train_data,
                     method = "glmnet",
                     preProcess = c("center", "scale"),
                     tuneLength = 10,
                     trControl = train_cont)

# Best tuning parameter
elastic_reg$bestTune

elas_cv <- cv.glmnet(x, y_train, alpha = 0.58, 
                     lambda = lambdas, 
                     standardize = TRUE, nfolds = 5)

# Make predictions on training set
predictions_train <- predict(elastic_reg, train_data)

eval_results = function(true, predicted, df) {
  SSE <- sum((predicted - true)^2)
  SST <- sum((true - mean(true))^2)
  R_square <- 1 - SSE / SST
  RMSE = sqrt(SSE/nrow(df))
  
  # Model performance metrics
  data.frame(
    RMSE = RMSE,
    Rsquare = R_square)
}

eval_results(train_data$targetRevenue, predictions_train, train_data) 

predictions_test <- predict(elastic_reg, total_test)

submissionDf3 = data.frame(custId=y, predRevenue = predictions_test)
write.csv(submissionDf3, "submission3.csv", row.names=FALSE)


#pls

model <- train(targetRevenue ~ custpageviews + 
                 mostfreqCountry + 
                 mostfreqchangroup + mostfreqMonth + mostfreqMobile +
                 mostfreqBroswer + 
                 mostfreqCont + mostfreqnewbounces + mostfreqnewvisit, 
               data = train_data, method = "pls",
               scale = TRUE,
               trControl = trainControl("cv", number = 10),
               tuneLength = 10)

summary(model)
predictions_test <- predict(model, total_test)
predictions_train <- predict(model, train_data)

eval_results(train_data$targetRevenue, predictions_train, train_data) 

submissionDf5 = data.frame(custId=total_test$custId, predRevenue = predictions_test)
write.csv(submissionDf5, "submission5.csv", row.names=FALSE)

#lasso
dataf1 = dataf1[,-5]

x = as.matrix(dataf1)
x_test = as.matrix(total_test1)


y_train = train_data$targetRevenue

lambdas <- 10^seq(2, -3, by = -.1)
lasso_reg <- cv.glmnet(x, y_train, alpha = 1, lambda = lambdas, 
                       standardize = TRUE, nfolds = 5)
optimal_lambda <- lasso_reg$lambda.min
optimal_lambda

view(dataf1)
# Prediction and evaluation on train data
predictions_train <- predict(lasso_reg, s = optimal_lambda, newx = x)
eval_results(y_train, predictions_train, train_data)

# Prediction and evaluation on test data
predictions_test <- predict(lasso_reg, s = optimal_lambda, newx = x_test)
submissionDf3 = data.frame(custId=total_test$custId, predRevenue = predictions_test)

write.csv(submissionDf3, "submission3.csv", row.names=FALSE)

#ridge 
ridge_cv <- cv.glmnet(x, y_train, alpha = 0, lambda = lambdas, standardize = TRUE, nfolds = 5)

optimal_lambda <- ridge_cv$lambda.min
optimal_lambda

# Prediction and evaluation on train data
predictions_train <- predict(ridge_cv, s = optimal_lambda, newx = x)
eval_results(y_train, predictions_train, train_data)

# Prediction and evaluation on test data
predictions_test <- predict(ridge_cv, s = optimal_lambda, newx = x_test)
submissionDf4 = data.frame(custId=total_test$custId, predRevenue = predictions_test)

write.csv(submissionDf4, "submission4.csv", row.names=FALSE)


#rlm
rlm_model = rlm(targetRevenue ~ custpageviews + 
                  mostfreqCountry + 
                  mostfreqchangroup + mostfreqMonth + mostfreqMobile +
                  mostfreqBroswer + 
                  mostfreqCont + mostfreqnewbounces + mostfreqnewvisit, train_data, 
                psi = psi.bisquare)

summary(rlm_model)
predictions_train <- predict(rlm_model, train_data)
predictions_train
#regr.eval(train_data$targetRevenue, predictions_train)
predictions_test <- predict(rlm_model, total_test)

submissionDf6 = data.frame(custId=total_test$custId, predRevenue = predictions_test)

write.csv(submissionDf6, "submission6.csv", row.names=FALSE)


#svm
model_svm = svm(targetRevenue~ custpageviews + mostfreqMonth+ mostfreqCountry, 
                data = train_data)
svm_test = predict(model_svm, total_test)
submissionDf7 = data.frame(custId=total_test$custId, predRevenue = svm_test)
write.csv(submissionDf6, "submission6.csv", row.names=FALSE)
