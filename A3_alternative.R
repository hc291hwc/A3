#------------------
library('tidyverse')
library('dplyr')
library('stringr')
library('tidyr')
library("readr")
library('ggplot2')
library('lubridate')
library('AER')
library('moments')
library('fastDummies')

#------------------
# Reading data 
setwd('C:/Users/ddd/Downloads/Econ613_A3')


crimelong <- read.csv('crime_long.csv')
officers <- read.csv('officers.csv')
population <- read.csv('population.csv')

#=========================
#Exercise 1 & 2
#=========================

#-----
# q1 
#-----
# change the time index to lubridate object, so it will be easier to manipulate and plot the time series
crimelong$crime_month <- ymd(crimelong$crime_month)
population$month <- ymd(population$month)
officers$month <- ymd(officers$month)

# group all the month together by the group_by function and create a new month variable, then
# use summarize function to sum all the crimes in a given month 
dat_totalcrime <- crimelong %>%
  group_by(month=ceiling_date(crime_month, "month")) %>%
  summarize(tot_amount=sum(crimes))

# graph the total crime in a month by ggplot, adding the x-labels
dat_totalcrime %>% ggplot(aes(x=month, y=tot_amount ))  + geom_line()  +  scale_x_date(date_labels = "%Y %b %d")

#-----
#q2
#-----
# merge the two data sets by two keys (crime_month, district) because we need units in the following exercise
dat_merge <- left_join(crimelong, population, by = c("crime_month" = "month", "district" = "district"))


#-----
#q3
#-----

# change the categorical variable to 'factor' type 
dat_merge$crime_type = as.factor(dat_merge$crime_type)
dat_merge$district = as.factor(dat_merge$district)

# find the total crimes per resident 
# also, I find the median income, share of black, hispanic, and white residents here because I only have 
#   to group by the month, district 
panel_total <- dat_merge %>%
  group_by(month=ceiling_date(crime_month, "month"), district) %>%
  summarize(crime_total_per_resident = sum(crimes)/ sum(tot_pop),
            median_income = median(p50_inc),
            share_black = sum(tot_black) / sum(tot_pop),
            share_hispanic = sum(tot_hisp) / sum(tot_pop),
            share_white = sum(tot_white) / sum(tot_pop)
  )


# find the violent crimes and property crimes per resident 
# I group by both (month, district, crime_type) to find the crime rate for each crime category 
#   during each month 
g_dat_merge_crime_type <- dat_merge %>%
  group_by(month=ceiling_date(crime_month, "month"), district, crime_type) %>%
  summarize(crime_type_per_resident = sum(crimes)/ sum(tot_pop)  )

# after grouping out the data, select the desired crime type, violent and property 
panel_violent <- filter(g_dat_merge_crime_type, crime_type == 'violent')[c(1, 2, 4)]
panel_property <- filter(g_dat_merge_crime_type, crime_type == 'property')[c(1,2, 4)]

colnames(panel_violent) = c('month', 'district', 'crime_vio_per_resident')
colnames(panel_property) = c('month', 'district','crime_pro_per_resident')


# merge all the data (panel_total, panel_violent, panel_property) all together to get my final 
#   panel data (each of the three has 5128 obs.)
panel_data <- left_join(panel_total, panel_violent, by = c('month' = 'month', 'district'= 'district')) 
panel_data <- left_join(panel_data, panel_property,  by = c('month' = 'month', 'district'= 'district')) 

# re-order the column as the sequence of the assignment questions
panel_data <- panel_data[c("month", 'district',"crime_total_per_resident", "crime_vio_per_resident",
                           'crime_pro_per_resident', 'median_income', 'share_black',
                           'share_hispanic', 'share_white')]

# sort the panel_data by district (unit)
panel_data = arrange(panel_data, district)

# In the officers.csv data set, district name was called unit, so I changed the colname of 'district' to 'unit' 
#     for consistency 

# I did not drop the na values 
colnames(panel_data)[2] = c('unit')




#=========================
#Exercise 3
#=========================

# plot the arrest number over time with 1 particular officer, just to briefly understand the data 
p <- ggplot(filter(officers, NUID == 1), aes(x=month, y= arrest)) + geom_line() +  scale_x_date(date_labels = "%Y %b %d")
print(p)

# merge the officers data and the panel_data from exercise 2 
officers$unit = as.factor(officers$unit)
mer_ex3 <- left_join(officers, panel_data, by = c('month' = 'month', 'unit' = 'unit'))


# estimate the ols model 
eg3_1 <- lm(arrest ~ tenure +  crime_total_per_resident +  log(median_income) +  share_black + 
                     share_hispanic +   share_white , data = mer_ex3)
summary(eg3_1)

# printCoefmat(coeftest(eg3_1, vcov = sandwich))

#=========================
#Exercise 4
#=========================

# estimate the ols model with unit and time fixed effect 
eg4 <- lm(arrest ~ tenure +  crime_total_per_resident +  log(median_income) +  share_black + 
              share_hispanic +  share_white + as.factor(unit) + as.factor(month) , data = mer_ex3)

# don't want to report the coefficients of fixed effect 
printCoefmat(coeftest(eg4, vcov = sandwich)[1:7,])


#=========================
#Exercise 5
#=========================


#-----
#q1
#-----

# using the data from exercise 3, I calculate the mean for every column for the within and between estimator
avg_overtime <-  mer_ex3 %>%
  group_by(NUID, unit) %>%
  summarize(num_period = n(),
            avg_arrest = mean(arrest),
            avg_tenure = mean(tenure),
            avg_totalcrime = mean(crime_total_per_resident),
            avg_median_inc = mean(median_income),
            avg_black = mean(share_black),
            avg_hispanic = mean(share_hispanic),
            avg_white = mean(share_white)
            )

# after creating the mean, I left-merge this avg dataframe to the original dataframe, so I can calculate the 
#   demean estimator
# I later figure it out that I have a much smarter way to do so. I can just calculat the demean estimator in the last step all at once 
mer_ex5 <- left_join(mer_ex3, avg_overtime, by = c('NUID' = "NUID", 'unit' = 'unit'))

# calculat demean columns 
dat_within_all <- mer_ex5 %>% mutate(wi_arrest = arrest - avg_arrest,
                                 wi_tenure = tenure - avg_tenure,
                                 wi_totalcri = crime_total_per_resident - avg_totalcrime,
                                 wi_median_inc = median_income - avg_median_inc,
                                 wi_black = share_black - avg_black,
                                 wi_hispanic = share_hispanic - avg_hispanic,
                                 wi_white = share_white - avg_white
                                 )     


dat_within <- dat_within_all[-4:-20]
dat_between <- dat_within_all[-4:-13]



# first difference

# to find the first difference column, I first sort the time index in  descending order 
dat_firstdiff <- mer_ex3 %>% arrange(NUID, unit, desc(month))

# calculate Yt- Tt-1 in the individual, unit level 
dat_firstdiff <- dat_firstdiff %>%
  group_by(NUID, unit) %>%
  mutate(fd_tenure =  tenure - lag(tenure),
         fd_arrest = arrest - lag(arrest), 
         fd_crimetot = crime_total_per_resident - lag(crime_total_per_resident),
         fd_medinc = median_income - lag(median_income),
         fd_black = share_black - lag(share_black),
         fd_hispanic = share_hispanic - lag(share_hispanic),
         fd_white = share_white - lag(share_white)
         )
dat_firstdiff <- dat_firstdiff[-4:-12]

# first difference will generate NA values(the first one in the window), so I dropped the na 
dat_firstdiff_noNA <- na.omit(dat_firstdiff)

# implement within, between, firstdiff estimators 
eg5_within <- lm(wi_arrest ~ wi_tenure +  wi_totalcri +  log(wi_median_inc) +  wi_black + 
                   wi_hispanic +  wi_white + as.factor(unit) + as.factor(month) , data = dat_within)

eg5_between <- lm(avg_arrest ~ avg_tenure +  avg_totalcrime +  log(avg_median_inc) +  avg_black + 
                    avg_hispanic +  avg_white + as.factor(unit) + as.factor(month) , data = dat_between)

eg5_firstdiff <- lm(fd_arrest ~ fd_tenure +  fd_crimetot +  log(fd_medinc) +  fd_black + 
                      fd_hispanic +  fd_white + as.factor(unit) + as.factor(month) , data = dat_firstdiff_noNA)



# model summary, don't want to print out fixed effect 
printCoefmat(coeftest(eg5_within, vcov = sandwich)[1:7,])
printCoefmat(coeftest(eg5_between, vcov = sandwich)[1:7,])
printCoefmat(coeftest(eg5_firstdiff, vcov = sandwich)[1:7,])



#-----
#q2
#-----

#GMM approach 
# after trying really hard, I did not solve this question successfully, but please see my code and thinking process, thank you!

# create a data frame w/o na value 
mer_ex5_no_na <- mer_ex3 %>% drop_na()
mer_ex5_no_na <- mer_ex5_no_na[c("NUID", "arrest", "unit", "month", "tenure", "crime_total_per_resident",
                                 "median_income", "share_black" ,  "share_hispanic", "share_white" )]
# create factor variables 
mer_ex5_no_na <- subset(mer_ex5_no_na, select = -c(month) )
                        
# mer_ex5_no_na$month <-  as.factor(mer_ex5_no_na$month)
mer_ex5_no_na$unit <- as.factor(mer_ex5_no_na$unit)

mer_ex5_final <- fastDummies::dummy_cols(mer_ex5_no_na)
mer_ex5_final$median_income = log(mer_ex5_final$median_income)

# create independent variables matrix and dependent variable vector
ex5_ind <- as.matrix(mer_ex5_final[, 4:34])
ex5_dep <- as.matrix(mer_ex5_final[, 2])

# calculate the numbers of parameters
n_individual = nrow(ex5_ind)
n_estimators = ncol(ex5_ind)
n_par =  n_estimators 

# method of moments with simulations here
# calculate the variance of the moments by boot strap.
nboot   = 9
mom_mat = mat.or.vec(n_par,nboot)
for (iN in 1:nboot)
{
  xs           = sample(ex5_dep, n_individual,replace=T)
  mom          <- all.moments(xs, order.max= n_par )
  mom_mat[,iN] = mom[-1]
}

#--------------
param = runif(n_estimators, -1, 1)
xs           = (sample(as.data.frame(ex5_ind), n_individual,replace=T))
fitted_val   = xs %*% param 
#--------------

vs = apply(mom_mat,1,var)

mm_data = function(param,vs, ex5_dep, ex5_ind)
{
  data_mom = mat.or.vec(n_par,1)
    # form the mixture
  error_term  =   ex5_dep - ex5_ind %*% param
    # calculate the moments
  data_mom = all.moments(error_term, order.max = n_par )[-1]
  
  
  crit    = (t(t(data_mom)))*(1/vs)*(data_mom )
  return(sum(crit));
}




#start = c(-0.2029413,0.6773643,2.4927606,-1.4250393,-5.7520823)

start = runif(n_par, -10, 10)

res  = optim(start,fn= mm_data,method="BFGS",control=list(trace=6,REPORT=1,maxit=1000),vs=vs,ex5_dep = ex5_dep, ex5_ind = ex5_ind)

param    = res$par







