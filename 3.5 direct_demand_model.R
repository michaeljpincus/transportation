rm(list=ls())

####Load data
load("C:/Users/erickg/Box/2. Courses/CPLN 550 Transportation Planning/1. Lectures/Part 3 Transportation modeling/Direct Demand/stationridership_phoenix.RData")
#Feel free to double click the data file or load with code.

dat<-data
rm(data)

#We are trying to predict ridership in Phoenix

####### DATA INSPECTION #######

# Look at the data
# Dummies
str(dat)

#Dictionary
labels <- c("Average weekday transit ridership (average of boardings and alightings)",
            "Trains in and out at the AM peak", "Number of bus connections within a quarter mile",
            "Park and ride dummy", "Commuter rail dummy", "terminal station", "airport station",
            "BRT station", "light rail station", "heavy rail station", "multimodal transfer center", 
            "average distance to next station stops", "network distance to central business district",
            "jobs accessible within 30m drive", "Population residing within a half mile", "jobs within a half mile",
            "Unique CBSA id", "CBSA name")

labels <- as.data.frame(cbind(names(dat),labels))
labels

head(dat)
summary(dat)
table(dat$parking_d)
hist(dat$rider)
# What are CBSAs?
# https://en.wikipedia.org/wiki/Core-based_statistical_area

dat$jobs_30m_drive[dat$jobs_30m_drive < 0] <- NA

# This isn't a very helpful visualization because of outlier(s).
plot(density(dat$rider))
# What should normal look like?
normaldist <- rnorm(1000,mean=6300,sd=12775.94)

par(mfrow=c(1,2))
plot(density(dat$rider))
plot(density(normaldist))

# Let's take a closer look
summary(dat$rider)
# How to find the row(s) with max. ridership?
dat[which.max(dat$rider),]

# Seems like most observations have ridership below 10,000.
hist(dat$rider[dat$rider < 10000], breaks = 20)

hist(dat$rider[dat$lrt_d == 1])
hist(dat$rider[dat$hrt_d == 1])

hist(dat$rider[dat$lrt_d == 1 & dat $ rider < 10000])
hist(dat$rider[dat$hrt_d == 1 & dat $ rider < 10000])

# Plot two variables
# What's the relationship?
par(mfrow=c(1,1))

plot(dat$frequency, dat$rider)
abline(lm(dat$rider ~ dat$frequency))

#Boxplot is a helpful way to look at distribution
boxplot(rider ~ hrt_d, dat)

# Outlier issue
boxplot(rider ~ hrt_d, dat, outline = F)
boxplot(rider~hrt_d, subset(dat, dat$rider <10000))
median(dat$rider[dat$hrt_d==0])
median(dat$rider[dat$hrt_d==1])

# Is New York causing problems again?
mean(dat$rider[dat$hrt_d==1])
mean(dat$rider[dat$hrt_d==1 & dat$CBSA_nam!="New York-Northern New Jersey-Long Island NY-NJ-PA"])


###### MODEL BUILDING ######

#Predict Average daily ridership as a function of population, jobs, and service frequency
# Model building

x <- lm(rider ~ pop_halfmile + jobs_halfmile + frequency + sta_dist + terminal_d, dat)
summary(x)

## How to interpret the results? Causality?
# Coefficients, Stand Error, Pr, p-value, R-squared or Adjusted R-squared?

2.276e-01+2.317e-02*1.96
2.276e-01-2.317e-02*1.96


##Let's add bus connections
x2 <- lm(rider ~ pop_halfmile + jobs_halfmile + frequency  + sta_dist + terminal_d + bus_connections, dat)
summary(x2)

# Did this improve our model
# What does the Adjusted R-squared tell you (be careful)?
stargazer(x,x2, type = "text")

# 1. Informal: statistically significant variable? 
# 2. Theoretically logical? 
# 3. Formal Full Model Reduced model ANOVA test
anova(x,x2)

######Let's see what our models say
dat$p1 <- predict(x2)

hist(dat$p1)
# Notice something weird here? 

summary(dat$p1)

## Important to look at residuals.
# Is the model good enough?


# Get the residuals
dat$r1<- resid(x2)
plot(dat$p1,dat$r1)

abline(0,0)


# Very clearly heteroscedastic (as opposed to homoscedastic).
# Indicates non-linear relationship, missing variables, etc.

##Let's try a non-linear function through transformation
hist(log(dat$rider))


# Why plus 1 here?

x3 <- lm(log(rider+1) ~ log(pop_halfmile+1) + log(jobs_halfmile+1) 
         + log(frequency) + log(sta_dist) + terminal_d + log (bus_connections+1)  , dat)
summary(x3)

# Log transformation will get rid of negative values in prediction.
# How do these plots look? Better?
plot(x3)


# Let's predict using our new model
dat$p3 <- predict(x3)

# p3 are log transformed values.
dat$p3exp <- exp(dat$p3)-1

plot(density(dat$p3exp))
lines(density(dat$rider, na.rm=TRUE), col='red')
# Fitness v. Predicting power
# Try more on your own

####### PREDICTION #######

## Now let's see how well models predict phoenix

phoenix$p3 <- predict(x3, newdata=phoenix)
phoenix$p3exp <- exp(phoenix$p3)-1

plot(phoenix$p3exp, phoenix$rider)

# Estimate an R-squared
mod <- lm(log(rider+1) ~ p3, phoenix)
summary(mod)


##How about actual ridership
correlation <- cor(phoenix$rider, phoenix$p3exp)
correlation^2 #Even better

##########How might we improve?##########

#Let's improve the sample

x4 <- lm(log(rider+1) ~ log(pop_halfmile+1) + log(jobs_halfmile+1) 
         + log(frequency) + log(sta_dist) + terminal_d +
           log (bus_connections+1) , subset(dat, lrt_d== 1 & hrt_d != 1))

summary(x4)

phoenix$p4 <- predict(x4, newdata=phoenix)
phoenix$p4exp <- exp(phoenix$p4)-1


# Estimate an R-squared
mod <- lm(rider~ p4exp, phoenix)
summary(mod)

##Next steps for improving prediction?
#ideas

##Clean regression output with stargazer

library(stargazer)

stargazer(x,x2, type="text")

stargazer(x, x2, type="html", out="models.htm"
          , dep.var.labels=c("Average light rail station boardings and alightings")
          , covariate.labels=c("People within  a half mile", "Jobs within a half mile", "AM peak service frequency",
                               "Average distance to nearest stations", "Terminal Station",
                             "Bus connections nearby"))

##A few last thoughts

# Good fit does not always mean good predicting power. At a certain point, often in opposition.
# Prediction can only be as good as our data allow it to be
# "All models are wrong but some models are useful..." -George Box:

