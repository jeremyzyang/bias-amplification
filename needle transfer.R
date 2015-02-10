                                         # needle transfer #

# Y: frequency of using used needle in last 30 days
# Z: frequency of drug injection in shooting gallery in last 30 days
# G: 23 different sites (cities)
# X: age,gender,ethnicity,education,frequency of drug injection in last 30 days, HIV diagnosis 
# W: HIV seroprevalence, X.groupmean


# read data
dir <- "C:/Users/Jeremy/Desktop/Workspace/R/multilevel/data.tsv" # change to local path
needle.data <- read.delim(dir)
nrow(needle.data);ncol(needle.data)
head(needle.data[,1:10],10)

IDU <- needle.data[which(needle.data$PTARPOP==1),] # restrict sample to injection drug user (IDU)

# selecting dependent variable (Y) and individual level predictor (X,Z)
variable <- c('CTUSED','XYXPINT','ASEX','ARACGP','ASCHGR','CTIJ30','BHT3','CTSUPLB','SITECODE')
names <- c('freq.used','age','gender','ethnicity','education','freq.injection','HIV','gallery','site') 

data <- IDU[variable]
colnames(data) <- names
nrow(data);ncol(data)
head(data,10)

data$site <- as.factor(data$site) # use site as factor

# dropping observations with missing dependent variable (Y), changing others to NA
newdata <- data[which(data$freq.used >= 0),]

newdata$education[newdata$education < 0] <- NA
newdata$freq.injection[newdata$freq.injection < 0] <- NA
newdata$gallery[newdata$gallery < 0] <- NA

# selecting group level predictor (W)
freq.used.mean <- tapply(newdata$freq.used,newdata$site,mean,na.rm=TRUE)
freq.injection.mean <- tapply(newdata$freq.injection,newdata$site,mean,na.rm=TRUE)
HIV.site <- tapply(newdata$HIV,newdata$site,sum,na.rm=TRUE)

age.mean <- tapply(newdata$age,newdata$site,mean,na.rm=TRUE)
gender.mean <- tapply(newdata$gender,newdata$site,mean,na.rm=TRUE)
ethnicity.mean <- tapply(newdata$ethnicity,newdata$site,mean,na.rm=TRUE)
education.mean <- tapply(newdata$education,newdata$site,mean,na.rm=TRUE)
HIV.mean <- tapply(newdata$HIV,newdata$site,mean,na.rm=TRUE)
gallery.mean <- tapply(newdata$galler,newdata$site,mean,na.rm=TRUE)

site <- as.factor(101:123)
group <- data.frame(site,freq.used.mean,freq.injection.mean,HIV.site)
newgroup <- data.frame(site,age.mean,gender.mean,ethnicity.mean,education.mean,HIV.mean,gallery.mean)

# merge individual data and group data (including group mean of all predictors) into one dataframe 
fulldata <- merge(newdata,group,'site')
head(fulldata,10)
fulldata <- merge(fulldata,newgroup,'site')
fulldata <- fulldata[,c('site','freq.used','freq.injection','age','gender','ethnicity','education','HIV','gallery','freq.used.mean','freq.injection.mean','age.mean','gender.mean','ethnicity.mean','education.mean','HIV.mean','gallery.mean')]
head(fulldata)

# de-group-mean all variables
y <- fulldata$gallery
i <- 2:9
for (i in 2:9)
{fulldata[i] <- fulldata[i]-fulldata[i+8]}
fulldata <- rename(fulldata,c(gallery='gallery.gmc'))
fulldata$gallery <- y
colnames(fulldata)
head(fulldata,10)

class(fulldata$gallery.gmc)
class(fulldata$gallery)

fulldata <- as.data.frame(fulldata)

# run lmer and lm models
library(lme4)

## Z ~ (1|site)
m1 <- lmer(gallery ~ (1|site),data=fulldata); summary(m1) 
m1.between <- 57.16
m1.within <- 839.86

## Z ~ X + W + (1|group)
m2 <- lmer(gallery ~ freq.injection + age + gender + ethnicity + education + HIV + 
             freq.injection.mean + age.mean + gender.mean + ethnicity.mean + education.mean + HIV.mean +
             (1|site),data=fulldata);summary(m2)
m2.between <- 7.319
m2.within <- 812.767

## Y ~ Z.gmc + Z.mn + (1|group)
m3 <- lmer(freq.used ~ gallery.gmc + gallery.mean + (1|site),data=fulldata)
summary(m3)
m3.tau <- 1.02833

## Y ~ Z.gmc + Z.mn + X + W + (1|group)
m4 <- lmer(freq.used ~ gallery.gmc + gallery.mean +
             freq.injection + age + gender + ethnicity + education + HIV +
             freq.injection.mean + age.mean + gender.mean + ethnicity.mean + education.mean + HIV.mean +
             (1|site),data=fulldata)
summary(m4)
m4.tau <- 0.879831

## Y ~ Z
m5 <- lm(freq.used ~ gallery.gmc,data=fulldata)
summary(m5)
m5.tau <- 1.02833

## Y ~ Z + X + W
m6 <- lm(freq.used ~ gallery.gmc +
           freq.injection + age + gender + ethnicity + education + HIV +
           freq.injection.mean + age.mean + gender.mean + ethnicity.mean + education.mean + HIV.mean,
         data=fulldata);summary(m6)
m6.tau <- 0.879831

