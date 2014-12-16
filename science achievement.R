                               # family backgroud on science score #

# Y: science test raw score
# G: different schools
# Z: family background (word knowledge,father's education,number of books in home)
# X: sex, years of science study, hours of science instruction per week
# W: Z.groupmean,X.groupmean

library(foreign)
library(plyr)
library(reshape)
mydata <- read.spss("C:/Users/Jeremy/Desktop/Workspace/R/multilevel/science achievement/Sweden.sav", to.data.frame=TRUE)
data.sweden.all <- mydata
dim(data.sweden.all)
data.sweden.all[1:10,]

# notes: the original study use a subsample of about 1800 observations, they use father's occupation but that column
# appears as intelligible symbols: you can check data.sweden[1:20,112], so I use father's education instead. They
# also include an EXPLORE scale (Kelly 1978) which is a summary of some variables in the data,but they didn't 
# mention how it was caculated exactly,so I didn't include that
set.seed(123)
data.sweden <- data.sweden.all[sample(1:nrow(data.sweden.all),1800,replace=FALSE),]

names <- c('school','score','sex','word_knowl','father_edu','num_books','yr_sci_study','hr_sci_instruc')
data.sweden.all <- data.sweden.all[,c(3,512,115,202,116,133,433,445)]
colnames(data.sweden.all) <- names


# recoding
data.sweden.all$sex <- revalue(data.sweden.all$sex,c("GIRL"=0,"BOY"=1))
data.sweden.all$father_edu <- revalue(data.sweden.all$father_edu, c("0 YEARS"=0, "GT 0 LE 5"=1,"GT 5 LE 10"=2, "GT 10 LE 15"=3,"GT 15 YRS"=4))
data.sweden.all$num_books <- revalue(data.sweden.all$num_books, c("NONE"=0, "1 - 10"=1,"11 - 25"=2, "26 - 50"=3,"51 OR MORE"=4))
data.sweden.all$yr_sci_study <- revalue(data.sweden.all$yr_sci_study, c("NEVER   STUDIED"=0, "LE 1 YR"=1,"GT 1 LE 3"=2, "GT 3 LE 5"=3,"GT 5 YRS"=4))
data.sweden.all$hr_sci_instruc <- revalue(data.sweden.all$hr_sci_instruc, c("DO NOT  TAKE"=0, "LE 2 HRS"=1,"GT 2 LE 4"=2, "GT 4 LE 6"=3,"GT 6 HRS"=4))

data.sweden.all$sex <- as.numeric(as.character(data.sweden.all$sex))
data.sweden.all$father_edu <- as.numeric(as.character(data.sweden.all$father_edu))
data.sweden.all$num_books <- as.numeric(as.character(data.sweden.all$num_books))
data.sweden.all$yr_sci_study <- as.numeric(as.character(data.sweden.all$yr_sci_study))
data.sweden.all$hr_sci_instruc <- as.numeric(as.character(data.sweden.all$hr_sci_instruc))

# computing group level predictors and merge data
sex_mean <- tapply(data.sweden.all$sex,data.sweden.all$school,mean,na.rm=TRUE)
word_knowl_mean <- tapply(data.sweden.all$word_knowl,data.sweden.all$school,mean,na.rm=TRUE)
father_edu_mean <- tapply(data.sweden.all$father_edu,data.sweden.all$school,mean,na.rm=TRUE)
num_books_mean <- tapply(data.sweden.all$num_books,data.sweden.all$school,mean,na.rm=TRUE)
yr_sci_study_mean <- tapply(data.sweden.all$yr_sci_study,data.sweden.all$school,mean,na.rm=TRUE)
hr_sci_instruc_mean <- tapply(data.sweden.all$hr_sci_instruc,data.sweden.all$school,mean,na.rm=TRUE)

school <- unique(data.sweden.all$school)
grouplevel <- data.frame(school,sex_mean,word_knowl_mean,father_edu_mean,num_books_mean,yr_sci_study_mean,hr_sci_instruc_mean)
x <- merge(data.sweden.all,grouplevel,'school')
data.sweden.all <- x
dim(data.sweden.all)

s <- data.sweden.all

# de-group-mean all variables

y <- data.sweden.all$num_books
for (i in 3:8)
{data.sweden.all[i] <- data.sweden.all[i]-data.sweden.all[i+6]}
head(data.sweden.all,20)
data.sweden.all <- rename(data.sweden.all,c(num_books='num_books_gmc'))
data.sweden.all$num_books <- y

head(data.sweden.all)

# run lmer and lm models: I take number of books in home as Z here, it can be easily extended to other two
library(lme4)

## Z ~ (1|site)
m1 <- lmer(num_books ~ (1|school),data.sweden.all); summary(m1) 
m1.us.between <- 0.08676
m1.us.within <- 0.66215

m1.uk.between <- 0.1242
m1.uk.within <- 0.9397

m1.sweden.between <- 0.02018
m1.sweden.within <- 1.23222

## Z ~ X + W + (1|group)

m2 <- lmer(num_books ~ sex + word_knowl + father_edu + yr_sci_study + hr_sci_instruc + 
             sex_mean + word_knowl_mean + father_edu_mean + num_books_mean + yr_sci_study_mean + 
             hr_sci_instruc_mean + (1|school),data.sweden.all); summary(m2)
m2.us.between <- 0.02718
m2.us.within <- 0.61541

m2.uk.between <- 0.04234
m2.uk.within <- 0.87750

m2.sweden.between <- 0.6162
m2.sweden.within <- 1.0041

## Y ~ Z.gmc + Z.mn + (1|group)
m3 <- lmer(score ~ num_books_gmc + num_books_mean + (1|school),data.sweden.all); summary(m3)
m3.us.tau <- 1.9897

m3.uk.tau <- 2.2491

m3.sweden.tau <- 2.682

## Y ~ Z.gmc + Z.mn + X + W + (1|group)
m4 <- lmer(score ~ num_books_gmc + sex + word_knowl + father_edu + yr_sci_study + hr_sci_instruc + 
             sex_mean + word_knowl_mean + father_edu_mean + num_books_mean + yr_sci_study_mean + 
             hr_sci_instruc_mean + (1|school),data.sweden.all); summary(m4)
m4.us.tau <- 1.08205

m4.uk.tau <- 1.34745

m4.sweden.tau <-1.0356

## Y ~ Z
m5 <- lm(score ~ num_books_gmc,data.sweden.all);summary(m5)
m5.us.tau <- 1.8050

m5.uk.tau <- 2.3071

m5.sweden.tau <- 2.646

## Y ~ Z + X + W
m6 <- lm(score ~ num_books_gmc + sex + word_knowl + father_edu + yr_sci_study + hr_sci_instruc + 
             sex_mean + word_knowl_mean + father_edu_mean + num_books_mean + yr_sci_study_mean + 
             hr_sci_instruc_mean,data.sweden.all);summary(m6)
m6.us.tau <- 1.26782

m6.uk.tau <- 1.39709

m6.sweden.tau <- 1.0356

