                                          # fertility #
# Y: # children ever born (1974-1976)
# G: different countries
# Z: education
# X: age, childhood residence 
# W: x.gmn, GNP per capita (1974), family planning program index (1972)


# install read.isi package
install.packages('C:/Users/vsingh/Desktop/bias-amp/Read.isi_0.5.1.tar.gz', repos = NULL, type="source")
library(Read.isi)

# read in data from 35 countries, it's gonna take A WHILE to run the read.isi function
dir <- 'C:/Users/vsingh/Desktop/bias-amp/fertility/' # the folder that contains the data sets

# 12 countries in the original analysis
colombia <- read.isi(paste(dir,'Colombia.dct',sep=''),paste(dir,'Colombia.dat',sep='')) 
costarica <- read.isi(paste(dir,'CostaRica.dct',sep=''),paste(dir,'CostaRica.dat',sep=''))
fiji <- read.isi(paste(dir,'Fiji.dct',sep=''),paste(dir,'Fiji.dat',sep=''))
guyana <- read.isi(paste(dir,'Guyana.dct',sep=''),paste(dir,'Guyana.dat',sep=''))
indonesia <- read.isi(paste(dir,'Indonesia.dct',sep=''),paste(dir,'Indonesia.dat',sep=''))
ivorycoast <- read.isi(paste(dir,'IvoryCoast.dct',sep=''),paste(dir,'IvoryCoast.dat',sep='')) 
jamaica <- read.isi(paste(dir,'Jamaica.dct',sep=''),paste(dir,'Jamaica.dat',sep=''))
kenya <- read.isi(paste(dir,'Kenya.dct',sep=''),paste(dir,'Kenya.dat',sep=''))
korea <- read.isi(paste(dir,'Korea.dct',sep=''),paste(dir,'Korea.dat',sep=''))
lesotho <- read.isi(paste(dir,'Lesotho.dct',sep=''),paste(dir,'Lesotho.dat',sep=''))
panama <- read.isi(paste(dir,'Panama.dct',sep=''),paste(dir,'Panama.dat',sep=''))
srilanka <- read.isi(paste(dir,'Srilanka.dct',sep=''),paste(dir,'Srilanka.dat',sep=''))

# newly added 23 countries
bangladesh <- read.isi(paste(dir,'Bangladesh.dct',sep=''),paste(dir,'Bangladesh.dat',sep=''))
benin <- read.isi(paste(dir,'Benin.dct',sep=''),paste(dir,'Benin.dat',sep=''))
cameroon <- read.isi(paste(dir,'Cameroon.dct',sep=''),paste(dir,'Cameroon.dat',sep=''))
dominican <- read.isi(paste(dir,'DominicanRepublic.dct',sep=''),paste(dir,'DominicanRepublic.dat',sep=''))
ecuador <- read.isi(paste(dir,'Ecuador.dct',sep=''),paste(dir,'Ecuador.dat',sep=''))
egypt <- read.isi(paste(dir,'Egypt.dct',sep=''),paste(dir,'Egypt.dat',sep=''))
ghana <- read.isi(paste(dir,'Ghana.dct',sep=''),paste(dir,'Ghana.dat',sep=''))
haiti <- read.isi(paste(dir,'Haiti.dct',sep=''),paste(dir,'Haiti.dat',sep=''))
mauritania <- read.isi(paste(dir,'Mauritania.dct',sep=''),paste(dir,'Mauritania.dat',sep=''))
mexico <- read.isi(paste(dir,'Mexico.dct',sep=''),paste(dir,'Mexico.dat',sep=''))
morocco <- read.isi(paste(dir,'Morocco.dct',sep=''),paste(dir,'Morocco.dat',sep=''))
nepal <- read.isi(paste(dir,'Nepal.dct',sep=''),paste(dir,'Nepal.dat',sep=''))
sudan <- read.isi(paste(dir,'NorthSudan.dct',sep=''),paste(dir,'NorthSudan.dat',sep=''))
pakistan <- read.isi(paste(dir,'Pakistan.dct',sep=''),paste(dir,'Pakistan.dat',sep=''))
paraguay <- read.isi(paste(dir,'Paraguay.dct',sep=''),paste(dir,'Paraguay.dat',sep=''))
peru <- read.isi(paste(dir,'Peru.dct',sep=''),paste(dir,'Peru.dat',sep=''))
philippines <- read.isi(paste(dir,'Philippines.dct',sep=''),paste(dir,'Philippines.dat',sep=''))
senegal <- read.isi(paste(dir,'Senegal.dct',sep=''),paste(dir,'Senegal.dat',sep=''))
syria <- read.isi(paste(dir,'Syria.dct',sep=''),paste(dir,'Syria.dat',sep=''))
tunisia <- read.isi(paste(dir,'Tunisia.dct',sep=''),paste(dir,'Tunisia.dat',sep=''))
turkey <- read.isi(paste(dir,'Turkey.dct',sep=''),paste(dir,'Turkey.dat',sep=''))
venezuela <- read.isi(paste(dir,'Venezuela.dct',sep=''),paste(dir,'Venezuela.dat',sep=''))
yemen <- read.isi(paste(dir,'Yemen.dct',sep=''),paste(dir,'Yemen.dat',sep=''))

df <- list(bangladesh,benin,cameroon,colombia,costarica,dominican,ecuador,egypt,fiji,ghana,guyana,haiti,indonesia,
           ivorycoast,jamaica,kenya,korea,lesotho,mauritania,mexico,morocco,nepal,sudan,pakistan,panama,paraguay,
           peru,philippines,senegal,srilanka,syria,tunisia,turkey,venezuela,yemen)

# subsetting data
df.12 <- list(colombia,costarica,fiji,guyana,indonesia,jamaica,kenya,korea,lesotho,panama,peru,srilanka)
df.12 <- lapply(df.12,function (df) {df <- df[,c('V208','V703','V704','V802','V010','V107')] 
                            colnames(df) <- c('children_ever_born','childhood_residence','education','partner_education','age','marital_status')
                            return(df)})
colombia <- df.12[[1]]
costarica <- df.12[[2]]
fiji <- df.12[[3]]
guyana <- df.12[[4]]
indonesia <- df.12[[5]]
jamaica <- df.12[[6]]
kenya <- df.12[[7]]
korea <- df.12[[8]]
lesotho <- df.12[[9]]
panama <- df.12[[10]]
peru <- df.12[[11]]
srilanka <- df.12[[12]]

# add in group level predictor 
df.12 <- list(colombia,costarica,fiji,guyana,indonesia,jamaica,kenya,korea,lesotho,panama,peru,srilanka)
df.12 <- lapply(df.12,)

country <- 1:12 
gnp <- c(500,740,840,440,150,1050,200,480,140,1000,740,130) # gross national product 1972-1974 
planning <- c(85.3,39.8,55.4,29.9,87.1,64.1,33.7,96.9,15.5,59.2,26.4,81.6) # family-planning program index (bigger means stronger intervention)

for (i in 1:12){
  df.12[[i]]$country <- i
  df.12[[i]]$gnp <- gnp[i]
  df.12[[i]]$family_planning <- planning[i]
}

colombia <- df.12[[1]]
costarica <- df.12[[2]]
fiji <- df.12[[3]]
guyana <- df.12[[4]]
indonesia <- df.12[[5]]
jamaica <- df.12[[6]]
kenya <- df.12[[7]]
korea <- df.12[[8]]
lesotho <- df.12[[9]]
panama <- df.12[[10]]
peru <- df.12[[11]]
srilanka <- df.12[[12]]

# merge dataset 
data <- Reduce(function(x, y) merge(x, y, all=TRUE), 
                   list(colombia,costarica,fiji,guyana,indonesia,jamaica,kenya,korea,lesotho,panama,peru,srilanka))
write.csv(data,'C:/Users/Jeremy/Desktop/fertility_merged_data.csv')

# read the data directly so you don't have to reprocess the raw data
dir <- "C:/Users/Jeremy/Desktop/Workspace/R/multilevel/fertility/fertility_merged_data.csv"
data <- read.csv(dir)
data <- data[,-1]
head(data)
toy <- data

# recode
toy$children_ever_born[which(toy$children_ever_born>=22)] <- NA
table(toy[,1])

toy$childhood_residence[which(toy$childhood_residence=='VILLAGE'|toy$childhood_residence=='Conutryside'|toy$childhood_residence=='Rural'|toy$childhood_residence=='RURAL')] <- 1 # village
toy$childhood_residence[which(toy$childhood_residence=='Town'|toy$childhood_residence=='TOWN')] <- 2 # town
toy$childhood_residence[which(toy$childhood_residence=='City'|toy$childhood_residence=='CITY'|toy$childhood_residence=='URBAN')] <- 3 # city
toy$childhood_residence[which(toy$childhood_residence==0|toy$childhood_residence=='NOT USED')] <- NA
toy$childhood_residence <- droplevels(toy$childhood_residence)
table(toy[,2])

toy$education[which(toy$education=='No schooling'|toy$education=='NO SCHOOLING'|toy$education=='None'|toy$education=='NONE'|toy$education=='COMPL PRIMARY'|toy$education=='Pre-primary only'|toy$education=='Primary'|toy$education=='PRIMARY < 4 YEARS'|toy$education=='Primary <4 years'|toy$education=='Primary 1'|toy$education=='Primary 2'|toy$education=='Primary 3'|toy$education=='Primary 4'|toy$education=='Primary 4+ years'|toy$education=='PRIMARY 4+ YEARS'|toy$education=='Primary 5'|toy$education=='Primary 6'|toy$education=='0'|toy$education=='1'|toy$education=='2'|toy$education=='3'|toy$education=='4'|toy$education=='5'|toy$education=='6')] <- 1 # <=6
toy$education[which(toy$education=='7'|toy$education=='8'|toy$education=='9'|toy$education=='COMPL JUNIOR'|toy$education=='Primary 7'|toy$education=='Primary 8'|toy$education=='Secondary'|toy$education=='Secondary 1'|toy$education=='Secondary 2'|toy$education=='Secondary 3')] <- 2 # <=9
toy$education[which(toy$education=='10'|toy$education=='11'|toy$education=='12'|toy$education=='COMPL SENIOR'|toy$education=='Secondary 4'|toy$education=='Secondary 5'|toy$education=='Secondary 6')] <- 3 # <= 12
toy$education[which(toy$education=='13'|toy$education=='14'|toy$education=='15'|toy$education=='Secondary or higher'|toy$education=='COMPL ACADEMY'|toy$education=='SECONDARY-HIGHER'|toy$education=='Vocational')] <- 4 # <= 15
toy$education[which(toy$education=='16'|toy$education=='17'|toy$education=='18'|toy$education=='19'|toy$education=='20'|toy$education=='14+ years'|toy$education=='COMPL UNIV'|toy$education=='Post University 2'|toy$education=='Some educ,lev.ns'|toy$education=='University'|toy$education=='University 1'|toy$education=='University 2'|toy$education=='University 3'|toy$education=='University 4')] <- 5 # > 15
toy$education[which(toy$education=='Adult literacy')] <- NA
toy$education <- droplevels(toy$education)
table(toy[,3])
toy$education <- as.numeric(toy$education)

# use the variables used by original study
colnames(toy)
toy <- toy[,c(1,2,3,5,7,8,9)]
head(toy)

# run models, take education as Z
library(lme4)

## Z ~ (1|site)
m1 <- lmer(education ~ (1|country),toy); summary(m1) 
m1.between = 0.1801
m1.within = 0.6548

## Z ~ X + W + (1|group)
m2 <- lmer(education ~  + childhood_residence + age + gnp + family_planning + (1|country),toy); summary(m2)
m2.between = 0.04996
m2.within = 0.41743

## Y ~ Z + (1|group) 
m3 <- lmer(children_ever_born ~ education + (1|country),toy); summary(m3)
m3.tau = -1.05743

## Y ~ Z + X + W + (1|group)
m4 <- lmer(children_ever_born ~ education + childhood_residence + age + gnp + family_planning + (1|country),toy); summary(m4)
m4.tau = -0.6309649
  
## Y ~ Z
m5 <- lm(children_ever_born ~ education,toy);summary(m5)
m5.tau = -0.93903

## Y ~ Z + X + W
m6 <- lm(children_ever_born ~ education + childhood_residence + age + gnp + family_planning,toy);summary(m6)
m6.tau = -0.6086841
