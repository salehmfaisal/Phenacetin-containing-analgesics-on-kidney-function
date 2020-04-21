data<- read.csv("parc.txt") 
data
head(data)
####checking for missing data

data$creat_68[data$creat_68==9.99] <- NA
data$creat_69[data$creat_69==9.99] <- NA
data$creat_70[data$creat_70==9.99] <- NA
data$creat_71[data$creat_71==9.99] <- NA
data$creat_72[data$creat_72==9.99] <- NA
data$creat_75[data$creat_75==9.99] <- NA
data$creat_78[data$creat_78==9.99] <- NA
is.na(data)
### Creating new data set
years=c(0,0,0,68,69,70,71,72,75,78)
nrow=nrow(data)
newdata=NULL
for (i in (1:nrow)){
for(j in (4:10)){
if(!is.na(data[i,j])){
Y=data[i,j]
Id=data[i,1]
age=data[i,2]
group=data[i,3]
year=years[j]
newdata=rbind(newdata,c(Id,Y,year,age,group))}}}
newdata
head(newdata)

swiss <- data.frame(newdata)
names(swiss) <- c("Id", "Y", "Year", "age", "group") 
head(swiss)
dat1 = data.frame(Y = swiss[,2], Id =swiss[,1], Year = swiss[,3], age=swiss[,4],group=factor(swiss[,5]))
dat1
dat1$Id = factor(dat1$Id)
dat1$yr =factor (dat1$Year-68)
dat1$age1 = dat1$age-mean(dat1$age)
hist(dat1$Y)
hist(log(dat1$Y))
require(lme4)
 ####Model comparison
  lmr1 <- lmer(log(Y) ~ age+ yr +group+ (1 | Id), data = dat1,REML=F)
lmr2 <- lmer(log(Y) ~ age+group+ (1 | yr)+(1|Id), data = dat1,REML=F)
lmr3<-lmer(log(Y) ~ age+ yr +group+ group*yr+(1 | Id), data = dat1,REML=F)
anova(lmr1,lmr2,lmr3)
lmr1b<-lmer(log(Y) ~  age+yr +group+(1 | Id), data = dat1,REML=T)##Kenward Roger approx only work for REML criteriaREML
#####Kenward-Roger appox
library(lmerTest)
anova(lmr1b, ddf="Kenward-Roger", refit=FALSE)
summary(lmr1b)
###confidence Interval
  fm1W <- confint(lmr1, method="Wald") 
fm1W  
fm1P <- confint(lmr1, method="profile")
fm1P
###To get P-value for parameters of the model
require(lmerTest)
summary(lmr1b)
##To estimate profile
pr1 <- profile (lmr1)
require("lattice")
xyplot ( pr1 , aspect =1.3)
splom ( pr1 ) ####Profile pairs plot
densityplot(pr1)
##reduced model for Profile pairs plot
lmr1r=lmer(log(Y) ~  yr +(1 | Id), data = dat1,REML=F)
pr2 <- profile (lmr1r)
splom ( pr2 )

###Residual plot QQplot
plot(lmr1, type=c("p","smooth"))
qqmath(lmr1, id=0.05)
##to see nonlinear trend
plot(dat1$Year,dat1$Y)

