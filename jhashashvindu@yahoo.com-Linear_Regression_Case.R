 # Clear environment
rm(list = ls()) 
setwd("D:\\download\\r study\\Regression Case Studies - Linear & Logistic(1)\\Linear Regression Case")
getwd()
#install.packages("readxl")
library("readxl")
li <- read_excel("Linear Regression Case.xlsx")
#Understand the data
#Understand the data
str(li)
names(li)
View(li)
head(li)
tail(li)
dim(li)
nrow(li)
ncol(li)
summary(li)
#Understand distribution of the data


summary1 <- summary(li)
summary2 <- str(li)
#install.packages("psych")
library(psych)
summary3 <- describe(li)

write.csv(summary1, file = "summary1.csv")
write.csv(summary2, file = "summary2.csv")
write.csv(summary3, file = "summary3.csv")

require(Hmisc)
require(corrplot)
require(car)

#Step 1 - EDA
#=========================================================
# user written function for creating descriptive statistics
#----------------------------------------------------------

# descriptive statistics

mystats=function(x){
  if(class(x)=="numeric"){
    Var_Type=class(x)
    n<-length(x)
    nmiss<-sum(is.na(x))
    mean<-mean(x,na.rm=T)
    std<-sd(x,na.rm=T)
    var<-var(x,na.rm=T)
    min<-min(x,na.rm=T)
    p1<-quantile(x,0.01,na.rm=T)
    p5<-quantile(x,0.05,na.rm=T)
    p10<-quantile(x,0.1,na.rm=T)
    q1<-quantile(x,0.25,na.rm=T)
    q2<-quantile(x,0.5,na.rm=T)
    q3<-quantile(x,0.75,na.rm=T)
    p90<-quantile(x,0.9,na.rm=T)
    p95<-quantile(x,0.95,na.rm=T)
    p99<-quantile(x,0.99,na.rm=T)
    max<-max(x,na.rm=T)
    UC1=mean(x,na.rm=T)+3*sd(x,na.rm=T)
    LC1=mean(x,na.rm=T)-3*sd(x,na.rm=T)
    UC2=quantile(x,0.99,na.rm=T)
    LC2=quantile(x,0.01,na.rm=T)
    iqr=IQR(x,na.rm=T)
    UC3=q3+1.5*iqr
    LC3=q1-1.5*iqr
    ot1<-max>UC1 | min<LC1 
    ot2<-max>UC2 | min<LC2 
    ot3<-max>UC3 | min<LC3
    return(c(Var_Type=Var_Type, n=n,nmiss=nmiss,mean=mean,std=std,var=var,min=min,p1=p1,p5=p5,p10=p10,q1=q1,q2=q2,q3=q3,p90=p90,p95=p95,p99=p99,max=max,ot_m1=ot1,ot_m2=ot2,ot_m2=ot3))
  }
  else{
    Var_Type=class(x)
    n<-length(x)
    nmiss<-sum(is.na(x))
    fre<-table(x)
    prop<-prop.table(table(x))
    x[is.na(x)]<-x[which.max(prop.table(table(x)))]
    
    return(c(Var_Type=Var_Type, n=n,nmiss=nmiss,freq=fre,proportion=prop))
  }
}

# Creating user defined variables.

li$total_spend <- li$cardspent + li$card2spent
li$total_items <- li$carditems + li$card2items

#Vector of numaerical variables
num_var= sapply(li,is.numeric)
Other_var= !sapply(li,is.numeric)


#Applying above defined function on numerical variables
my_num_data<-t(data.frame(apply(li[num_var], 2, mystats)))
my_cat_data<-data.frame(t(apply(li[Other_var], 2, mystats)))
View(my_num_data)
View(my_cat_data)

write.csv(my_num_data, file = "my_num_data.csv")

#Missing Value Treatment
li[,num_var] <- apply(data.frame(li[,num_var]), 2, function(x){x <- replace(x, is.na(x), mean(x, na.rm=TRUE))})
li[,Other_var] <- apply(data.frame(li[,Other_var]), 2, function(x){x <- replace(x, is.na(x), which.max(prop.table(table(x))))})


# Outlier capping with p95 and p5.
numeric_vars = names(li)[sapply(li, FUN=is.numeric)]

outlier_treat <- function(x){
  UC1 = quantile(x, p=0.95,na.rm=T)
  LC1 = quantile(x, p=0.05,na.rm=T)
  x[x>UC1]=UC1
  x[x<LC1]=LC1
  
  return(x)
}

mydata_num_vars = data.frame(apply(li[numeric_vars], 2, FUN=outlier_treat))

# checking the distribution of total spends.

li$lntotal_spend<-log(li$total_spend)
hist(li$lntotal_spend)

# Correlation matrix

corrm<- cor(li[,num_var])                                 
View(corrm)

write.csv(corrm, file = "corrm1.csv")
names(my_dataFA)

#Variable reduction using anova test on categorical variables.
fit <- lm(formula = total_spend ~ region
          +townsize
          +gender
          +agecat
          +birthmonth
          +edcat
          +jobcat
          +union
          +employ
          +empcat
          +retire
          +inccat
          +default
          +jobsat
          +marital
          +spousedcat
          +homeown
          +hometype
          +address
          +addresscat
          +cars
          +carown
          +cartype
          +carcatvalue
          +carbought
          +carbuy
          +commute
          +commutecat
          +commutecar
          +commutemotorcycle
          +commutecarpool
          +commutebus
          +commuterail
          +commutepublic
          +commutebike
          +commutewalk
          +commutenonmotor
          +telecommute
          +reason
          +polview
          +polparty
          +polcontrib
          +vote
          +card
          +cardtype
          +cardbenefit
          +cardfee
          +cardtenure
          +cardtenurecat
          +card2
          +card2type
          +card2benefit
          +card2fee
          +card2tenure
          +card2tenurecat
          +active
          +bfast
          +churn
          +tollfree
          +equip
          +callcard
          +wireless
          +multline
          +voice
          +pager
          +internet
          +callid
          +callwait
          +forward
          +confer
          +ebill
          +owntv
          +ownvcr
          +owndvd
          +owncd
          +ownpda
          +ownpc
          +ownipod
          +owngame
          +ownfax
          +news
          +response_01
          +response_02
          +response_03, data = li)
b <- anova(fit)
b


# step wise regression
fitt <- step(lm(lntotal_spend ~ region
                +townsize
                +gender
                +agecat
                +edcat
                +employ
                +empcat
                +retire
                +inccat
                +hometype
                +commutebike
                +card
                +card2
                +voice
                +internet
                +age
                +ed
                +income
                +lninc
                +debtinc
                +creddebt
                +lncreddebt
                +othdebt
                +lnothdebt
                +spoused
                +reside
                +pets
                +pets_dogs
                +pets_birds
                +pets_reptiles
                +pets_small
                +pets_saltfish
                +pets_freshfish
                +carvalue
                +carditems
                +cardspent
                +card2items
                +card2spent
                +tenure
                +longmon
                +longten
                +lnlongmon
                +lnlongten
                +tollmon
                +lntollmon
                +tollten
                +lntollten
                +equipmon
                +lnequipmon
                +equipten
                +lnequipten
                +cardmon
                +lncardmon
                +cardten
                +lncardten
                +wiremon
                +lnwiremon
                +wireten
                +lnwireten
                +hourstv
                +commutetime
                , data=li), direction = "both")

summary(fitt)

#cONVERTING CATEGORICAL VARIABLES AS FACTORS
li$gender1 <- as.factor(li$gender)
li$edcat1 <- as.factor(li$edcat)
li$inccat1 <- as.factor(li$inccat)
li$card1 <- as.factor(li$card)
li$card2_1 <- as.factor(li$card2)

#Splitting data into Training, Validaton and Testing Dataset
train_ind <- sample(1:nrow(li), size = floor(0.70 * nrow(li)))

training<-li[train_ind,]
testing<-li[-train_ind,]

#Building Models for training dataset


fit <- lm(lntotal_spend ~ gender1 + edcat1 + inccat1 + card1 + card2_1 + age + 
            income + lninc + debtinc + creddebt + othdebt + pets_dogs + 
            longmon + 
            longten + lnlongmon + tollten + lnequipmon + cardmon + cardten + 
            lnwiremon + wireten, data = training)


summary(fit) # show results

require(MASS)
step3<- stepAIC(fit,direction="both")

ls(step3)
step3$anova

# Calculating cooks'd for influential observation.
training$Cd <- cooks.distance(fit)
training1 <- subset(training, Cd< (4/3500))


fit3<-lm(lntotal_spend ~ gender1 + edcat1 + card1 + card2_1 + age + income + 
           lninc + cardmon + cardten + wireten, data = training1)

summary(fit3)
step4<- stepAIC(fit3,direction="both")

#Multicollinierity Check using VIF
library(car)
vif(fit3)

fit4<-lm(lntotal_spend ~ gender1 + edcat1 + card1 + card2_1 + age + income + 
           lninc + wireten, data = training1) 

summary(fit4)

# Standardized beta.

lm.beta<-function (MOD) 
{
  b <- summary(MOD)$coef[-1, 1]
  sx <- sd(MOD$model[-1])
  sy <- sd(MOD$model[1])
  beta <- b * sx/sy
  return(beta)
}


install.packages("QuantPsyc")
library(QuantPsyc)
lm.beta(fit3)

# Score using predict function
t1<-cbind(training1, pred_spent = exp(predict(fit4)))
names(t1)
t1<- transform(t1, APE = abs(pred_spent - total_spend)/total_spend)
mean(t1$APE)
View(t1)

t2<-cbind(testing, pred_spent=exp(predict(fit4,testing)))
t2<- transform(t2, APE = abs(pred_spent - total_spend)/total_spend)
mean(t2$APE)
View(t2)

#Decile Analysis Reports - t1(training)

# find the decile locations 
decLocations <- quantile(t1$pred_spent, probs = seq(0.1,0.9,by=0.1))

# use findInterval with -Inf and Inf as upper and lower bounds
t1$decile <- findInterval(t1$pred_spent,c(-Inf,decLocations, Inf))

require(sqldf)
t1_DA <- sqldf("select decile, count(decile) as count, avg(pred_spent) as avg_pre_spent,   
               avg(total_spend) as avg_actual_spend
               from t1
               group by decile
               order by decile desc")

View(t1_DA)
write.csv(t1_DA, file = "t1_DA.csv")

#Decile Analysis Reports - t2(testing)

# find the decile locations 
decLocations <- quantile(t2$pred_spent, probs = seq(0.1,0.9,by=0.1))

# use findInterval with -Inf and Inf as upper and lower bounds
t2$decile <- findInterval(t2$pred_spent,c(-Inf,decLocations, Inf))

require(sqldf)
t2_DA <- sqldf("select decile, count(decile) as count, avg(pred_spent) as avg_pre_spent,   
               avg(total_spend) as avg_Actual_spend
               from t2
               group by decile
               order by decile desc")

View(t2_DA)
write.csv(t2_DA, file = "t2_DA.csv")















































































































































