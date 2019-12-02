library(ggplot2)
library(psych)     # funkcja describeBy()
library(lawstat)   # funkcja levene.test()
library(dunn.test)
library(tseries)

births <- read_excel("births.xls")

#### Seeing data
str(births)
tail(births,4)
#RACE  = rasa (1 = biała, 2 = czarna, 3 = inna)
#SMOKE = palenie w trakcie ciąży (1 = tak, 0 = nie)
births$AGE[births$AGE < 23]<- 0
births$AGE[births$AGE >= 23 ]<- 1


births$RACE<- factor(births$RACE, labels = c("white", "black", "other"))
births$SMOKE<- factor(births$SMOKE, labels = c("SMOKE", "noSMOKE"))

births$AGE<- factor(births$AGE, labels = c("0-23", "23+"))


str(births)

ggplot(births)+ geom_boxplot(aes(births$RACE, births$BWT, color= births$RACE))
ggplot(births)+ geom_boxplot(aes(births$AGE, births$BWT, color= births$AGE))
ggplot(births)+ geom_boxplot(aes(births$RACE, births$BWT, color= births$AGE))

describeBy(births$BWT, births$RACE, mat = TRUE)
describeBy(births$BWT, births$AGE, mat = TRUE)


tapply(births$BWT, births[, c(1,3)],length)



# normality distribution

shapiro_pvalue<-function (x){
  return (shapiro.test(x)$p.value)
}

shapiro_results<- tapply(births$BWT, births[,c(1,3)], shapiro_pvalue)
shapiro_results
# all groups are showing normality


births_anova <- aov(BWT~AGE*RACE, data = births)
births_anova
summary(births_anova)


