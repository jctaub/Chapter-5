
library(haven)
gb91ind <- read_dta("~/Desktop/PhD work/UKDA-7210-stata9/stata9/gb91ind.dta")

#put in dataset
gardiner<- #dataset

  #subset to older adults living in sheffield
sheffield<- subset(gardiner, gardiner$AREAP=='51')
old<- subset(sheffield, sheffield$AGE>=50)

#dependent variable whether one had access to car
#Yes=1, No= 0

old$carslog<- old$CARS

old$carslog<- ifelse(old$CARS=='1', 1, old$carslog)
old$carslog<- ifelse(old$CARS=='2', 1, old$carslog)
old$carslog<- ifelse(old$CARS=='3', 1, old$carslog)
old$carslog<- ifelse(old$CARS=='NaN', 0, old$carslog)

table(old$carslog)

old$carslog<- factor(old$carslog)


#recode ethnicity, with other as dummy

old$white<- old$ETHGROUP

old$white<- ifelse(old$ETHGROUP=='1', 1, old$white)
old$white<- ifelse(old$ETHGROUP!='1', 0, old$white)

old$black_carr<- old$ETHGROUP

old$black_carr<- ifelse(old$ETHGROUP=='2', 1, old$black_carr)
old$black_carr<- ifelse(old$ETHGROUP!='2', 0, old$black_carr)

old$black_afr<- old$ETHGROUP

old$black_afr<- ifelse(old$ETHGROUP=='3', 1, old$black_afr)
old$black_afr<- ifelse(old$ETHGROUP!='3', 0, old$black_afr)

old$black_oth<- old$ETHGROUP

old$black_oth<- ifelse(old$ETHGROUP=='4', 1, old$black_oth)
old$black_oth<- ifelse(old$ETHGROUP!='4', 0, old$black_oth)

old$indian<- old$ETHGROUP

old$indian<- ifelse(old$ETHGROUP=='5', 1, old$indian)
old$indian<- ifelse(old$ETHGROUP!='5', 0, old$indian)

old$pakistani<- old$ETHGROUP

old$pakistani<- ifelse(old$ETHGROUP=='6', 1, old$pakistani)
old$pakistani<- ifelse(old$ETHGROUP!='6', 0, old$pakistani)

old$bangladeshi<- old$ETHGROUP

old$bangladeshi<- ifelse(old$ETHGROUP=='7', 1, old$bangladeshi)
old$bangladeshi<-ifelse(old$ETHGROUP!='7', 0, old$bangladeshi)

old$chinese<- old$ETHGROUP

old$chinese<-ifelse(old$ETHGROUP=='8', 1, old$chinese)
old$chinese<- ifelse(old$ETHGROUP!='8', 0, old$chinese)

old$asian_oth<- old$ETHGROUP

old$asian_oth<- ifelse(old$ETHGROUP=='9', 1, old$asian_oth)
old$asian_oth<- ifelse(old$ETHGROUP!='9', 0, old$asian_oth)


#tenure dummies, rented local authority or newt won in england or wales constant
#8,9,10 irrelevant since sheffield

old$tenure1<- old$TENURE

old$tenure1<- ifelse(old$TENURE=='1', 1, old$tenure1)
old$tenure1<- ifelse(old$TENURE!='1', 0, old$tenure1)

old$tenure2<- old$TENURE

old$tenure2<- ifelse(old$TENURE=='2', 1, old$tenure2)
old$tenure2<- ifelse(old$TENURE!='2', 0, old$tenure2)

old$tenure3<- old$TENURE

old$tenure3<- ifelse(old$TENURE=='3', 1, old$tenure3)
old$tenure3<- ifelse(old$TENURE!='3', 0, old$tenure3)

old$tenure4<- old$TENURE

old$tenure4<- ifelse(old$TENURE=='4', 1, old$tenure4)
old$tenure4<- ifelse(old$TENURE!='4', 0, old$tenure4)

old$tenure5<- old$TENURE

old$tenure5<- ifelse(old$TENURE=='5', 1, old$tenure5)
old$tenure5<- ifelse(old$TENURE!='5', 0, old$tenure5)

old$tenure6<- old$TENURE

old$tenure6<- ifelse(old$TENURE=='6', 1, old$tenure6)
old$tenure6<- ifelse(old$TENURE!='6', 0, old$tenure6)

#use male as dummy variable

old$male<- old$SEX

old$male<- ifelse(old$SEX=='1', 1, old$male)
old$male<- ifelse(old$SEX!='1', 0, old$male)

#recode longterm limiting illness

old$illness<- old$LTILL

old$illness<- ifelse(old$LTILL=='1', 1, old$illness)
old$illness<- ifelse(old$LTILL!='1', 0, old$illness)

old$healthy<- old$LTILL

old$healthy<- ifelse(old$LTILL=='2', 1, old$healthy)
old$healthy<- ifelse(old$LTILL!='2', 0, old$healthy)

logreg<- glm(carslog~AGE+ male+ tenure1+ tenure2+ tenure3+ tenure4+ tenure5+ tenure6+ 
               white+ black_carr+ black_afr+ black_oth+ indian+ pakistani+ bangladeshi+
               chinese+ asian_oth+ illness, data = old, family = binomial(link = "logit"))
summary(logreg)



#recode ethnic to white and other

old$ethnic<- old$ETHGROUP

old$ethnic<- ifelse(old$ETHGROUP!='1', 1, old$ethnic)
old$ethnic<- ifelse(old$ETHGROUP=='1', 0, old$ethnic)

old$owner<- old$TENURE

old$owner<- ifelse(old$TENURE=='1', 1, old$owner)
old$owner<- ifelse(old$TENURE=='2', 1, old$owner)
old$owner<- ifelse(old$owner!=1, 0, old$owner)


old$age<- old$AGE-50

logreg2<- glm(carslog~AGE+ male+ owner+ white+ illness, data = old, family = "binomial")
summary(logreg2)

#library(devtools)
#library(broom)

logreg_single<- tidy(logreg2)


#write.csv(logreg_single, '~/downloads/logreg_gardsingle6.csv')

























