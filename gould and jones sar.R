#library(haven)
#gb91ind <- read_dta("~/Desktop/PhD work/UKDA-7210-stata9/stata9/gb91ind.dta")

#put desired dataset whether it be original or synthetic into thename gould
gould<- #dataset

middle<-subset(gould, gould$AGE>=30) 
middle<-subset(middle, middle$AGE<=60) 
middle<-subset(middle, middle$RESIDSTA=='1'|middle$RESIDSTA=='2')

#create age groups with ages 30-39 as dummy

middle$agegroup<- middle$AGE

middle$agegroup[middle$AGE<=39]<- '30-39'
middle$agegroup[middle$AGE>=40 & middle$AGE<=49]<- '40-49'
middle$agegroup[middle$AGE>=50]<- '50-60'

middle$age40_49<-middle$agegroup 

middle$age40_49<- ifelse(middle$agegroup=='40-49', 1, middle$age40_49)
middle$age40_49<- ifelse(middle$agegroup!='40-49', 0, middle$age40_49)

middle$age50_60<- middle$agegroup

middle$age50_60<- ifelse(middle$agegroup=='50-60', 1, middle$age50_60)
middle$age50_60<- ifelse(middle$agegroup!='50-60', 0, middle$age50_60)

#Create race dummies
middle$race<- middle$ETHGROUP

middle$race<- ifelse(middle$ETHGROUP=='1', 'white', middle$race)
middle$race<- ifelse(middle$ETHGROUP!='1', 'black', middle$race)

middle$black<- middle$race

middle$black<- ifelse(middle$race=='black', 1, middle$black)
middle$black<- ifelse(middle$race!= 'black', 0, middle$black)

#create sex dummies
middle$female<- middle$SEX

middle$female<- ifelse(middle$SEX=='2', 1, middle$female)
middle$female<- ifelse(middle$SEX!='2', 0, middle$female)

#create illness dummies

middle$ill<- middle$LTILL

middle$ill<- ifelse(middle$LTILL=='1', 1, middle$ill)
middle$ill<- ifelse(middle$LTILL!='1', 0, middle$ill)

middle$ill<- as.numeric(middle$ill)

#create interaction terms
middle$female<- as.numeric(middle$female)
middle$age40_49<- as.numeric(middle$age40_49)
middle$age50_60<- as.numeric(middle$age50_60)
middle$black<- as.numeric(middle$black)


middle$female40_49<- middle$female*middle$age40_49
middle$female50_60<- middle$female*middle$age50_60
middle$black_female<- middle$female*middle$black
middle$black40_49<- middle$black*middle$age40_49
middle$black50_60<- middle$black*middle$age50_60


#library(lme4)


#create model
modelA<- glmer(ill~age40_49+ age50_60+ female+ female40_49+ female50_60+ black+
                 black_female+ black40_49+ black50_60+ (1|AREAP), data =middle, 
               family = binomial("logit"))
summary(modelA)

modelA_tidy<- tidy(modelA)

write_excel_csv(modelA_tidy, "~/Downloads/gould_single6.csv")

########################################################

#library(devtools)
#library(broom)

#model b does not work
#dummies for model B

middle$nocar<- middle$CARS
middle$nocar<- ifelse(middle$CARS=='0', 1, middle$nocar)
middle$nocar<- ifelse(middle$CARS!='0', 0, middle$nocar)

middle$CARS<- ifelse(middle$CARS=='3', '2', middle$CARS)

middle$two_plus_cars<- middle$CARS
middle$two_plus_cars<- ifelse(middle$CARS=='2', 1, middle$two_plus_cars)
middle$two_plus_cars<- ifelse(middle$CARS!='2', 0, middle$two_plus_cars)

middle$housing_tenure<- middle$TENURE

middle$housing_tenure[middle$TENURE=='1' | middle$TENURE=='2']<- 'owner'
middle$housing_tenure[middle$TENURE=='7'| middle$TENURE=='8'| middle$TENURE=='9'|
                        middle$TENURE=='10']<- 'rent_LA'
middle$housing_tenure[middle$TENURE=='3'| middle$TENURE=='4'| middle$TENURE=='5'|
                        middle$TENURE=='6']<- 'rent_private'

middle$rent_LA<- middle$housing_tenure
middle$rent_LA<- ifelse(middle$housing_tenure=='rent_LA', 1, middle$rent_LA)
middle$rent_LA<- ifelse(middle$housing_tenure!='rent_LA', 0, middle$rent_LA)

middle$rent_private<- middle$housing_tenure
middle$rent_private<- ifelse(middle$housing_tenure=='rent_private', 1, middle$rent_private)
middle$rent_private<- ifelse(middle$housing_tenure!='rent_private', 0, middle$rent_private)

middle$social_class<- middle$SOCLASS

middle$social_class[middle$SOCLASS=='1'| middle$SOCLASS=='2']<-'prof_managerial'
middle$social_class[middle$SOCLASS=='3'| middle$SOCLASS=='4']<- 'skilled'
middle$social_class[middle$SOCLASS=='5'| middle$SOCLASS=='6']<- 'unskilled'
middle$social_class[middle$SOCLASS=='7'| middle$SOCLASS=='8'|
                      middle$SOCLASS=='9']<-'other_class'

middle$class1_2<- middle$social_class
middle$class1_2<- ifelse(middle$social_class=='prof_managerial', 1, middle$class1_2)
middle$class1_2<- ifelse(middle$social_class!='prof_managerial', 0, middle$class1_2)

middle$class4_5<- middle$social_class
middle$class4_5<- ifelse(middle$social_class=='unskilled', 1, middle$class4_5)
middle$class4_5<- ifelse(middle$social_class!='unskilled', 0, middle$class4_5)

middle$class_other<- middle$social_class
middle$class_other<- ifelse(middle$social_class=='other_class', 1, middle$class_other)
middle$class_other<- ifelse(middle$social_class!='other_class', 0, middle$class_other)

middle$nocar<- as.numeric(middle$nocar)
middle$two_plus_cars<- as.numeric(middle$two_plus_cars)
middle$rent_LA<- as.numeric(middle$rent_LA)
middle$rent_private<-as.numeric(middle$rent_private)
middle$class1_2<- as.numeric(middle$class1_2)
middle$class4_5<-as.numeric(middle$class4_5)
middle$class_other<- as.numeric(middle$class_other)

middle$nocar_rentLA<- middle$nocar*middle$rent_LA
middle$nocar_private<- middle$nocar*middle$rent_private
middle$nocar_c1_2<- middle$nocar*middle$class1_2
middle$nocar_class_other<- middle$nocar*middle$class_other
middle$twocar_cl_2<- middle$two_plus_cars*middle$class1_2
middle$twocar_other<- middle$two_plus_cars*middle$class_other
middle$LA_c1_2<- middle$rent_LA*middle$class1_2
middle$LA_other<- middle$rent_LA* middle$class_other
middle$private_c1_2<- middle$rent_private*middle$class1_2
middle$private_c4_5<- middle$rent_private*middle$class4_5

modelB<- glmer(ill~age40_49+ age50_60+ female+ female40_49+ female50_60+ black+
                 black_female+ black40_49+ black50_60+ nocar+ two_plus_cars+ rent_LA+
                 rent_private+ class1_2+ class4_5+ class_other+ nocar_rentLA+ nocar_private+
                 nocar_c1_2+ nocar_c1_2+ twocar_cl_2+ twocar_other+ LA_c1_2+ LA_other+
                 private_c1_2+ private_c4_5 +(1|AREAP), data =middle, 
               family = binomial("logit"))
 
summary(modelB)


















