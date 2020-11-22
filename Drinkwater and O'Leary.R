drinklear<-#dataset

wales <- subset(drinklear, drinklear$REGIONP=='11')


#subset by age, economic, language
wales<- subset(wales, wales$AGE>=16)
wales<- subset(wales, wales$AGE<=64)

wales<- subset(wales, wales$ECONPRIM=='1'|wales$ECONPRIM=='2'| wales$ECONPRIM=='3'|
                 wales$ECONPRIM=='4'|wales$ECONPRIM=='5'|wales$ECONPRIM=='6')

wales<- subset(wales, wales$RESIDSTA!='3')

wales<- subset(wales, wales$WELSHLAN!='NaN')

#subset to male
male<- subset(wales, wales$SEX=='1')
male<- subset(male, male$AGE<=64)

table1m<-table(male$WELSHLAN)
table1m
prop.table(table1m)*100

#subset to female and by age
female<- subset(wales, wales$SEX=='2')
female<- subset(female, female$AGE<=59)

table1f<- table(female$WELSHLAN)
table1f
prop.table(table1f)*100

#table 3 fnot from SAR

#table 5 probit

#subset wales by age

#create age category varaibles

wales$AGE<- as.numeric(wales$AGE)

wales$agecat<- wales$AGE

wales$agecat[wales$AGE<=19]<-'age16-19'
wales$agecat[wales$AGE>=20 & wales$AGE<=24]<-'age20-24'
wales$agecat[wales$AGE>=25 & wales$AGE<=39]<- 'age25-39'
wales$agecat[wales$AGE>=40 & wales$AGE<=49]<- 'age40-49'
wales$agecat[wales$AGE>=50]<- 'age50-64'


#making dummies

wales$age16_19<- wales$agecat
wales$age16_19<- ifelse(wales$agecat=='age16-19', 1, wales$age16_19)
wales$age16_19<- ifelse(wales$agecat!='age16-19', 0, wales$age16_19)
wales$age16_19<- as.factor(wales$age16_19)

wales$age20_24<- wales$agecat
wales$age20_24<- ifelse(wales$agecat=='age20-24', 1, wales$age20_24)
wales$age20_24<- ifelse(wales$agecat!='age20-24', 0, wales$age20_24)
wales$age20_24<- as.factor(wales$age20_24)

wales$age25_39<- wales$agecat
wales$age25_39<- ifelse(wales$agecat=='age25-39', 1, wales$age25_39)
wales$age25_39<- ifelse(wales$agecat!='age25-39', 0, wales$age25_39)
wales$age25_39<- as.factor(wales$age25_39)

wales$age50_64<-wales$agecat
wales$age50_64<- ifelse(wales$agecat=='age50-64', 1, wales$age50_64)
wales$age50_64<- ifelse(wales$agecat!='age50-64', 0, wales$age50_64)
wales$age50_64<- as.factor(wales$age50_64)

#dummies for marraige

wales$single<- wales$MSTATUS
wales$single<- ifelse(wales$MSTATUS=='1', 1, wales$single)
wales$single<- ifelse(wales$MSTATUS!='1', 0, wales$single)


wales$MSTATUS<- ifelse(wales$MSTATUS=='5', '4', wales$MSTATUS)

wales$divorce_widow<- wales$MSTATUS
wales$divorce_widow<- ifelse(wales$MSTATUS=='4', 1,  wales$divorce_widow)
wales$divorce_widow<- ifelse(wales$MSTATUS!='4', 0,  wales$divorce_widow)
wales$divorce_widow<- as.factor(wales$divorce_widow)

wales$dep_child<- wales$DEPCHILD
wales$dep_child<- ifelse(wales$DEPCHILD=='1', 1, wales$dep_child)
wales$dep_child<- ifelse(wales$DEPCHILD!='1', 0, wales$dep_child)

wales$qual1<- wales$QUALNUM
wales$qual1<- ifelse(wales$QUALNUM=='1', 1, wales$qual1)
wales$qual1<- ifelse(wales$QUALNUM!='1', 0, wales$qual1)

wales$qual2<- wales$QUALNUM
wales$qual2<- ifelse(wales$QUALNUM=='2', 1, wales$qual2)
wales$qual2<- ifelse(wales$QUALNUM!='2', 0, wales$qual2)

wales$own_outright<- wales$TENURE
wales$own_outright<- ifelse(wales$TENURE=='1', 1, wales$own_outright)
wales$own_outright<- ifelse(wales$TENURE!='1', 0, wales$own_outright)

wales$rent_furnished<- wales$TENURE
wales$rent_furnished<- ifelse(wales$TENURE=='3', 1, wales$rent_furnished)
wales$rent_furnished<- ifelse(wales$TENURE!='3', 0, wales$rent_furnished)

wales$rent_unfurnished<- wales$TENURE
wales$rent_unfurnished<- ifelse(wales$TENURE=='4', 1, wales$rent_unfurnished)
wales$rent_unfurnished<- ifelse(wales$TENURE!='4', 0, wales$rent_unfurnished)

wales$rent_job<- wales$TENURE
wales$rent_job<- ifelse(wales$TENURE=='5', 1, wales$rent_job)
wales$rent_job<- ifelse(wales$TENURE!='5', 0, wales$rent_job)

wales$rent_HA<- wales$TENURE
wales$rent_HA<-ifelse(wales$TENURE=='6', 1, wales$rent_HA)
wales$rent_HA<-ifelse(wales$TENURE!='6', 0, wales$rent_HA)

wales$rent_NT<- wales$TENURE
wales$rent_NT<- ifelse(wales$TENURE=='7', 1, wales$rent_NT)
wales$rent_NT<- ifelse(wales$TENURE!='7', 0, wales$rent_NT)

wales$born_not_wales<- wales$COBIRTH
wales$born_not_wales<- ifelse(wales$COBIRTH=='3', 0, wales$born_not_wales)
wales$born_not_wales<- ifelse(wales$COBIRTH!='3', 1, wales$born_not_wales)

#counties with west glamorgan as dummy

wales$clwyd<- wales$DCOUNTY
wales$clwyd<- ifelse(wales$DCOUNTY=='48', 1, wales$clwyd)
wales$clwyd<- ifelse(wales$DCOUNTY!='48', 0, wales$clwyd)

wales$dyfed<- wales$DCOUNTY
wales$dyfed<- ifelse(wales$DCOUNTY=='49', 1, wales$dyfed)
wales$dyfed<- ifelse(wales$DCOUNTY!='49', 0, wales$dyfed)

wales$gwent<- wales$DCOUNTY
wales$gwent<- ifelse(wales$DCOUNTY=='50', 1, wales$gwent)
wales$gwent<- ifelse(wales$DCOUNTY!='50', 0, wales$gwent)

wales$gwynedd<- wales$DCOUNTY
wales$gwynedd<- ifelse(wales$DCOUNTY=='51', 1, wales$gwynedd)
wales$gwynedd<- ifelse(wales$DCOUNTY!='51', 0, wales$gwynedd)

wales$mid_glamorgan<- wales$DCOUNTY
wales$mid_glamorgan<- ifelse(wales$DCOUNTY=='52', 1, wales$mid_glamorgan)
wales$mid_glamorgan<- ifelse(wales$DCOUNTY!='52', 0, wales$mid_glamorgan)

wales$powys<- wales$DCOUNTY
wales$powys<-ifelse(wales$DCOUNTY=='53', 1, wales$powys)
wales$powys<-ifelse(wales$DCOUNTY!='53', 0, wales$powys)

wales$south_glamorgan<- wales$DCOUNTY
wales$south_glamorgan<- ifelse(wales$DCOUNTY=='54', 1, wales$south_glamorgan)
wales$south_glamorgan<- ifelse(wales$DCOUNTY!='54', 0, wales$south_glamorgan)

#does not know welsh dummy

wales$speaks_welsh<- wales$WELSHLAN
wales$speaks_welsh<- ifelse(wales$WELSHLAN=='1', 1, wales$speaks_welsh)
wales$speaks_welsh<- ifelse(wales$WELSHLAN!='1', 0, wales$speaks_welsh)

wales$spk_rd_or_spk_wr<- wales$WELSHLAN
wales$spk_rd_or_spk_wr<- ifelse(wales$WELSHLAN=='2', 1, wales$spk_rd_or_spk_wr)
wales$spk_rd_or_spk_wr<- ifelse(wales$WELSHLAN!='2', 0, wales$spk_rd_or_spk_wr)

wales$read_or_write<- wales$WELSHLAN
wales$read_or_write<-ifelse(wales$WELSHLAN=='3', 1, wales$read_or_write)
wales$read_or_write<-ifelse(wales$WELSHLAN!='3', 0, wales$read_or_write)

wales$speak_read_write<- wales$WELSHLAN
wales$speak_read_write<- ifelse(wales$WELSHLAN=='4', 1, wales$speak_read_write)
wales$speak_read_write<- ifelse(wales$WELSHLAN!='4', 0, wales$speak_read_write)

wales$unemployed<- wales$ECONPRIM
wales$unemployed<- ifelse(wales$ECONPRIM=='6', 1, wales$unemployed)
wales$unemployed<- ifelse(wales$ECONPRIM!='6', 0, wales$unemployed)

wales$ill<- wales$LTILL
wales$ill<- ifelse(wales$LTILL=='1', 1, wales$ill)
wales$ill<- ifelse(wales$LTILL!='1', 0, wales$ill)

wales$employed<- wales$ECONPRIM
wales$employed<- ifelse(wales$ECONPRIM!='6', 1, wales$employed)
wales$employed<- ifelse(wales$ECONPRIM=='6', 0, wales$employed)


male<- subset(wales, wales$SEX=='1')

male_probit<- glm(unemployed~age16_19+ age20_24+ age25_39+ age50_64+ single+
                    divorce_widow+ dep_child+ ill+ qual1+ qual2+ own_outright+
                    rent_furnished+ rent_unfurnished+ rent_job+ rent_HA+ rent_NT+
                    born_not_wales+ clwyd+ dyfed+ gwent+ gwynedd+ mid_glamorgan+ powys+
                    south_glamorgan+ speak_read_write+ spk_rd_or_spk_wr+ speaks_welsh+
                    read_or_write, family = binomial(link = "probit"), data = male)
summary(male_probit)

#library(devtools)
#library(broom)



#confint(male_probit)

female<- subset(wales, wales$SEX=='2')
female<- subset(female, female$AGE<=59)

female_probit<- glm(unemployed~age16_19+ age20_24+ age25_39+ age50_64+ single+
                      divorce_widow+ dep_child+ ill+ qual1+ qual2+ own_outright+
                      rent_furnished+ rent_unfurnished+ rent_job+ rent_HA+ rent_NT+
                      born_not_wales+ clwyd+ dyfed+ gwent+ gwynedd+ mid_glamorgan+ powys+
                      south_glamorgan+ speak_read_write+ spk_rd_or_spk_wr+ speaks_welsh+
                      read_or_write, family = binomial(link = "probit"), data = female)

summary(female_probit)

#probit_male_single<- tidy(male_probit)
#probit_female_single<- tidy(female_probit)

drink_syn<- tidy(male_probit)
drink_syn_fem<- tidy(female_probit)

