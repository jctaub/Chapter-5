
ballard <- #dataset

#subset to pakistani men
pak_men <- subset(ballard, ballard$ETHGROUP=='6' & ballard$SEX=='1')

table(pak_men$COBIRTH)

#create variables for country of birth
pak_men$birth <- pak_men$COBIRTH

pak_men$birth[pak_men$COBIRTH<=3]<- 'Britain'
pak_men$birth[pak_men$COBIRTH==17]<- 'India'
pak_men$birth[pak_men$COBIRTH==18]<- 'Pakistan'
pak_men$birth[pak_men$COBIRTH>3 & pak_men$COBIRTH<17]<- 'NaN'
pak_men$birth[pak_men$COBIRTH>18]<- 'NaN'

#create age categorical variables
pak_men$agecat <- pak_men$AGE

pak_men$agecat[pak_men$AGE<=4] <- '0-4'
pak_men$agecat[pak_men$AGE>=5 & pak_men$AGE<=9] <- '05-9'
pak_men$agecat[pak_men$AGE>=10 & pak_men$AGE<=14] <- '10-14'
pak_men$agecat[pak_men$AGE>=15 & pak_men$AGE<=19] <- '15-19'
pak_men$agecat[pak_men$AGE>=20 & pak_men$AGE<=24] <- '20-24'
pak_men$agecat[pak_men$AGE>=25 & pak_men$AGE<=29] <- '25-29'
pak_men$agecat[pak_men$AGE>=30 & pak_men$AGE<=34] <- '30-34'
pak_men$agecat[pak_men$AGE>=35 & pak_men$AGE<=39] <- '35-39'
pak_men$agecat[pak_men$AGE>=40 & pak_men$AGE<=44] <- '40-44'
pak_men$agecat[pak_men$AGE>=45 & pak_men$AGE<=49] <- '45-49'
pak_men$agecat[pak_men$AGE>=50 & pak_men$AGE<=54] <- '50-54'
pak_men$agecat[pak_men$AGE>=55 & pak_men$AGE<=59] <- '55-59'
pak_men$agecat[pak_men$AGE>=60 & pak_men$AGE<=64] <- '60-64'
pak_men$agecat[pak_men$AGE>=65 & pak_men$AGE<=69] <- '65-69'
pak_men$agecat[pak_men$AGE>=70 & pak_men$AGE<=74] <- '70-74'
pak_men$agecat[pak_men$AGE>=75 & pak_men$AGE<=79] <- '75-79'
pak_men$agecat[pak_men$AGE>=80 & pak_men$AGE<=84] <- '80-84'
pak_men$agecat[pak_men$AGE>=85 & pak_men$AGE<=89] <- '85-89'
pak_men$agecat[pak_men$AGE>=90] <- '90-95'



#create figures
#figure 5.1
counts <- table(pak_men$birth, pak_men$agecat)

counts <- counts[, c("0-4", '5-9', '10-14', '15-19', '20-24', '25-29',
                     '30-34', '35-39', '40-44', '45-49', '50-54', '55-59',
                     '60-64', '65-69', '70-74', '75-79', '80-84', '85-89',
                     '90-95')]

barplot(counts, xlab = "Age Group", legend= rownames(counts))

# figure 5.2

#subset to pakistani women
pak_women <- subset(ballard, ballard$SEX=='2' & ballard$ETHGROUP =='6')

pak_women$mar_stat <- pak_women$MSTATUS

pak_women$mar_stat[pak_women$MSTATUS==1] <- 'single'
pak_women$mar_stat[pak_women$MSTATUS==2] <- 'married'
pak_women$mar_stat[pak_women$MSTATUS==3] <- 'married'
pak_women$mar_stat[pak_women$MSTATUS==4] <- 'divorced'
pak_women$mar_stat[pak_women$MSTATUS==5] <- 'widowed'


pak_women$agecat <- pak_women$AGE

pak_women$agecat[pak_women$AGE<=4] <- '0-4'
pak_women$agecat[pak_women$AGE>=5 & pak_women$AGE<=9] <- '05-9'
pak_women$agecat[pak_women$AGE>=10 & pak_women$AGE<=14] <- '10-14'
pak_women$agecat[pak_women$AGE>=15 & pak_women$AGE<=19] <- '15-19'
pak_women$agecat[pak_women$AGE>=20 & pak_women$AGE<=24] <- '20-24'
pak_women$agecat[pak_women$AGE>=25 & pak_women$AGE<=29] <- '25-29'
pak_women$agecat[pak_women$AGE>=30 & pak_women$AGE<=34] <- '30-34'
pak_women$agecat[pak_women$AGE>=35 & pak_women$AGE<=39] <- '35-39'
pak_women$agecat[pak_women$AGE>=40 & pak_women$AGE<=44] <- '40-44'
pak_women$agecat[pak_women$AGE>=45 & pak_women$AGE<=49] <- '45-49'
pak_women$agecat[pak_women$AGE>=50 & pak_women$AGE<=54] <- '50-54'
pak_women$agecat[pak_women$AGE>=55 & pak_women$AGE<=59] <- '55-59'
pak_women$agecat[pak_women$AGE>=60 & pak_women$AGE<=64] <- '60-64'
pak_women$agecat[pak_women$AGE>=65 & pak_women$AGE<=69] <- '65-69'
pak_women$agecat[pak_women$AGE>=70 & pak_women$AGE<=74] <- '70-74'
pak_women$agecat[pak_women$AGE>=75 & pak_women$AGE<=79] <- '75-79'
pak_women$agecat[pak_women$AGE>=80 & pak_women$AGE<=84] <- '80-84'
pak_women$agecat[pak_women$AGE>=85 & pak_women$AGE<=89] <- '85-89'
pak_women$agecat[pak_women$AGE>=90] <- '90-95'


counts2 <- table(pak_women$mar_stat, pak_women$agecat)

counts2 <- counts2[, c("0-4", '5-9', "10-14", '15-19', '20-24', '25-29',
                       '30-34', '35-39', '40-44', '45-49', '50-54', '55-59',
                       '60-64', '65-69', '70-74', '75-79', '80-84', '85-89',
                       '90-95')]


barplot(counts2,  xlab = "Age Group", col=c("darkblue","red", "purple", "green"), legend= rownames(counts2))



#use DCOUNTY and REGIONP for geography variables

ballard$breg<- ballard$REGIONP

ballard$breg<- ifelse(ballard$REGIONP== '2', 'York and East Anglia', ballard$breg)
ballard$breg<- ifelse(ballard$REGIONP== '4', 'York and East Anglia', ballard$breg)
ballard$breg<- ifelse(ballard$DCOUNTY=='8', 'west york', ballard$breg)
ballard$breg<- ifelse(ballard$REGIONP=='3', 'east midlands', ballard$breg)
ballard$breg<- ifelse(ballard$REGIONP=='5', 'inner london', ballard$breg)
ballard$breg<- ifelse(ballard$REGIONP=='6', 'outer london', ballard$breg)
ballard$breg<- ifelse(ballard$REGIONP=='7', 'SE England', ballard$breg)
ballard$breg<- ifelse(ballard$REGIONP=='8', 'wales and SW', ballard$breg)
ballard$breg<- ifelse(ballard$REGIONP=='11', 'wales and SW', ballard$breg)
ballard$breg<- ifelse(ballard$REGIONP=='9', 'west midlands', ballard$breg)
ballard$breg<- ifelse(ballard$REGIONP=='12', 'scottland', ballard$breg)
ballard$breg<- ifelse(ballard$REGIONP=='1', 'NE', ballard$breg)
ballard$breg<- ifelse(ballard$REGIONP=='10', 'NW', ballard$breg)
ballard$breg<- ifelse(ballard$DCOUNTY=='3', 'greater manchester', ballard$breg)
ballard$breg<- ifelse(ballard$DCOUNTY=='31', 'lancashire', ballard$breg)

pakistani <- subset(ballard, ballard$ETHGROUP=='6')

pie <-table(pakistani$breg)
prop.pie<-prop.table(pie)*100

#south asian subset

south.asian<- subset(ballard, ballard$ETHGROUP=='5'|ballard$ETHGROUP=='6'|
                       ballard$ETHGROUP=='7')

counts3<- table(south.asian$ETHGROUP, south.asian$breg)

counts3<- counts3[,c("NE", "west york", 'York and East Anglia', 'east midlands', 
                     'inner london', 'outer london', 'SE England', 'west midlands', 
                     'greater manchester', 'lancashire', 'wales and SW', 'scottland')]

barplot(counts3, legend = rownames(counts3))