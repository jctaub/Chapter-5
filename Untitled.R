library(haven)
#gb91ind <- read_dta("~/Desktop/PhD work/UKDA-7210-stata9/stata9/gb91ind.dta")

eade<-#dataset



#table 6.3
#subset to employed people

employed <- subset(eade, eade$SOCLASS=='1'|eade$SOCLASS=='2'|eade$SOCLASS=='3'|
                     eade$SOCLASS=='4'|eade$SOCLASS=='5'|eade$SOCLASS=='6')

employed$AGE <- as.numeric(employed$AGE)

#subset by age
employed <- subset(employed, employed$AGE>= 16)


employed$variable<-'11'

employed$var2<- '12'

#subset to men
men <- subset(employed, employed$SEX=='1')

table6.3<-table(men$ETHGROUP, men$SOCLASS)
table6.3<-prop.table(table6.3, 1)*100



table6.3b<- table(men$SOCLASS)
table6.3b<-prop.table(table6.3b)*100

#subset to born in ireland

irishmen<- subset(men, men$COBIRTH=='4'| men$COBIRTH=='6')

table6.3c <- table(irishmen$SOCLASS)
table6.3c<-prop.table(table6.3c)*100


key6.3<- as.data.frame(table(men$ETHGROUP))
key6.3b<- as.data.frame(table(men$variable))
key6.3c<-as.data.frame(table(irishmen$var2))

key6.3<- rbind(key6.3, key6.3b)

key6.3<- rbind(key6.3, key6.3c)


#table 6.4 women

women<- subset(employed, employed$SEX=='2')

table6.4 <- table(women$ETHGROUP, women$SOCLASS)
table6.4<-prop.table(table6.4, 1)*100

key6.4<-table(women$ETHGROUP)

table6.4b <- table(women$SOCLASS)
table6.4b<-prop.table(table6.4b)*100

#irishwomen

irishwomen <- subset(women, women$COBIRTH=='4'| women$COBIRTH=='6')

table6.4c <- table(irishwomen$SOCLASS)
table6.4c<-prop.table(table6.4c)*100



tab6.3df<- as.data.frame(table6.3)
tab6.3bdf<- as.data.frame(table6.3b)
tab6.3cdf<-as.data.frame(table6.3c)

tab6.3bdf$Var2<- tab6.3bdf$Var1
tab6.3bdf$Var1<-'11'

tab6.3df<-rbind(tab6.3df, tab6.3bdf)

tab6.3cdf$Var2<-tab6.3cdf$Var1
tab6.3cdf$Var1<-'12'

tab6.3df<-rbind(tab6.3df, tab6.3cdf)



tab6.4df<-as.data.frame(table6.4)
tab6.4bdf<-as.data.frame(table6.4b)
tab6.4cdf<-as.data.frame(table6.4c)


tab6.4bdf$Var2<- tab6.4bdf$Var1
tab6.4bdf$Var1<-'11'

tab6.4df<-rbind(tab6.4df, tab6.4bdf)

tab6.4cdf$Var2<-tab6.4cdf$Var1
tab6.4cdf$Var1<-'12'

tab6.4df<-rbind(tab6.4df, tab6.4cdf)


key6.4<- as.data.frame(table(women$ETHGROUP))
key6.4b<-as.data.frame(table(women$variable))
key6.4c<- as.data.frame(table(irishmen$var2))

key6.4<- rbind(key6.4, key6.4b)
key6.4<- rbind(key6.4, key6.4c)




