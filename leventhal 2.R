#library(haven)
#gb91ind <- read_dta("~/Desktop/PhD work/UKDA-7210-stata9/stata9/gb91ind.dta")
#library(readr)



leventhal<- #dataset



#will need to subset to base pop and then make a target varaible

base<- subset(leventhal, leventhal$AGE>=16)

base$target<- base$RELAT

base$target<- ifelse(base$RELAT=='1', 1, base$target)
base$target<- ifelse(base$RELAT!='1', 0, base$target)
base$target<- ifelse(base$AGE<55, 0, base$target)
base$target<- ifelse(base$ECPOSFHP!='1', 0, base$target)
base$target<- ifelse(base$TENURE!='1', 0, base$target)

table(base$target)

table1<-table(base$target, base$REGIONP)
table1<-prop.table(table1, 2)*100


#change percentage  based on dataset
DFtable1<- as.data.frame(table1)
DFtable1<- subset(DFtable1, DFtable1$Var1==1)
DFtable1$Freq<- DFtable1$Freq*100/1.46

keep<- c("Var2", "Freq")
Table1<-DFtable1[, keep]

table2<- table(base$target, base$MSTATUS)
table2<-prop.table(table2, 2)*100

DFtable2<- as.data.frame(table2)
DFtable2<- subset(DFtable2, DFtable2$Var1==1)
DFtable2$Freq<- DFtable2$Freq*100/1.46

Table2<- DFtable2[, keep]

base$race<- base$ETHGROUP

base$race<- ifelse(base$ETHGROUP=='1', 'white', base$race)
base$race<- ifelse(base$ETHGROUP!='1', 'non=white', base$race)

table3<- table(base$target, base$race)
table3<-prop.table(table3, 2)*100

DFtable3<-as.data.frame(table3)
DFtable3<- subset(DFtable3, DFtable3$Var1==1)
DFtable3$Freq<- DFtable3$Freq*100/1.46

Table3<- DFtable3[, keep]

#table 4


#subsett to no NA, reset percentage to 2.02

base1<- subset(base, base$SOCLASS!='NaN')

table4<- table(base1$target, base1$SOCLASS)
table4<-prop.table(table4, 2)*100

DFtable4<-as.data.frame(table4)
DFtable4<- subset(DFtable4, DFtable4$Var1==1)
DFtable4$Freq<- DFtable4$Freq*100/2.02

Table4<- DFtable4[, keep]

#table 5
#using number fomr social group, hoping their missing is the same

table5<- table(base1$target, base1$SEGROUP)
table5<-prop.table(table5, 2)*100

DFtable5<-as.data.frame(table5)
DFtable5<- subset(DFtable5, DFtable5$Var1==1)
DFtable5$Freq<- DFtable5$Freq*100/2.02

Table5<- DFtable5[, keep]


#table 6
table6<- table(base$target, base$CENHEAT)
table6<-prop.table(table6, 2)*100

DFtable6<-as.data.frame(table6)
DFtable6<- subset(DFtable6, DFtable6$Var1==1)
DFtable6$Freq<- DFtable6$Freq*100/1.46

Table6<- DFtable6[, keep]

#table 7
base2<- subset(base, base$CARS!='NaN')

table7<- table(base2$target, base2$CARS)
table7<-prop.table(table7, 2)*100

DFtable7<-as.data.frame(table7)
DFtable7<- subset(DFtable7, DFtable7$Var1==1)
DFtable7$Freq<- DFtable7$Freq*100/1.51

Table7<- DFtable7[, keep]



var_red<-c('Var2', 'Freq')

DFtable1<- DFtable1[, var_red]
DFtable2<- DFtable2[, var_red]
DFtable3<- DFtable3[, var_red]
DFtable4<- DFtable4[, var_red]
DFtable5<- DFtable5[, var_red]
DFtable6<- DFtable6[, var_red]
DFtable7<- DFtable7[, var_red]



View(DFtable1)
View(DFtable2)
View(DFtable3)
View(DFtable4)
View(DFtable5)
View(DFtable6)
View(DFtable7)

