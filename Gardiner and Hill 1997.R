library(haven)
gb91ind <- read_dta("~/Desktop/PhD work/UKDA-7210-stata9/stata9/gb91ind.dta")

#put in dataset
gardiner1997<- #dataset

#code for bicyle
#subset the dataset to respeondents that have answered on their work transportation


gardiner1997 <- subset(gardiner1997, gardiner1997$TRANWORK=='1'|gardiner1997$TRANWORK=='2'|
                     gardiner1997$TRANWORK=='3'|gardiner1997$TRANWORK=='4'|
                     gardiner1997$TRANWORK=='5'|gardiner1997$TRANWORK=='6'|
                     gardiner1997$TRANWORK=='7'|gardiner1997$TRANWORK=='8'|
                     gardiner1997$TRANWORK=='9'|gardiner1997$TRANWORK=='0')

#subset to the areas used in the gardner and hill paper
gardiner <- subset(gardiner1997, gardiner1997$AREAP=='51'| gardiner1997$AREAP=='53'| 
                     gardiner1997$AREAP=='64'| gardiner1997$AREAP=='70'|
                     gardiner1997$AREAP=='83'| gardiner1997$AREAP=='85'|
                     gardiner1997$AREAP=='181'| gardiner1997$AREAP=='182'|
                     gardiner1997$AREAP=='184'| gardiner1997$AREAP=='201'|
                     gardiner1997$AREAP=='207'  )

#code a variable to whether or not respondents cycle to work                     
gardiner$cycle <- gardiner$TRANWORK

gardiner$cycle <- ifelse(gardiner$TRANWORK== '7', 'cycle', gardiner$cycle)
gardiner$cycle <- ifelse(gardiner$TRANWORK== '-8', 'NaN', gardiner$cycle)
gardiner$cycle <- ifelse(gardiner$TRANWORK== '0', 'NaN', gardiner$cycle)
gardiner$cycle <- ifelse(gardiner$cycle!= 'cycle', 'no', gardiner$cycle)

table(gardiner$AREAP, gardiner$cycle)

#table 1 cycling by area


table1 <-table(gardiner$AREAP, gardiner$cycle)
table1
proptab1<-prop.table(table1, 1)*100

table(gardiner$AREAP)

#table 2 gender subsets

males<- subset(gardiner, gardiner$SEX=='1')
females <- subset(gardiner, gardiner$SEX=='2')


table2a <-table(males$AREAP, males$cycle)
table2a
prop.tab2a<-prop.table(table2a, 1)*100

table(males$AREAP)

table2b <- table(females$AREAP, females$cycle)
table2b
prop.tab2b<-prop.table(table2b, 1)*100

table(females$AREAP)

#table 3 race subsets

white<- subset(gardiner, gardiner$ETHGROUP== '1')

table3a<- table(white$AREAP, white$cycle)
table3a
prop.tab3a<-prop.table(table3a, 1)*100

table(white$AREAP)

ethnic <- subset(gardiner, gardiner$ETHGROUP!='1')

table3b <- table(ethnic$AREAP, ethnic$cycle)
table3b
prop.tab3b<-prop.table(table3b, 1)*100

table(ethnic$AREAP)

#table 4 cycling by race

leicester <- subset(gardiner, gardiner$AREAP=='184')

leicester$race <- leicester$ETHGROUP

leicester$race <- ifelse(leicester$ETHGROUP== '1', 'white', leicester$race)

leicester$race <- ifelse(leicester$ETHGROUP== '2', 'black', leicester$race)
leicester$race <- ifelse(leicester$ETHGROUP== '3', 'black', leicester$race)
leicester$race <- ifelse(leicester$ETHGROUP== '4', 'black', leicester$race)

leicester$race <- ifelse(leicester$ETHGROUP== '5', 'asian', leicester$race)
leicester$race <- ifelse(leicester$ETHGROUP== '6', 'asian', leicester$race)
leicester$race <- ifelse(leicester$ETHGROUP== '7', 'asian', leicester$race)

leicester$race <- ifelse(leicester$ETHGROUP== '8', 'asian', leicester$race)
leicester$race <- ifelse(leicester$ETHGROUP== '9', 'asian', leicester$race)

leicester$race <- ifelse(leicester$ETHGROUP== '10', 'other', leicester$race)

table4<-table(leicester$race, leicester$cycle)
table4
prop.tab4<-prop.table(table4, 1)*100

table(leicester$race)

#table 5, recode qual level, subset cyclists

cyclists <- subset(gardiner, gardiner$TRANWORK=='7')

cyclists$qual_level<- cyclists$QUALEVEL

cyclists$qual_level<- ifelse(cyclists$QUALEVEL== 'NaN', 'no qual', cyclists$qual_level)
cyclists$qual_level<- ifelse(cyclists$QUALEVEL== '1', 'level a or b', cyclists$qual_level)
cyclists$qual_level<- ifelse(cyclists$QUALEVEL== '2', 'level a or b', cyclists$qual_level)
cyclists$qual_level<- ifelse(cyclists$QUALEVEL== '3', 'level c', cyclists$qual_level)

table5<-table(cyclists$qual_level, cyclists$AREAP)
table5

#not sure what percent they are taking

#table 6, recode socclass

cyclists$soc_class <- cyclists$SOCLASS

cyclists$soc_class<- ifelse(cyclists$SOCLASS== '1', 'Prof, mang, tech', cyclists$soc_class)
cyclists$soc_class<- ifelse(cyclists$SOCLASS== '2', 'Prof, mang, tech', cyclists$soc_class)

cyclists$soc_class<- ifelse(cyclists$SOCLASS== '3', 'skilled', cyclists$soc_class)
cyclists$soc_class<- ifelse(cyclists$SOCLASS== '4', 'skilled', cyclists$soc_class)

cyclists$soc_class<- ifelse(cyclists$SOCLASS== '5', 'part-skilled, unskilled', cyclists$soc_class)
cyclists$soc_class<- ifelse(cyclists$SOCLASS== '6', 'part-skilled, unskilled', cyclists$soc_class)

cyclists$soc_class<- ifelse(cyclists$SOCLASS== '7', 'other', cyclists$soc_class)
cyclists$soc_class<- ifelse(cyclists$SOCLASS== '8', 'other', cyclists$soc_class)
cyclists$soc_class<- ifelse(cyclists$SOCLASS== '9', 'other', cyclists$soc_class)

table6<-table(cyclists$soc_class, cyclists$AREAP)
table6

#unsure percentages
prop.tab6<-prop.table(table6, 1)*100
prop.tab6
