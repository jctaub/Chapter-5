# write script synthpop

green <- #dataset
 
#create variable  of industry growth
green$indgrowth <- green$INDUSDIV

green$indgrowth <- ifelse(green$INDUSDIV== '-8', 'NaN', green$indgrowth)
green$indgrowth <- ifelse(green$INDUSDIV== '10', 'NaN', green$indgrowth)
green$indgrowth <- ifelse(green$INDUSDIV== '0', '3slow_dec', green$indgrowth)
green$indgrowth <- ifelse(green$INDUSDIV== '1', '1fast_dec', green$indgrowth)
green$indgrowth <- ifelse(green$INDUSDIV== '2', '1fast_dec', green$indgrowth) #extraction ores==mining?
green$indgrowth <- ifelse(green$INDUSDIV== '3', '2med_dec', green$indgrowth) #Metal gd/eng/veh might mean engineering
green$indgrowth <- ifelse(green$INDUSDIV== '4', '4slow_grow', green$indgrowth)
green$indgrowth <- ifelse(green$INDUSDIV== '5', '4slow_grow', green$indgrowth)
green$indgrowth <- ifelse(green$INDUSDIV== '6', '5med_grow', green$indgrowth)
green$indgrowth <- ifelse(green$INDUSDIV== '7', '3slow_dec', green$indgrowth)
green$indgrowth <- ifelse(green$INDUSDIV== '8', '5med_grow', green$indgrowth)
green$indgrowth <- ifelse(green$INDUSDIV== '9', '6fast_grow', green$indgrowth)

green$occgrowth <- green$OCCSUBMJ

green$occgrowth <- ifelse(green$OCCSUBMJ=='-8', 'NaN', green$occgrowth)
green$occgrowth <- ifelse(green$OCCSUBMJ=='1', '4med_growth', green$occgrowth)
green$occgrowth <- ifelse(green$OCCSUBMJ=='2', '4med_growth', green$occgrowth)
green$occgrowth <- ifelse(green$OCCSUBMJ=='3', '4med_growth', green$occgrowth)
green$occgrowth <- ifelse(green$OCCSUBMJ=='4', '4med_growth', green$occgrowth)
green$occgrowth <- ifelse(green$OCCSUBMJ=='5', '4med_growth', green$occgrowth)
green$occgrowth <- ifelse(green$OCCSUBMJ=='6', '5fast_growth', green$occgrowth)
green$occgrowth <- ifelse(green$OCCSUBMJ=='7', '4med_growth', green$occgrowth)
green$occgrowth <- ifelse(green$OCCSUBMJ=='8', '3slow_growth', green$occgrowth)
green$occgrowth <- ifelse(green$OCCSUBMJ=='9', '5fast_growth', green$occgrowth)
green$occgrowth <- ifelse(green$OCCSUBMJ=='10', '2slow_dec', green$occgrowth)
green$occgrowth <- ifelse(green$OCCSUBMJ=='11', '2slow_dec', green$occgrowth)
green$occgrowth <- ifelse(green$OCCSUBMJ=='12', '4med_growth', green$occgrowth)
green$occgrowth <- ifelse(green$OCCSUBMJ=='13', '1med_dec', green$occgrowth)
green$occgrowth <- ifelse(green$OCCSUBMJ=='14', '2slow_dec', green$occgrowth)
green$occgrowth <- ifelse(green$OCCSUBMJ=='15', '4med_growth', green$occgrowth)
green$occgrowth <- ifelse(green$OCCSUBMJ=='16', '5fast_growth', green$occgrowth)
green$occgrowth <- ifelse(green$OCCSUBMJ=='17', '2slow_dec', green$occgrowth)
green$occgrowth <- ifelse(green$OCCSUBMJ=='18', '3slow_growth', green$occgrowth)
green$occgrowth <- ifelse(green$OCCSUBMJ=='19', '2slow_dec', green$occgrowth)
green$occgrowth <- ifelse(green$OCCSUBMJ=='20', '2slow_dec', green$occgrowth)
green$occgrowth <- ifelse(green$OCCSUBMJ=='21', '1med_dec', green$occgrowth)
green$occgrowth <- ifelse(green$OCCSUBMJ=='22', '1med_dec', green$occgrowth)

#create hours worked categorical variable
green$hour_worked <- green$HOURS

green$hour_worked <- ifelse(green$HOURS== '-8', 'NaN', green$hour_worked)

green$hour_worked <- ifelse(green$HOURS== '1', '0-15', green$hour_worked)
green$hour_worked <- ifelse(green$HOURS== '2', '0-15', green$hour_worked)
green$hour_worked <- ifelse(green$HOURS== '3', '0-15', green$hour_worked)
green$hour_worked <- ifelse(green$HOURS== '4', '0-15', green$hour_worked)
green$hour_worked <- ifelse(green$HOURS== '5', '0-15', green$hour_worked)
green$hour_worked <- ifelse(green$HOURS== '6', '0-15', green$hour_worked)
green$hour_worked <- ifelse(green$HOURS== '7', '0-15', green$hour_worked)
green$hour_worked <- ifelse(green$HOURS== '8', '0-15', green$hour_worked)
green$hour_worked <- ifelse(green$HOURS== '9', '0-15', green$hour_worked)
green$hour_worked <- ifelse(green$HOURS== '10', '0-15', green$hour_worked)
green$hour_worked <- ifelse(green$HOURS== '11', '0-15', green$hour_worked)
green$hour_worked <- ifelse(green$HOURS== '12', '0-15', green$hour_worked)
green$hour_worked <- ifelse(green$HOURS== '13', '0-15', green$hour_worked)
green$hour_worked <- ifelse(green$HOURS== '14', '0-15', green$hour_worked)
green$hour_worked <- ifelse(green$HOURS== '15', '0-15', green$hour_worked)

green$hour_worked <- ifelse(green$HOURS== '16', '16-30', green$hour_worked)
green$hour_worked <- ifelse(green$HOURS== '17', '16-30', green$hour_worked)
green$hour_worked <- ifelse(green$HOURS== '18', '16-30', green$hour_worked)
green$hour_worked <- ifelse(green$HOURS== '19', '16-30', green$hour_worked)
green$hour_worked <- ifelse(green$HOURS== '20', '16-30', green$hour_worked)
green$hour_worked <- ifelse(green$HOURS== '21', '16-30', green$hour_worked)
green$hour_worked <- ifelse(green$HOURS== '22', '16-30', green$hour_worked)
green$hour_worked <- ifelse(green$HOURS== '23', '16-30', green$hour_worked)
green$hour_worked <- ifelse(green$HOURS== '24', '16-30', green$hour_worked)
green$hour_worked <- ifelse(green$HOURS== '25', '16-30', green$hour_worked)
green$hour_worked <- ifelse(green$HOURS== '26', '16-30', green$hour_worked)
green$hour_worked <- ifelse(green$HOURS== '27', '16-30', green$hour_worked)
green$hour_worked <- ifelse(green$HOURS== '28', '16-30', green$hour_worked)
green$hour_worked <- ifelse(green$HOURS== '29', '16-30', green$hour_worked)
green$hour_worked <- ifelse(green$HOURS== '30', '16-30', green$hour_worked)

green$hour_worked <- ifelse(green$HOURS== '31', '31-40', green$hour_worked)
green$hour_worked <- ifelse(green$HOURS== '32', '31-40', green$hour_worked)
green$hour_worked <- ifelse(green$HOURS== '33', '31-40', green$hour_worked)
green$hour_worked <- ifelse(green$HOURS== '34', '31-40', green$hour_worked)
green$hour_worked <- ifelse(green$HOURS== '35', '31-40', green$hour_worked)
green$hour_worked <- ifelse(green$HOURS== '36', '31-40', green$hour_worked)
green$hour_worked <- ifelse(green$HOURS== '37', '31-40', green$hour_worked)
green$hour_worked <- ifelse(green$HOURS== '38', '31-40', green$hour_worked)
green$hour_worked <- ifelse(green$HOURS== '39', '31-40', green$hour_worked)
green$hour_worked <- ifelse(green$HOURS== '40', '31-40', green$hour_worked)

green$hour_worked <- ifelse(green$HOURS== '41', '41-60', green$hour_worked)
green$hour_worked <- ifelse(green$HOURS== '42', '41-60', green$hour_worked)
green$hour_worked <- ifelse(green$HOURS== '43', '41-60', green$hour_worked)
green$hour_worked <- ifelse(green$HOURS== '44', '41-60', green$hour_worked)
green$hour_worked <- ifelse(green$HOURS== '45', '41-60', green$hour_worked)
green$hour_worked <- ifelse(green$HOURS== '46', '41-60', green$hour_worked)
green$hour_worked <- ifelse(green$HOURS== '47', '41-60', green$hour_worked)
green$hour_worked <- ifelse(green$HOURS== '48', '41-60', green$hour_worked)
green$hour_worked <- ifelse(green$HOURS== '49', '41-60', green$hour_worked)
green$hour_worked <- ifelse(green$HOURS== '50', '41-60', green$hour_worked)
green$hour_worked <- ifelse(green$HOURS== '51', '41-60', green$hour_worked)
green$hour_worked <- ifelse(green$HOURS== '52', '41-60', green$hour_worked)
green$hour_worked <- ifelse(green$HOURS== '53', '41-60', green$hour_worked)
green$hour_worked <- ifelse(green$HOURS== '54', '41-60', green$hour_worked)
green$hour_worked <- ifelse(green$HOURS== '55', '41-60', green$hour_worked)
green$hour_worked <- ifelse(green$HOURS== '56', '41-60', green$hour_worked)
green$hour_worked <- ifelse(green$HOURS== '57', '41-60', green$hour_worked)
green$hour_worked <- ifelse(green$HOURS== '58', '41-60', green$hour_worked)
green$hour_worked <- ifelse(green$HOURS== '59', '41-60', green$hour_worked)
green$hour_worked <- ifelse(green$HOURS== '60', '41-60', green$hour_worked)

green$hour_worked <- ifelse(green$HOURS== '61', '61+', green$hour_worked)
green$hour_worked <- ifelse(green$HOURS== '62', '61+', green$hour_worked)
green$hour_worked <- ifelse(green$HOURS== '63', '61+', green$hour_worked)
green$hour_worked <- ifelse(green$HOURS== '64', '61+', green$hour_worked)
green$hour_worked <- ifelse(green$HOURS== '65', '61+', green$hour_worked)
green$hour_worked <- ifelse(green$HOURS== '66', '61+', green$hour_worked)
green$hour_worked <- ifelse(green$HOURS== '67', '61+', green$hour_worked)
green$hour_worked <- ifelse(green$HOURS== '68', '61+', green$hour_worked)
green$hour_worked <- ifelse(green$HOURS== '69', '61+', green$hour_worked)
green$hour_worked <- ifelse(green$HOURS== '70', '61+', green$hour_worked)
green$hour_worked <- ifelse(green$HOURS== '71', '61+', green$hour_worked)
green$hour_worked <- ifelse(green$HOURS== '81', '61+', green$hour_worked)



#subset to men

men<- subset(green, green$SEX=='1')

counts<- table(men$indgrowth, men$ETHGROUP)


prop.counts <- as.data.frame(prop.table(counts, 2)*100)

prop.table(counts, 2)*100

#barplot(prop.counts, xlab = 'race')

greenkeymen<- as.data.frame(table(men$ETHGROUP))

#figure 4.2

women<- subset(green, green$SEX=='2')

counts2<- table(women$indgrowth, women$ETHGROUP)

prop.counts2 <-as.data.frame(prop.table(counts2, 2)*100)

#barplot(prop.counts2, xlab = 'race')

greenkeywomen<- as.data.frame(table(women$ETHGROUP))

#figure 4.3 

counts3 <- table(men$occgrowth, men$ETHGROUP)

prop.counts3<- as.data.frame(prop.table(counts3, 2)*100)

#barplot(prop.counts3)




#figure 4.4

counts4 <- table(women$occgrowth, women$ETHGROUP)

prop.counts4 <- as.data.frame(prop.table(counts4, 2)*100)

#barplot(prop.counts4)


#figure 4.5

counts5 <- table(men$hour_worked, men$ETHGROUP)

prop.counts5 <- as.data.frame(prop.table(counts5, 2)*100)

#barplot(prop.counts5)

#figure 4.6

counts6 <- table(women$hour_worked, women$ETHGROUP)

prop.counts6 <- as.data.frame(prop.table(counts6, 2)*100)

#barplot(prop.counts6)

