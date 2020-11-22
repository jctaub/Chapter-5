

new_champ <- #dataset

  #subset to those moving within britain
within_brit <- subset(new_champ, new_champ$DISTMOVE=='0'|new_champ$DISTMOVE=='5'|
                        new_champ$DISTMOVE=='10'|new_champ$DISTMOVE=='15'|
                        new_champ$DISTMOVE=='20'|new_champ$DISTMOVE=='30'|
                        new_champ$DISTMOVE=='40'|new_champ$DISTMOVE=='50'|
                        new_champ$DISTMOVE=='60'|new_champ$DISTMOVE=='80'|
                        new_champ$DISTMOVE=='100'|new_champ$DISTMOVE=='150'|
                        new_champ$DISTMOVE=='200')

within_brit<- subset(new_champ, new_champ$DISTMOVE!='NaN')
within_brit<- subset(within_brit, within_brit$DISTMOVE!='-8')
within_brit<- subset(within_brit, within_brit$DISTMOVE!='-1')

#create categorical moving variable
within_brit$dist_move_cat <- within_brit$DISTMOVE

within_brit$dist_move_cat <- ifelse(within_brit$DISTMOVE=='0', '0-4', within_brit$dist_move_cat)
within_brit$dist_move_cat <- ifelse(within_brit$DISTMOVE=='5', '05-9', within_brit$dist_move_cat)
within_brit$dist_move_cat <- ifelse(within_brit$DISTMOVE=='10', '10-49', within_brit$dist_move_cat)
within_brit$dist_move_cat <- ifelse(within_brit$DISTMOVE=='15', '10-49', within_brit$dist_move_cat)
within_brit$dist_move_cat <- ifelse(within_brit$DISTMOVE=='20', '10-49', within_brit$dist_move_cat)
within_brit$dist_move_cat <- ifelse(within_brit$DISTMOVE=='30', '10-49', within_brit$dist_move_cat)
within_brit$dist_move_cat <- ifelse(within_brit$DISTMOVE=='40', '10-49', within_brit$dist_move_cat)
within_brit$dist_move_cat <- ifelse(within_brit$DISTMOVE=='50', '50-199', within_brit$dist_move_cat)
within_brit$dist_move_cat <- ifelse(within_brit$DISTMOVE=='60', '50-199', within_brit$dist_move_cat)
within_brit$dist_move_cat <- ifelse(within_brit$DISTMOVE=='80', '50-199', within_brit$dist_move_cat)
within_brit$dist_move_cat <- ifelse(within_brit$DISTMOVE=='100', '50-199', within_brit$dist_move_cat)
within_brit$dist_move_cat <- ifelse(within_brit$DISTMOVE=='150', '50-199', within_brit$dist_move_cat)
within_brit$dist_move_cat <- ifelse(within_brit$DISTMOVE=='200', '7_200+', within_brit$dist_move_cat)

table4.7 <- table(within_brit$dist_move_cat, within_brit$ETHGROUP)


table4.7<-prop.table(table4.7, 2)*100

table4.7

table4.7df<- as.data.frame(table4.7)

key<-table(within_brit$ETHGROUP)

key<- as.data.frame(key)

write_excel_csv(key, '~/Downloads/key_champ_single7.csv')

