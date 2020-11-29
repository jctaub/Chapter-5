#I've created a number of functions that are designed to have the tables 
#and models from the other scripts put in so that they can have the 
#variance/standard error calculated and to combine models for the MI


#######################
#Frequency models

#These models are for calculating the frequency table variance/standard error

#calculting variance
var.freq<-function(data1, data2, Var1, Var2, Freq){
  data1<-frac(data1, "Freq")
  data1$varmerge<-data1$Var2
  data1$Var2<- NULL
  data2$varmerge<-data2$Var1
  data2$Var1<- NULL
  data1<- merge(data1, data2, by= "varmerge")
  data1$std.error<- 1- data1$Freq.x
  data1$std.error<- data1$std.error* data1$Freq.x
  data1$std.error<- data1$std.error/data1$Freq.y
  return(data1)
}

#changing the name of some of the variables to function better in future variables
name.change<-function(data1, data2, Var1, Var2, Freq){
  data1<-var.freq(data1, data2, Var1, Var2, Freq)
  data1$term<- data1$Var1
  data1$estimate<- data1$Freq.x
  vars<- c('varmerge', 'term', 'estimate', 'std.error')
  data1<- data1[, vars]
  return(data1)
}

#this function calculates the standard error for the original dataset, since 
# frequency tables don't have variance/standard error in the R table function 
orig.se<-function(data1, data2, Var1, Var2, Freq){
  test<- name.change(data1, data2, Var1, Var2, Freq)
  test$std.error<- sqrt(test$std.error)
  return(test)
}


###############
#calculating variance/standard error for frequency tables in synthetic data
var.raab.freq.single<- function(data1, data2, Var1, Var2, Freq){
  test<- name.change(data1, data2, Var1, Var2, Freq)
  test$var<- 2*test$std.error
  vars<- c('varmerge','term','estimate', 'var')
  test<- test[, vars]
  return(test)
}


se.raab.freq.single<- function(data1, data2, Var1, Var2, Freq){
  test<-var.raab.freq.single(data1, data2, Var1, Var2, Freq)
  test$se<-sqrt(test$var)
  vars<- c('varmerge','term', 'estimate', 'se')
  test<- test[, vars]
  print(test)
}





#########
# Calculating the variance/standard error  for singly imputed regression models
varaiance2<-function(dataset, std.error){
  dataset$std.error<- dataset$std.error^2
  return(dataset)
}


var.raab.single<- function(dataset,term,  estimate,  std.error){
  test<- varaiance2(dataset, std.error)
  test$var<- 2*test$std.error
  vars<- c('term','estimate', 'var')
  test<- test[, vars]
  return(test)
}


se.raab.single<- function(dataset, term, estimate, std.error){
  test<-var.raab.single(dataset, term, estimate, std.error)
  test$se<-sqrt(test$var)
  vars<- c('term', 'estimate', 'se')
  test<- test[, vars]
  print(test)
}



#calculating variance for multiply imputed regression models

combine.mi3<-function(data1, data2, data3, data4, data5, data6, data7, data8, data9, data10, 
                      varmerge,term, estimate, std.error){
  vars<- c('varmerge','term', 'estimate', 'std.error')
  data1<-data1[, vars]
  data2<-data2[, vars]
  data3<-data3[, vars]
  data4<-data4[, vars]
  data5<-data5[, vars]
  data6<-data6[, vars]
  data7<-data7[, vars]
  data8<-data8[, vars]
  data9<-data9[, vars]
  data10<-data10[, vars]
  data1$estimate1<- data1$estimate
  data2$estimate2<- data2$estimate
  data1$estimate<-NULL
  data2$estimate<-NULL
  data1$std.error1<-data1$std.error
  data2$std.error2<-data2$std.error
  data1$std.error<-NULL
  data2$std.error<-NULL
  merge1<- merge(data1, data2, by= c('varmerge','term'))
  data3$estimate3<-data3$estimate
  data3$estimate<-NULL
  data3$std.error3<-data3$std.error
  data3$std.error<-NULL
  merge2<-merge(merge1, data3, by= c('varmerge','term'))
  data4$estimate4<-data4$estimate
  data4$estimate<-NULL
  data4$std.error4<-data4$std.error
  data4$std.error<-NULL
  merge3<-merge(merge2, data4, by=c('varmerge' ,'term'))
  data5$estimate5<-data5$estimate
  data5$estimate<-NULL
  data5$std.error5<-data5$std.error
  data5$std.error<-NULL
  merge4<-merge(merge3, data5, by= c('varmerge','term'))
  data6$estimate6<-data6$estimate
  data6$estimate<-NULL
  data6$std.error6<-data6$std.error
  data6$std.error<-NULL
  merge5<-merge(merge4, data6, by=c('varmerge', 'term'))
  data7$estimate7<-data7$estimate
  data7$estimate<-NULL
  data7$std.error7<-data7$std.error
  data7$std.error<-NULL
  merge6<-merge(merge5, data7, by=c('varmerge','term'))
  data8$estimate8<-data8$estimate
  data8$estimate<-NULL
  data8$std.error8<-data8$std.error
  data8$std.error<-NULL
  merge7<-merge(merge6, data8, by=c('varmerge', 'term'))
  data9$estimate9<-data9$estimate
  data9$estimate<-NULL
  data9$std.error9<-data9$std.error
  data9$std.error<-NULL
  merge8<-merge(merge7, data9, by=c( 'varmerge','term'))
  data10$estimate10<-data10$estimate
  data10$estimate<-NULL
  data10$std.error10<-data10$std.error
  data10$std.error<-NULL
  merge9<-merge(merge8, data10, by=c( 'varmerge','term'))
  merge9$combined<- merge9$estimate1 + merge9$estimate2 + merge9$estimate3 +merge9$estimate4 +
    merge9$estimate5+ merge9$estimate6+ merge9$estimate7+ merge9$estimate8 + merge9$estimate9+
    merge9$estimate10
  merge9$average<- merge9$combined/10
  return(merge9)
}


combine.4<- function(data1,data1b, data2, data2b, data3, data3b, data4, data4b, data5, data5b, data6, 
                     data6b, data7, data7b, data8, data8b, data9, data9b, data10, data10b,
                     Var1, Var2, Freq){
  data1<-name.change(data1, data1b, Var1, Var2, Freq)
  data2<- name.change(data2, data2b, Var1, Var2, Freq)
  data3<-name.change(data3, data3b, Var1, Var2, Freq)
  data4<-name.change(data4, data4b, Var1, Var2, Freq)
  data5<-name.change(data5, data5b, Var1, Var2, Freq)
  data6<-name.change(data6, data6b, Var1, Var2, Freq)
  data7<-name.change(data7, data7b, Var1, Var2, Freq)
  data8<-name.change(data8, data8b, Var1, Var2, Freq)
  data9<-name.change(data9, data9b, Var1, Var2, Freq)
  data10<-name.change(data10, data10b, Var1, Var2, Freq)
  test<-combine.mi3(data1, data2, data3, data4, data5, data6, data7, data8, data9, data10, 
                    varmerge,term, estimate, std.error)
  return(test)
}

vardata.freq<- function(data1,data1b, data2, data2b, data3, data3b, data4, data4b, data5, data5b, data6, 
                        data6b, data7, data7b, data8, data8b, data9, data9b, data10, data10b,
                        Var1, Var2, Freq){
  test<- combine.4(data1,data1b, data2, data2b, data3, data3b, data4, data4b, data5, data5b, data6, 
                   data6b, data7, data7b, data8, data8b, data9, data9b, data10, data10b,
                   Var1, Var2, Freq)
  test$winvar<- test$std.error1+ test$std.error2+test$std.error3+ test$std.error4+ test$std.error5+
    test$std.error6 + test$std.error7+ test$std.error8+ test$std.error9+ test$std.error10
  test$winvar<- test$winvar/10
  return(test)
}


var.freq.reduce<- function(data1,data1b, data2, data2b, data3, data3b, data4, data4b, data5, data5b, data6, 
                           data6b, data7, data7b, data8, data8b, data9, data9b, data10, data10b,
                           Var1, Var2, Freq){
  test<-vardata.freq(data1,data1b, data2, data2b, data3, data3b, data4, data4b, data5, data5b, data6, 
                     data6b, data7, data7b, data8, data8b, data9, data9b, data10, data10b,
                     Var1, Var2, Freq)
  vars<-c('varmerge', 'term', 'average', 'winvar')
  test<-test[, vars]
  return(test)
}



var.raab.freq.mi<-function(data1,data1b, data2, data2b, data3, data3b, data4, data4b, data5, data5b, data6, 
                           data6b, data7, data7b, data8, data8b, data9, data9b, data10, data10b,
                           Var1, Var2, Freq){
  test<-var.freq.reduce(data1,data1b, data2, data2b, data3, data3b, data4, data4b, data5, data5b, data6, 
                        data6b, data7, data7b, data8, data8b, data9, data9b, data10, data10b,
                        Var1, Var2, Freq)
  test$var<- test$winvar*1.1
  return(test)
}



se.raab.freq.mi<-function(data1,data1b, data2, data2b, data3, data3b, data4, data4b, data5, data5b, data6, 
                          data6b, data7, data7b, data8, data8b, data9, data9b, data10, data10b,
                          Var1, Var2, Freq){
  test<-var.raab.freq.mi(data1,data1b, data2, data2b, data3, data3b, data4, data4b, data5, data5b, data6, 
                         data6b, data7, data7b, data8, data8b, data9, data9b, data10, data10b,
                         Var1, Var2, Freq)
  test$se<- sqrt(test$var)
  vars<-c('varmerge', 'term', 'average', 'se')
  test<-test[, vars]
  return(test)
} 



#calculating the between imputation variance to calculate the MI regression variance/standard error

btwvar.freq<-function(data1,data1b, data2, data2b, data3, data3b, data4, data4b, data5, data5b, data6, 
                      data6b, data7, data7b, data8, data8b, data9, data9b, data10, data10b,
                      Var1, Var2, Freq){
  test<-vardata.freq(data1,data1b, data2, data2b, data3, data3b, data4, data4b, data5, data5b, data6, 
                     data6b, data7, data7b, data8, data8b, data9, data9b, data10, data10b,
                     Var1, Var2, Freq)
  test$dif1<- (test$estimate1- test$average)^2
  test$dif2<- (test$estimate2- test$average)^2
  test$dif3<- (test$estimate3- test$average)^2
  test$dif4<- (test$estimate4- test$average)^2
  test$dif5<- (test$estimate5- test$average)^2
  test$dif6<- (test$estimate6- test$average)^2
  test$dif7<- (test$estimate7- test$average)^2
  test$dif8<- (test$estimate8- test$average)^2
  test$dif9<- (test$estimate9- test$average)^2
  test$dif10<- (test$estimate10- test$average)^2
  test$btwvar<- test$dif1 + test$dif2 + test$dif3+ test$dif4 + test$dif5 + test$dif6+ test$dif7+
    test$dif8 + test$dif9 + test$dif10
  test$btwvar<-test$btwvar/9
  return(test)
}


totvar.freq<-function(data1,data1b, data2, data2b, data3, data3b, data4, data4b, data5, data5b, data6, 
                      data6b, data7, data7b, data8, data8b, data9, data9b, data10, data10b,
                      Var1, Var2, Freq){
  test<-btwvar.freq(data1,data1b, data2, data2b, data3, data3b, data4, data4b, data5, data5b, data6, 
                    data6b, data7, data7b, data8, data8b, data9, data9b, data10, data10b,
                    Var1, Var2, Freq)
  test$totvar<- 1.1*test$btwvar
  test$totvar<- test$totvar- test$winvar
  vars<- c('varmerge','term', 'average', 'winvar', 'btwvar', 'totvar')
  test<- test[, vars]
  return(test)
}


se.freq<- function(data1,data1b, data2, data2b, data3, data3b, data4, data4b, data5, data5b, data6, 
                   data6b, data7, data7b, data8, data8b, data9, data9b, data10, data10b,
                   Var1, Var2, Freq){
  test<-totvar.freq(data1,data1b, data2, data2b, data3, data3b, data4, data4b, data5, data5b, data6, 
                    data6b, data7, data7b, data8, data8b, data9, data9b, data10, data10b,
                    Var1, Var2, Freq)
  test$totvar<- abs(test$totvar)
  test$se<-sqrt(test$totvar)
  vars<- c( 'varmerge','term', 'average', 'se')
  test<- test[, vars]
  print(test)
}



var.reiter<-function(data1, data2, data3, data4, data5, data6, data7, data8, data9, data10, 
                     term, estimate, std.error, number1, number2){
  test<- totvar(data1, data2, data3, data4, data5, data6, data7, data8, data9, data10, 
                term, estimate, std.error)
  test$reitvar<- test$winvar*number1/number2
  return(test)
}


se.reiter<- function(data1, data2, data3, data4, data5, data6, data7, data8, data9, data10, 
                     term, estimate, std.error, number1, number2){
  test<- var.reiter(data1, data2, data3, data4, data5, data6, data7, data8, data9, data10, 
                    term, estimate, std.error, number1, number2)
  test$se<- sqrt(test$reitvar)
  vars<- c('term', 'average', 'se')
  test<- test[, vars]
  return(test)
} 


combine.mi3.reiter<-function(data1, data2, data3, data4, data5, data6, data7, data8, data9, data10, 
                             varmerge,term, estimate, std.error, Freq){
  vars<- c('varmerge','term', 'estimate', 'std.error', "Freq")
  data1<-data1[, vars]
  data2<-data2[, vars]
  data3<-data3[, vars]
  data4<-data4[, vars]
  data5<-data5[, vars]
  data6<-data6[, vars]
  data7<-data7[, vars]
  data8<-data8[, vars]
  data9<-data9[, vars]
  data10<-data10[, vars]
  data1$estimate1<- data1$estimate
  data2$estimate2<- data2$estimate
  data1$estimate<-NULL
  data2$estimate<-NULL
  data1$std.error1<-data1$std.error
  data2$std.error2<-data2$std.error
  data1$std.error<-NULL
  data2$std.error<-NULL
  data1$freq1<-data1$Freq
  data2$freq2<-data2$Freq
  data1$Freq<-NULL
  data2$Freq<-NULL
  merge1<- merge(data1, data2, by= c('varmerge','term'))
  data3$estimate3<-data3$estimate
  data3$estimate<-NULL
  data3$std.error3<-data3$std.error
  data3$std.error<-NULL
  data3$freq3<- data3$Freq
  data3$Freq<-NULL
  merge2<-merge(merge1, data3, by= c('varmerge','term'))
  data4$estimate4<-data4$estimate
  data4$estimate<-NULL
  data4$std.error4<-data4$std.error
  data4$std.error<-NULL
  data4$freq4<-data4$Freq
  data4$Freq<-NULL
  merge3<-merge(merge2, data4, by=c('varmerge' ,'term'))
  data5$estimate5<-data5$estimate
  data5$estimate<-NULL
  data5$std.error5<-data5$std.error
  data5$std.error<-NULL
  data5$freq5<-data5$Freq
  data5$Freq<-NULL
  merge4<-merge(merge3, data5, by= c('varmerge','term'))
  data6$estimate6<-data6$estimate
  data6$estimate<-NULL
  data6$std.error6<-data6$std.error
  data6$std.error<-NULL
  data6$freq6<-data6$Freq
  data6$Freq<-NULL
  merge5<-merge(merge4, data6, by=c('varmerge', 'term'))
  data7$estimate7<-data7$estimate
  data7$estimate<-NULL
  data7$std.error7<-data7$std.error
  data7$std.error<-NULL
  data7$freq7<-data7$Freq
  data7$Freq<-NULL
  merge6<-merge(merge5, data7, by=c('varmerge','term'))
  data8$estimate8<-data8$estimate
  data8$estimate<-NULL
  data8$std.error8<-data8$std.error
  data8$std.error<-NULL
  data8$freq8<-data8$Freq
  data8$Freq<-NULL
  merge7<-merge(merge6, data8, by=c('varmerge', 'term'))
  data9$estimate9<-data9$estimate
  data9$estimate<-NULL
  data9$std.error9<-data9$std.error
  data9$std.error<-NULL
  data9$freq9<-data9$Freq
  data9$Freq<-NULL
  merge8<-merge(merge7, data9, by=c( 'varmerge','term'))
  data10$estimate10<-data10$estimate
  data10$estimate<-NULL
  data10$std.error10<-data10$std.error
  data10$std.error<-NULL
  data10$freq10<-data10$Freq
  data10$Freq<-NULL
  merge9<-merge(merge8, data10, by=c( 'varmerge','term'))
  merge9$combined<- merge9$estimate1 + merge9$estimate2 + merge9$estimate3 +merge9$estimate4 +
    merge9$estimate5+ merge9$estimate6+ merge9$estimate7+ merge9$estimate8 + merge9$estimate9+
    merge9$estimate10
  merge9$average<- merge9$combined/10
  merge9$nsyn<- merge9$freq1+ merge9$freq2+merge9$freq3+ merge9$freq4+ merge9$freq5+ merge9$freq6+
    merge9$freq7+ merge9$freq8+ merge9$freq9+ merge9$freq10
  merge9$nsyn<- merge9$nsyn/10
  return(merge9)
}


combine.4.reiter<- function(data1,data1b, data2, data2b, data3, data3b, data4, data4b, data5, data5b, data6, 
                            data6b, data7, data7b, data8, data8b, data9, data9b, data10, data10b,
                            Var1, Var2, Freq){
  data1<-name.change.reiter(data1, data1b, Var1, Var2, Freq)
  data2<- name.change.reiter(data2, data2b, Var1, Var2, Freq)
  data3<-name.change.reiter(data3, data3b, Var1, Var2, Freq)
  data4<-name.change.reiter(data4, data4b, Var1, Var2, Freq)
  data5<-name.change.reiter(data5, data5b, Var1, Var2, Freq)
  data6<-name.change.reiter(data6, data6b, Var1, Var2, Freq)
  data7<-name.change.reiter(data7, data7b, Var1, Var2, Freq)
  data8<-name.change.reiter(data8, data8b, Var1, Var2, Freq)
  data9<-name.change.reiter(data9, data9b, Var1, Var2, Freq)
  data10<-name.change.reiter(data10, data10b, Var1, Var2, Freq)
  test<-combine.mi3.reiter(data1, data2, data3, data4, data5, data6, data7, data8, data9, data10, 
                           varmerge,term, estimate, std.error)
  return(test)
}


vardata.freq.reiter.1<- function(data1,data1b, data2, data2b, data3, data3b, data4, data4b, data5, data5b, data6, 
                                 data6b, data7, data7b, data8, data8b, data9, data9b, data10, data10b,
                                 Var1, Var2, Freq){
  test<- combine.4.reiter(data1,data1b, data2, data2b, data3, data3b, data4, data4b, data5, data5b, data6, 
                          data6b, data7, data7b, data8, data8b, data9, data9b, data10, data10b,
                          Var1, Var2, Freq)
  test$winvar<- test$std.error1+ test$std.error2+test$std.error3+ test$std.error4+ test$std.error5+
    test$std.error6 + test$std.error7+ test$std.error8+ test$std.error9+ test$std.error10
  test$winvar<- test$winvar/10
  return(test)
}




btwvar.freq.reiter<-function(data1,data1b, data2, data2b, data3, data3b, data4, data4b, data5, data5b, data6, 
                             data6b, data7, data7b, data8, data8b, data9, data9b, data10, data10b,
                             Var1, Var2, Freq){
  test<-vardata.freq.reiter.1(data1,data1b, data2, data2b, data3, data3b, data4, data4b, data5, data5b, data6, 
                              data6b, data7, data7b, data8, data8b, data9, data9b, data10, data10b,
                              Var1, Var2, Freq)
  test$dif1<- (test$estimate1- test$average)^2
  test$dif2<- (test$estimate2- test$average)^2
  test$dif3<- (test$estimate3- test$average)^2
  test$dif4<- (test$estimate4- test$average)^2
  test$dif5<- (test$estimate5- test$average)^2
  test$dif6<- (test$estimate6- test$average)^2
  test$dif7<- (test$estimate7- test$average)^2
  test$dif8<- (test$estimate8- test$average)^2
  test$dif9<- (test$estimate9- test$average)^2
  test$dif10<- (test$estimate10- test$average)^2
  test$btwvar<- test$dif1 + test$dif2 + test$dif3+ test$dif4 + test$dif5 + test$dif6+ test$dif7+
    test$dif8 + test$dif9 + test$dif10
  test$btwvar<-test$btwvar/9
  return(test)
}


totvar.freq.reiter<-function(data1,data1b, data2, data2b, data3, data3b, data4, data4b, data5, data5b, data6, 
                             data6b, data7, data7b, data8, data8b, data9, data9b, data10, data10b,
                             Var1, Var2, Freq){
  test<-btwvar.freq.reiter(data1,data1b, data2, data2b, data3, data3b, data4, data4b, data5, data5b, data6, 
                           data6b, data7, data7b, data8, data8b, data9, data9b, data10, data10b,
                           Var1, Var2, Freq)
  test$totvar<- 1.1*test$btwvar
  test$totvar<- test$totvar- test$winvar
  vars<- c('varmerge','term', 'average', 'winvar', 'nsyn' ,'totvar')
  test<- test[, vars]
  return(test)
}

vardata.freq.reiter.2<-function(data1,data1b, data2, data2b, data3, data3b, data4, data4b, data5, data5b, data6, 
                                data6b, data7, data7b, data8, data8b, data9, data9b, data10, data10b, data_orig,
                                Var1, Var2, Freq){
  test<-totvar.freq.reiter(data1,data1b, data2, data2b, data3, data3b, data4, data4b, data5, data5b, data6, 
                           data6b, data7, data7b, data8, data8b, data9, data9b, data10, data10b,
                           Var1, Var2, Freq) 
  data_orig$varmerge<-data_orig$Var1
  test2<- merge(test, data_orig, by= 'varmerge')
  return(test2)
}


vardata.freq.reiter.3<-function(data1,data1b, data2, data2b, data3, data3b, data4, data4b, data5, data5b, data6, 
                                data6b, data7, data7b, data8, data8b, data9, data9b, data10, data10b, data_orig,
                                Var1, Var2, Freq){
  test<-vardata.freq.reiter.2(data1,data1b, data2, data2b, data3, data3b, data4, data4b, data5, data5b, data6, 
                              data6b, data7, data7b, data8, data8b, data9, data9b, data10, data10b, data_orig,
                              Var1, Var2, Freq)
  test$reitvar<-test$winvar*test$nsyn/test$Freq
  return(test)
}





vardata.freq.reiter.4<-function(data1,data1b, data2, data2b, data3, data3b, data4, data4b, data5, data5b, data6, 
                                data6b, data7, data7b, data8, data8b, data9, data9b, data10, data10b, data_orig,
                                Var1, Var2, Freq){
  test<-vardata.freq.reiter.3(data1,data1b, data2, data2b, data3, data3b, data4, data4b, data5, data5b, data6, 
                              data6b, data7, data7b, data8, data8b, data9, data9b, data10, data10b, data_orig,
                              Var1, Var2, Freq)
  test$totvar<-ifelse(test$totvar<0, 0, test$totvar)
  test$reitvar<-ifelse(test$totvar>0, 0, test$reitvar)
  test$totvar<- test$totvar+test$reitvar
  vars<- c('varmerge', 'term', 'average', 'totvar')
  test<- test[, vars]
  return(test)
}


se.freq.reiter<- function(data1,data1b, data2, data2b, data3, data3b, data4, data4b, data5, data5b, data6, 
                          data6b, data7, data7b, data8, data8b, data9, data9b, data10, data10b,data_orig, 
                          Var1, Var2, Freq){
  test<-vardata.freq.reiter.4(data1,data1b, data2, data2b, data3, data3b, data4, data4b, data5, data5b, data6, 
                              data6b, data7, data7b, data8, data8b, data9, data9b, data10, data10b, data_orig,
                              Var1, Var2, Freq)
  test$totvar<- abs(test$totvar)
  test$se<-sqrt(test$totvar)
  vars<- c( 'varmerge','term', 'average', 'se')
  test<- test[, vars]
  print(test)
}



#for combining together the Levanthal tables, since calculating the variance for the Levanthal
#tables isn't realistic
combine.mi.freqL<-function(data1, data2, data3, data4, data5, data6, data7, data8, data9, data10, 
                           Var1, Var2, Freq){
  vars<- c('Var1','Var2', 'Freq')
  data1<-data1[, vars]
  data2<-data2[, vars]
  data3<-data3[, vars]
  data4<-data4[, vars]
  data5<-data5[, vars]
  data6<-data6[, vars]
  data7<-data7[, vars]
  data8<-data8[, vars]
  data9<-data9[, vars]
  data10<-data10[, vars]
  data1$estimate1<- data1$Freq
  data2$estimate2<- data2$Freq
  data1$Freq<-NULL
  data2$Freq<-NULL
  merge1<- merge(data1, data2, by= c('Var1', 'Var2'))
  data3$estimate3<-data3$Freq
  data3$Freq<-NULL
  merge2<-merge(merge1, data3, by= c('Var1', 'Var2'))
  data4$estimate4<-data4$Freq
  data4$Freq<-NULL
  merge3<-merge(merge2, data4, by= c('Var1', 'Var2'))
  data5$estimate5<-data5$Freq
  data5$Freq<-NULL
  merge4<-merge(merge3, data5, by= c('Var1', 'Var2'))
  data6$estimate6<-data6$Freq
  data6$Freq<-NULL
  merge5<-merge(merge4, data6, by=c('Var1', 'Var2'))
  data7$estimate7<-data7$Freq
  data7$Freq<-NULL
  merge6<-merge(merge5, data7, by=c('Var1', 'Var2'))
  data8$estimate8<-data8$Freq
  data8$Freq<-NULL
  merge7<-merge(merge6, data8, by=c('Var1', 'Var2'))
  data9$estimate9<-data9$Freq
  data9$Freq<-NULL
  merge8<-merge(merge7, data9, by=c('Var1', 'Var2'))
  data10$estimate10<-data10$Freq
  data10$Freq<-NULL
  merge9<-merge(merge8, data10, by=c('Var1', 'Var2'))
  merge9$combined<- merge9$estimate1 + merge9$estimate2 + merge9$estimate3 +merge9$estimate4 +
    merge9$estimate5+ merge9$estimate6+ merge9$estimate7+ merge9$estimate8 + merge9$estimate9+
    merge9$estimate10
  merge9$average<- merge9$combined/10
  vars<- c('Var1', 'Var2', 'average')
  merge9<- merge9[, vars]
  return(merge9)
}


##########################################################
#running combining functions

#original
champ_orig_tot<-orig.se(champion_orig, key_champ_orig, "Var1", "Var2", "Freq")

champ_sing_fix<- orig.se(champion_cart_single, key_champ_single, 'Var1', 'Var2', 'Freq')


green1_orig_tot<- orig.se(green1orig, greenkeymen_orig, 'Var1', 'Var2', 'Freq')
green2_orig_tot<- orig.se(green2orig, greenkeywomen_orig, 'Var1', 'Var2', 'Freq')
green3_orig_tot<- orig.se(green3orig, greenkeymen_orig, 'Var1', 'Var2', 'Freq')
green4_orig_tot<- orig.se(green4orig, greenkeywomen_orig, 'Var1', 'Var2', 'Freq')
green5_orig_tot<- orig.se(green5orig, greenkeymen_orig, 'Var1', 'Var2', 'Freq')
green6_orig_tot<- orig.se(green6orig, greenkeywomen_orig, 'Var1', 'Var2', 'Freq')

green_single1_fix<- orig.se(green1single, greenkeymen_single, "Var1", "Var2", "Freq")
green_single2_fix<- orig.se(green2single, greenkeywomen_single, "Var1", "Var2", "Freq")
green_single3_fix<- orig.se(green3single, greenkeymen_single, "Var1", "Var2", "Freq")
green_single4_fix<- orig.se(green4single, greenkeywomen_single, "Var1", "Var2", "Freq")
green_single5_fix<- orig.se(green5single, greenkeymen_single, "Var1", "Var2", "Freq")
green_single6_fix<- orig.se(green6single, greenkeywomen_single, "Var1", "Var2", "Freq")

gard_orig1_tot<- orig.se(gard1hillorig, gard1keyorig, 'Var1', 'Var2', 'Freq')
gard_orig2a_tot<- orig.se(gard2ahillorig, gard2akeyorig, 'Var1', 'Var2', 'Freq')
gard_orig2b_tot<- orig.se(gard2bhillorig, gard2bkeyorig, 'Var1', 'Var2', 'Freq')
gard_orig3a_tot<- orig.se(gard3ahillorig, gard3akeyorig, 'Var1', 'Var2', 'Freq')
gard_orig3b_tot<- orig.se(gard3bhillorig, gard3bkeyorig, 'Var1', 'Var2', 'Freq')
gard_orig4_tot<- orig.se(gard4hillorig, gard4keyorig, 'Var1', 'Var2', 'Freq')

gard_sing1_fix<- orig.se(gard1hillsing, gard1keysing, 'Var1', 'Var2', 'Freq')
gard_sing2a_fix<- orig.se(gard2ahillsing, gard2akeysing, 'Var1', 'Var2', 'Freq')
gard_sing2b_fix<- orig.se(gard2bhillsing, gard2bkeysing, 'Var1', 'Var2', 'Freq')
gard_sing3a_fix<- orig.se(gard3ahillsing, gard3akeysing, 'Var1', 'Var2', 'Freq')
gard_sing3b_fix<- orig.se(gard3bhillsing, gard3bkeysing, 'Var1', 'Var2', 'Freq')
gard_sing4_fix<- orig.se(gard4hillsing, gard4keysing, 'Var1', 'Var2', 'Freq')

bal_orig1_tot<-orig.se(ballard1orig, ballard1keyorig, 'Var1', 'Var2', 'Freq')
bal_orig2_tot<-orig.se(ballard2orig, ballard2keyorig, 'Var1', 'Var2', 'Freq')
bal_orig3_tot<-orig.se(ballard3orig, ballard3keyorig, 'Var1', 'Var2', 'Freq')
bal_orig4_tot<-orig.se(ballard4orig, ballard4keyorig, 'Var1', 'Var2', 'Freq')

bal_sing1_fix<-orig.se(ballard1single, ballard1keysingle, 'Var1', 'Var2', 'Freq')
bal_sing2_fix<-orig.se(ballard2single, ballard2keysingle, 'Var1', 'Var2', 'Freq')
bal_sing3_fix<-orig.se(ballard3single, ballard3keysingle, 'Var1', 'Var2', 'Freq')
bal_sing4_fix<-orig.se(ballard4single, ballard4keysingle, 'Var1', 'Var2', 'Freq')

eade6.3_sing_fix<- orig.se(eade6_3_singe, keyeade6_3_single, "Var1", "Var2", "Freq")
eade6.4_sing_fix<- orig.se(eade6_4_single, leyeade6_4_single, "Var1", "Var2", "Freq")

#raab single
champ_tot_raab_single<-se.raab.freq.single(champion_cart_single, key_champ_single, "Var1", "Var2", "Freq")

champ_tot_raab_single<-se.raab.freq.single(champion_syn, key_champ_syn, "Var1", "Var2", "Freq") #this one

champ_sing2<- se.raab.freq.single(champion_single2, key_champ_single2, 'Var1', 'Var2', 'Freq')
champ_sing3<- se.raab.freq.single(champion_single3, key_champ_single3, 'Var1', 'Var2', 'Freq')
champ_sing4<- se.raab.freq.single(champion_single4, key_champ_single4, 'Var1', 'Var2', 'Freq')
champ_sing5<- se.raab.freq.single(champion_single5, key_champ_single5, 'Var1', 'Var2', 'Freq')
champ_sing6<- se.raab.freq.single(champion_single6, key_champ_single6, 'Var1', 'Var2', 'Freq')
champ_sing7<- se.raab.freq.single(champion_single7, key_champ_single7, 'Var1', 'Var2', 'Freq')
champ_sing8<- se.raab.freq.single(champion_single8, key_champ_single8, 'Var1', 'Var2', 'Freq')
champ_sing9<- se.raab.freq.single(champion_single9, key_champ_single9, 'Var1', 'Var2', 'Freq')
champ_sing10<- se.raab.freq.single(champion_single10, key_champ_single10, 'Var1', 'Var2', 'Freq')

eade6.3_sing2<- se.raab.freq.single(eade6_3_single2, keyeade6_3_single2, 'Var1', 'Var2', 'Freq')
eade6.3_sing3<- se.raab.freq.single(eade6_3_single3, keyeade6_3_single3, 'Var1', 'Var2', 'Freq')
eade6.3_sing4<- se.raab.freq.single(eade6_3_single4, keyeade6_3_single4, 'Var1', 'Var2', 'Freq')
eade6.3_sing5<- se.raab.freq.single(eade6_3_single5, keyeade6_3_single5, 'Var1', 'Var2', 'Freq')
eade6.3_sing6<- se.raab.freq.single(eade6_3_single6, keyeade6_3_single6, 'Var1', 'Var2', 'Freq')
eade6.3_sing7<- se.raab.freq.single(eade6_3_single7, keyeade6_3_single7, 'Var1', 'Var2', 'Freq')
eade6.3_sing8<- se.raab.freq.single(eade6_3_single8, keyeade6_3_single8, 'Var1', 'Var2', 'Freq')
eade6.3_sing9<- se.raab.freq.single(eade6_3_single9, keyeade6_3_single9, 'Var1', 'Var2', 'Freq')
eade6.3_sing10<- se.raab.freq.single(eade6_3_single10, keyeade6_3_single10, 'Var1', 'Var2', 'Freq')

eade6.4_sing2<- se.raab.freq.single(eade6_4_single2, keyeade6_4_single2, 'Var1', 'Var2', 'Freq')
eade6.4_sing3<- se.raab.freq.single(eade6_4_single3, keyeade6_4_single3, 'Var1', 'Var2', 'Freq')
eade6.4_sing4<- se.raab.freq.single(eade6_4_single4, keyeade6_4_single4, 'Var1', 'Var2', 'Freq')
eade6.4_sing5<- se.raab.freq.single(eade6_4_single5, keyeade6_4_single5, 'Var1', 'Var2', 'Freq')
eade6.4_sing6<- se.raab.freq.single(eade6_4_single6, keyeade6_4_single6, 'Var1', 'Var2', 'Freq')
eade6.4_sing7<- se.raab.freq.single(eade6_4_single7, keyeade6_4_single7, 'Var1', 'Var2', 'Freq')
eade6.4_sing8<- se.raab.freq.single(eade6_4_single8, keyeade6_4_single8, 'Var1', 'Var2', 'Freq')
eade6.4_sing9<- se.raab.freq.single(eade6_4_single9, keyeade6_4_single9, 'Var1', 'Var2', 'Freq')
eade6.4_sing10<- se.raab.freq.single(eade6_4_single10, keyeade6_4_single10, 'Var1', 'Var2', 'Freq')

gard_hill_tot_raab2<- se.raab.single(logreg_gardsingle2, 'estimate', 'std.error')
gard_hill_tot_raab3<- se.raab.single(logreg_gardsingle3, 'estimate', 'std.error')
gard_hill_tot_raab4<- se.raab.single(logreg_gardsingle4, 'estimate', 'std.error')
gard_hill_tot_raab5<- se.raab.single(logreg_gardsingle5, 'estimate', 'std.error')
gard_hill_tot_raab6<- se.raab.single(logreg_gardsingle6, 'estimate', 'std.error')
gard_hill_tot_raab7<- se.raab.single(logreg_gardsingle7, 'estimate', 'std.error')
gard_hill_tot_raab8<- se.raab.single(logreg_gardsingle8, 'estimate', 'std.error')
gard_hill_tot_raab9<- se.raab.single(logreg_gardsingle9, 'estimate', 'std.error')
gard_hill_tot_raab10<- se.raab.single(logreg_gardsingle10, 'estimate', 'std.error')


gould_raab_single2<- se.raab.single(gould_single2, 'estimate', 'std.error')
gould_raab_single3<- se.raab.single(gould_single3, 'estimate', 'std.error')
gould_raab_single4<- se.raab.single(gould_single4, 'estimate', 'std.error')
gould_raab_single5<- se.raab.single(gould_single5, 'estimate', 'std.error')
gould_raab_single6<- se.raab.single(gould_single6, 'estimate', 'std.error')
gould_raab_single7<- se.raab.single(gould_single7, 'estimate', 'std.error')
gould_raab_single8<- se.raab.single(gould_single8, 'estimate', 'std.error')
gould_raab_single9<- se.raab.single(gould_single9, 'estimate', 'std.error')
gould_raab_single10<- se.raab.single(gould_single10, 'estimate', 'std.error')

gould_raab_single10<- se.raab.single(gould_single10, 'estimate', 'std.error')


drink_male_raab_single2<- se.raab.single(probit_male_single2, 'estimate', 'std.error')
drink_male_raab_single3<- se.raab.single(probit_male_single3, 'estimate', 'std.error')
drink_male_raab_single4<- se.raab.single(probit_male_single4, 'estimate', 'std.error')
drink_male_raab_single5<- se.raab.single(probit_male_single5, 'estimate', 'std.error')
drink_male_raab_single6<- se.raab.single(probit_male_single6, 'estimate', 'std.error')
drink_male_raab_single7<- se.raab.single(probit_male_single7, 'estimate', 'std.error')
drink_male_raab_single8<- se.raab.single(probit_male_single8, 'estimate', 'std.error')
drink_male_raab_single9<- se.raab.single(probit_male_single9, 'estimate', 'std.error')
drink_male_raab_single10<- se.raab.single(probit_male_single10, 'estimate', 'std.error')

drink_female_raab_single2<- se.raab.single(probit_female_single2, 'estimate', 'std.error')
drink_female_raab_single3<- se.raab.single(probit_female_single3, 'estimate', 'std.error')
drink_female_raab_single4<- se.raab.single(probit_female_single4, 'estimate', 'std.error')
drink_female_raab_single5<- se.raab.single(probit_female_single5, 'estimate', 'std.error')
drink_female_raab_single6<- se.raab.single(probit_female_single6, 'estimate', 'std.error')
drink_female_raab_single7<- se.raab.single(probit_female_single7, 'estimate', 'std.error')
drink_female_raab_single8<- se.raab.single(probit_female_single8, 'estimate', 'std.error')
drink_female_raab_single9<- se.raab.single(probit_female_single9, 'estimate', 'std.error')
drink_female_raab_single10<- se.raab.single(probit_female_single10, 'estimate', 'std.error')


green_single1_raab<- se.raab.freq.single(green1single, greenkeymen_single, "Var1", "Var2", "Freq")
green_single2_raab<- se.raab.freq.single(green2single, greenkeywomen_single, "Var1", "Var2", "Freq")
green_single3_raab<- se.raab.freq.single(green3single, greenkeymen_single, "Var1", "Var2", "Freq")
green_single4_raab<- se.raab.freq.single(green4single, greenkeywomen_single, "Var1", "Var2", "Freq")
green_single5_raab<- se.raab.freq.single(green5single, greenkeymen_single, "Var1", "Var2", "Freq")
green_single6_raab<- se.raab.freq.single(green6single, greenkeywomen_single, "Var1", "Var2", "Freq")


green_single1_raab2<- se.raab.freq.single(green1single2, greenkeymen_single2, "Var1", "Var2", "Freq")
green_single1_raab3<- se.raab.freq.single(green1single3, greenkeymen_single3, "Var1", "Var2", "Freq")
green_single1_raab4<- se.raab.freq.single(green1single4, greenkeymen_single4, "Var1", "Var2", "Freq")
green_single1_raab5<- se.raab.freq.single(green1single5, greenkeymen_single5, "Var1", "Var2", "Freq")
green_single1_raab6<- se.raab.freq.single(green1single6, greenkeymen_single6, "Var1", "Var2", "Freq")
green_single1_raab7<- se.raab.freq.single(green1single7, greenkeymen_single7, "Var1", "Var2", "Freq")
green_single1_raab8<- se.raab.freq.single(green1single8, greenkeymen_single8, "Var1", "Var2", "Freq")
green_single1_raab9<- se.raab.freq.single(green1single9, greenkeymen_single9, "Var1", "Var2", "Freq")
green_single1_raab10<- se.raab.freq.single(green1single10, greenkeymen_single10, "Var1", "Var2", "Freq")

#update excel
green_single3_raab2<- se.raab.freq.single(green3single2, greenkeymen_single2, "Var1", "Var2", "Freq")
green_single3_raab3<- se.raab.freq.single(green3single3, greenkeymen_single3, "Var1", "Var2", "Freq")
green_single3_raab4<- se.raab.freq.single(green3single4, greenkeymen_single4, "Var1", "Var2", "Freq")
green_single3_raab5<- se.raab.freq.single(green3single5, greenkeymen_single5, "Var1", "Var2", "Freq")
green_single3_raab6<- se.raab.freq.single(green3single6, greenkeymen_single6, "Var1", "Var2", "Freq")
green_single3_raab7<- se.raab.freq.single(green3single7, greenkeymen_single7, "Var1", "Var2", "Freq")
green_single3_raab8<- se.raab.freq.single(green3single8, greenkeymen_single8, "Var1", "Var2", "Freq")
green_single3_raab9<- se.raab.freq.single(green3single9, greenkeymen_single9, "Var1", "Var2", "Freq")
green_single3_raab10<- se.raab.freq.single(green3single10, greenkeymen_single10, "Var1", "Var2", "Freq")

green_single5_raab2<- se.raab.freq.single(green5single2, greenkeymen_single2, "Var1", "Var2", "Freq")
green_single5_raab3<- se.raab.freq.single(green5single3, greenkeymen_single3, "Var1", "Var2", "Freq")
green_single5_raab4<- se.raab.freq.single(green5single4, greenkeymen_single4, "Var1", "Var2", "Freq")
green_single5_raab5<- se.raab.freq.single(green5single5, greenkeymen_single5, "Var1", "Var2", "Freq")
green_single5_raab6<- se.raab.freq.single(green5single6, greenkeymen_single6, "Var1", "Var2", "Freq")
green_single5_raab7<- se.raab.freq.single(green5single7, greenkeymen_single7, "Var1", "Var2", "Freq")
green_single5_raab8<- se.raab.freq.single(green5single8, greenkeymen_single8, "Var1", "Var2", "Freq")
green_single5_raab9<- se.raab.freq.single(green5single9, greenkeymen_single9, "Var1", "Var2", "Freq")
green_single5_raab10<- se.raab.freq.single(green5single10, greenkeymen_single10, "Var1", "Var2", "Freq")

green_single2_raab2<- se.raab.freq.single(green2single2, greenkeywomen_single2, "Var1", "Var2", "Freq")
green_single2_raab3<- se.raab.freq.single(green2single3, greenkeywomen_single3, "Var1", "Var2", "Freq")
green_single2_raab4<- se.raab.freq.single(green2single4, greenkeywomen_single4, "Var1", "Var2", "Freq")
green_single2_raab5<- se.raab.freq.single(green2single5, greenkeywomen_single5, "Var1", "Var2", "Freq")
green_single2_raab6<- se.raab.freq.single(green2single6, greenkeywomen_single6, "Var1", "Var2", "Freq")
green_single2_raab7<- se.raab.freq.single(green2single7, greenkeywomen_single7, "Var1", "Var2", "Freq")
green_single2_raab8<- se.raab.freq.single(green2single8, greenkeywomen_single8, "Var1", "Var2", "Freq")
green_single2_raab9<- se.raab.freq.single(green2single9, greenkeywomen_single9, "Var1", "Var2", "Freq")
green_single2_raab10<- se.raab.freq.single(green2single10, greenkeywomen_single10, "Var1", "Var2", "Freq")

green_single4_raab2<- se.raab.freq.single(green4single2, greenkeywomen_single2, "Var1", "Var2", "Freq")
green_single4_raab3<- se.raab.freq.single(green4single3, greenkeywomen_single3, "Var1", "Var2", "Freq")
green_single4_raab4<- se.raab.freq.single(green4single4, greenkeywomen_single4, "Var1", "Var2", "Freq")
green_single4_raab5<- se.raab.freq.single(green4single5, greenkeywomen_single5, "Var1", "Var2", "Freq")
green_single4_raab6<- se.raab.freq.single(green4single6, greenkeywomen_single6, "Var1", "Var2", "Freq")
green_single4_raab7<- se.raab.freq.single(green4single7, greenkeywomen_single7, "Var1", "Var2", "Freq")
green_single4_raab8<- se.raab.freq.single(green4single8, greenkeywomen_single8, "Var1", "Var2", "Freq")
green_single4_raab9<- se.raab.freq.single(green4single9, greenkeywomen_single9, "Var1", "Var2", "Freq")
green_single4_raab10<- se.raab.freq.single(green4single10, greenkeywomen_single10, "Var1", "Var2", "Freq")

green_single6_raab2<- se.raab.freq.single(green6single2, greenkeywomen_single2, "Var1", "Var2", "Freq")
green_single6_raab3<- se.raab.freq.single(green6single3, greenkeywomen_single3, "Var1", "Var2", "Freq")
green_single6_raab4<- se.raab.freq.single(green6single4, greenkeywomen_single4, "Var1", "Var2", "Freq")
green_single6_raab5<- se.raab.freq.single(green6single5, greenkeywomen_single5, "Var1", "Var2", "Freq")
green_single6_raab6<- se.raab.freq.single(green6single6, greenkeywomen_single6, "Var1", "Var2", "Freq")
green_single6_raab7<- se.raab.freq.single(green6single7, greenkeywomen_single7, "Var1", "Var2", "Freq")
green_single6_raab8<- se.raab.freq.single(green6single8, greenkeywomen_single8, "Var1", "Var2", "Freq")
green_single6_raab9<- se.raab.freq.single(green6single9, greenkeywomen_single9, "Var1", "Var2", "Freq")
green_single6_raab10<- se.raab.freq.single(green6single10, greenkeywomen_single10, "Var1", "Var2", "Freq")


gard_singraab1<- se.raab.freq.single(gard1hillsing, gard1keysing, 'Var1', 'Var2', 'Freq')
gard_singraab2a<- se.raab.freq.single(gard2ahillsing, gard2akeysing, 'Var1', 'Var2', 'Freq')
gard_singraab2b<- se.raab.freq.single(gard2bhillsing, gard2bkeysing, 'Var1', 'Var2', 'Freq')
gard_singraab3a<- se.raab.freq.single(gard3ahillsing, gard3akeysing, 'Var1', 'Var2', 'Freq')
gard_singraab3b<- se.raab.freq.single(gard3bhillsing, gard3bkeysing, 'Var1', 'Var2', 'Freq')
gard_singraab4<- se.raab.freq.single(gard4hillsing, gard4keysing, 'Var1', 'Var2', 'Freq')



gard1_raab_sing2<- se.raab.freq.single(gard1hill_sing2, gard1key_sing2, 'Var1', 'Var2', 'Freq')
gard1_raab_sing3<- se.raab.freq.single(gard1hill_sing3, gard1key_sing3, 'Var1', 'Var2', 'Freq')
gard1_raab_sing4<- se.raab.freq.single(gard1hill_sing4, gard1key_sing4, 'Var1', 'Var2', 'Freq')
gard1_raab_sing5<- se.raab.freq.single(gard1hill_sing5, gard1key_sing5, 'Var1', 'Var2', 'Freq')
gard1_raab_sing6<- se.raab.freq.single(gard1hill_sing6, gard1key_sing6, 'Var1', 'Var2', 'Freq')
gard1_raab_sing7<- se.raab.freq.single(gard1hill_sing7, gard1key_sing7, 'Var1', 'Var2', 'Freq')
gard1_raab_sing8<- se.raab.freq.single(gard1hill_sing8, gard1key_sing8, 'Var1', 'Var2', 'Freq')
gard1_raab_sing9<- se.raab.freq.single(gard1hill_sing9, gard1key_sing9, 'Var1', 'Var2', 'Freq')
gard1_raab_sing10<- se.raab.freq.single(gard1hill_sing10, gard1key_sing10, 'Var1', 'Var2', 'Freq')

gard2a_raab_sing2<- se.raab.freq.single(gard2ahill_sing2, gard2akey_sing2, 'Var1', 'Var2', 'Freq')
gard2a_raab_sing3<- se.raab.freq.single(gard2ahill_sing3, gard2akey_sing3, 'Var1', 'Var2', 'Freq')
gard2a_raab_sing4<- se.raab.freq.single(gard2ahill_sing4, gard2akey_sing4, 'Var1', 'Var2', 'Freq')
gard2a_raab_sing5<- se.raab.freq.single(gard2ahill_sing5, gard2akey_sing5, 'Var1', 'Var2', 'Freq')
gard2a_raab_sing6<- se.raab.freq.single(gard2ahill_sing6, gard2akey_sing6, 'Var1', 'Var2', 'Freq')
gard2a_raab_sing7<- se.raab.freq.single(gard2ahill_sing7, gard2akey_sing7, 'Var1', 'Var2', 'Freq')
gard2a_raab_sing8<- se.raab.freq.single(gard2ahill_sing8, gard2akey_sing8, 'Var1', 'Var2', 'Freq')
gard2a_raab_sing9<- se.raab.freq.single(gard2ahill_sing9, gard2akey_sing9, 'Var1', 'Var2', 'Freq')
gard2a_raab_sing10<- se.raab.freq.single(gard2ahill_sing10, gard2akey_sing10, 'Var1', 'Var2', 'Freq')

gard2b_raab_sing2<- se.raab.freq.single(gard2bhill_sing2, gard2bkey_sing2, 'Var1', 'Var2', 'Freq')
gard2b_raab_sing3<- se.raab.freq.single(gard2bhill_sing3, gard2bkey_sing3, 'Var1', 'Var2', 'Freq')
gard2b_raab_sing4<- se.raab.freq.single(gard2bhill_sing4, gard2bkey_sing4, 'Var1', 'Var2', 'Freq')
gard2b_raab_sing5<- se.raab.freq.single(gard2bhill_sing5, gard2bkey_sing5, 'Var1', 'Var2', 'Freq')
gard2b_raab_sing6<- se.raab.freq.single(gard2bhill_sing6, gard2bkey_sing6, 'Var1', 'Var2', 'Freq')
gard2b_raab_sing7<- se.raab.freq.single(gard2bhill_sing7, gard2bkey_sing7, 'Var1', 'Var2', 'Freq')
gard2b_raab_sing8<- se.raab.freq.single(gard2bhill_sing8, gard2bkey_sing8, 'Var1', 'Var2', 'Freq')
gard2b_raab_sing9<- se.raab.freq.single(gard2bhill_sing9, gard2bkey_sing9, 'Var1', 'Var2', 'Freq')
gard2b_raab_sing10<- se.raab.freq.single(gard2bhill_sing10, gard2bkey_sing10, 'Var1', 'Var2', 'Freq')

gard3a_raab_sing2<- se.raab.freq.single(gard3ahill_sing2, gard3akey_sing2, 'Var1', 'Var2', 'Freq')
gard3a_raab_sing3<- se.raab.freq.single(gard3ahill_sing3, gard3akey_sing3, 'Var1', 'Var2', 'Freq')
gard3a_raab_sing4<- se.raab.freq.single(gard3ahill_sing4, gard3akey_sing4, 'Var1', 'Var2', 'Freq')
gard3a_raab_sing5<- se.raab.freq.single(gard3ahill_sing5, gard3akey_sing5, 'Var1', 'Var2', 'Freq')
gard3a_raab_sing6<- se.raab.freq.single(gard3ahill_sing6, gard3akey_sing6, 'Var1', 'Var2', 'Freq')
gard3a_raab_sing7<- se.raab.freq.single(gard3ahill_sing7, gard3akey_sing7, 'Var1', 'Var2', 'Freq')
gard3a_raab_sing8<- se.raab.freq.single(gard3ahill_sing8, gard3akey_sing8, 'Var1', 'Var2', 'Freq')
gard3a_raab_sing9<- se.raab.freq.single(gard3ahill_sing9, gard3akey_sing9, 'Var1', 'Var2', 'Freq')
gard3a_raab_sing10<- se.raab.freq.single(gard3ahill_sing10, gard3akey_sing10, 'Var1', 'Var2', 'Freq')

gard3b_raab_sing2<- se.raab.freq.single(gard3bhill_sing2, gard3bkey_sing2, 'Var1', 'Var2', 'Freq')
gard3b_raab_sing3<- se.raab.freq.single(gard3bhill_sing3, gard3bkey_sing3, 'Var1', 'Var2', 'Freq')
gard3b_raab_sing4<- se.raab.freq.single(gard3bhill_sing4, gard3bkey_sing4, 'Var1', 'Var2', 'Freq')
gard3b_raab_sing5<- se.raab.freq.single(gard3bhill_sing5, gard3bkey_sing5, 'Var1', 'Var2', 'Freq')
gard3b_raab_sing6<- se.raab.freq.single(gard3bhill_sing6, gard3bkey_sing6, 'Var1', 'Var2', 'Freq')
gard3b_raab_sing7<- se.raab.freq.single(gard3bhill_sing7, gard3bkey_sing7, 'Var1', 'Var2', 'Freq')
gard3b_raab_sing8<- se.raab.freq.single(gard3bhill_sing8, gard3bkey_sing8, 'Var1', 'Var2', 'Freq')
gard3b_raab_sing9<- se.raab.freq.single(gard3bhill_sing9, gard3bkey_sing9, 'Var1', 'Var2', 'Freq')
gard3b_raab_sing10<- se.raab.freq.single(gard3bhill_sing10, gard3bkey_sing10, 'Var1', 'Var2', 'Freq')

gard4_raab_sing2<- se.raab.freq.single(gard4hill_sing2, gard4key_sing2, 'Var1', 'Var2', 'Freq')
gard4_raab_sing3<- se.raab.freq.single(gard4hill_sing3, gard4key_sing3, 'Var1', 'Var2', 'Freq')
gard4_raab_sing4<- se.raab.freq.single(gard4hill_sing4, gard4key_sing4, 'Var1', 'Var2', 'Freq')
gard4_raab_sing5<- se.raab.freq.single(gard4hill_sing5, gard4key_sing5, 'Var1', 'Var2', 'Freq')
gard4_raab_sing6<- se.raab.freq.single(gard4hill_sing6, gard4key_sing6, 'Var1', 'Var2', 'Freq')
gard4_raab_sing7<- se.raab.freq.single(gard4hill_sing7, gard4key_sing7, 'Var1', 'Var2', 'Freq')
gard4_raab_sing8<- se.raab.freq.single(gard4hill_sing8, gard4key_sing8, 'Var1', 'Var2', 'Freq')
gard4_raab_sing9<- se.raab.freq.single(gard4hill_sing9, gard4key_sing9, 'Var1', 'Var2', 'Freq')
gard4_raab_sing10<- se.raab.freq.single(gard4hill_sing10, gard4key_sing10, 'Var1', 'Var2', 'Freq')


bal_singraab1<-se.raab.freq.single(ballard1single, ballard1keysingle, 'Var1', 'Var2', 'Freq')
bal_singraab2<-se.raab.freq.single(ballard2single, ballard2keysingle, 'Var1', 'Var2', 'Freq')
bal_singraab3<-se.raab.freq.single(ballard3single, ballard3keysingle, 'Var1', 'Var2', 'Freq')
bal_singraab4<-se.raab.freq.single(ballard4single, ballard4keysingle, 'Var1', 'Var2', 'Freq')


bal1_raabsing2<-se.raab.freq.single(ballard1single2, ballard1keysingle2, 'Var1', 'Var2', 'Freq')
bal1_raabsing3<-se.raab.freq.single(ballard1single3, ballard1keysingle3, 'Var1', 'Var2', 'Freq')
bal1_raabsing4<-se.raab.freq.single(ballard1single4, ballard1keysingle4, 'Var1', 'Var2', 'Freq')
bal1_raabsing5<-se.raab.freq.single(ballard1single5, ballard1keysingle5, 'Var1', 'Var2', 'Freq')
bal1_raabsing6<-se.raab.freq.single(ballard1single6, ballard1keysingle6, 'Var1', 'Var2', 'Freq')
bal1_raabsing7<-se.raab.freq.single(ballard1single7, ballard1keysingle7, 'Var1', 'Var2', 'Freq')
bal1_raabsing8<-se.raab.freq.single(ballard1single8, ballard1keysingle8, 'Var1', 'Var2', 'Freq')
bal1_raabsing9<-se.raab.freq.single(ballard1single9, ballard1keysingle9, 'Var1', 'Var2', 'Freq')
bal1_raabsing10<-se.raab.freq.single(ballard1single10, ballard1keysingle10, 'Var1', 'Var2', 'Freq')

bal2_raabsing2<-se.raab.freq.single(ballard2single2, ballard2keysingle2, 'Var1', 'Var2', 'Freq')
bal2_raabsing3<-se.raab.freq.single(ballard2single3, ballard2keysingle3, 'Var1', 'Var2', 'Freq')
bal2_raabsing4<-se.raab.freq.single(ballard2single4, ballard2keysingle4, 'Var1', 'Var2', 'Freq')
bal2_raabsing5<-se.raab.freq.single(ballard2single5, ballard2keysingle5, 'Var1', 'Var2', 'Freq')
bal2_raabsing6<-se.raab.freq.single(ballard2single6, ballard2keysingle6, 'Var1', 'Var2', 'Freq')
bal2_raabsing7<-se.raab.freq.single(ballard2single7, ballard2keysingle7, 'Var1', 'Var2', 'Freq')
bal2_raabsing8<-se.raab.freq.single(ballard2single8, ballard2keysingle8, 'Var1', 'Var2', 'Freq')
bal2_raabsing9<-se.raab.freq.single(ballard2single9, ballard2keysingle9, 'Var1', 'Var2', 'Freq')
bal2_raabsing10<-se.raab.freq.single(ballard2single10, ballard2keysingle10, 'Var1', 'Var2', 'Freq')

bal3_raabsing2<-se.raab.freq.single(ballard3single2, ballard3keysingle2, 'Var1', 'Var2', 'Freq')
bal3_raabsing3<-se.raab.freq.single(ballard3single3, ballard3keysingle3, 'Var1', 'Var2', 'Freq')
bal3_raabsing4<-se.raab.freq.single(ballard3single4, ballard3keysingle4, 'Var1', 'Var2', 'Freq')
bal3_raabsing5<-se.raab.freq.single(ballard3single5, ballard3keysingle5, 'Var1', 'Var2', 'Freq')
bal3_raabsing6<-se.raab.freq.single(ballard3single6, ballard3keysingle6, 'Var1', 'Var2', 'Freq')
bal3_raabsing7<-se.raab.freq.single(ballard3single7, ballard3keysingle7, 'Var1', 'Var2', 'Freq')
bal3_raabsing8<-se.raab.freq.single(ballard3single8, ballard3keysingle8, 'Var1', 'Var2', 'Freq')
bal3_raabsing9<-se.raab.freq.single(ballard3single9, ballard3keysingle9, 'Var1', 'Var2', 'Freq')
bal3_raabsing10<-se.raab.freq.single(ballard3single10, ballard3keysingle10, 'Var1', 'Var2', 'Freq')


bal4_raabsing2<-se.raab.freq.single(ballard4single2, ballard4keysingle2, 'Var1', 'Var2', 'Freq')
bal4_raabsing3<-se.raab.freq.single(ballard4single3, ballard4keysingle3, 'Var1', 'Var2', 'Freq')
bal4_raabsing4<-se.raab.freq.single(ballard4single4, ballard4keysingle4, 'Var1', 'Var2', 'Freq')
bal4_raabsing5<-se.raab.freq.single(ballard4single5, ballard4keysingle5, 'Var1', 'Var2', 'Freq')
bal4_raabsing6<-se.raab.freq.single(ballard4single6, ballard4keysingle6, 'Var1', 'Var2', 'Freq')
bal4_raabsing7<-se.raab.freq.single(ballard4single7, ballard4keysingle7, 'Var1', 'Var2', 'Freq')
bal4_raabsing8<-se.raab.freq.single(ballard4single8, ballard4keysingle8, 'Var1', 'Var2', 'Freq')
bal4_raabsing9<-se.raab.freq.single(ballard4single9, ballard4keysingle9, 'Var1', 'Var2', 'Freq')
bal4_raabsing10<-se.raab.freq.single(ballard4single10, ballard4keysingle10, 'Var1', 'Var2', 'Freq')



#raab mi
champ_tot_raab_mi<-se.raab.freq.mi(champion_cart1, key_champ1, champion_cart2, key_champ2, champion_cart3, key_champ3, 
                                   champion_cart4, key_champ4, champion_cart5, key_champ5, champion_cart6, key_champ6,
                                   champion_cart7, key_champ7, champion_cart8, key_champ8, champion_cart9, key_champ9,
                                   champion_cart10, key_champ10, 'Var1', 'Var2', 'Freq') 

green_raab_1_mi<- se.raab.freq.mi(green1s1, greenkeymen_s1, green1s2, greenkeymen_s2, green1s3, greenkeymen_s3,
                                  green1s4, greenkeymen_s4, green1s5, greenkeymen_s5, green1s6, greenkeymen_s6,
                                  green1s7, greenkeymen_s7, green1s8, greenkeymen_s8, green1s9, greenkeymen_s9,
                                  green1s10, greenkeymen_s10, 'Var1', 'Var2', 'Freq')

green_raab_3_mi<- se.raab.freq.mi(green3s1, greenkeymen_s1, green3s2, greenkeymen_s2, green3s3, greenkeymen_s3,
                                  green3s4, greenkeymen_s4, green3s5, greenkeymen_s5, green3s6, greenkeymen_s6,
                                  green3s7, greenkeymen_s7, green3s8, greenkeymen_s8, green3s9, greenkeymen_s9,
                                  green3s10, greenkeymen_s10, 'Var1', 'Var2', 'Freq')

green_raab_5_mi<- se.raab.freq.mi(green5s1, greenkeymen_s1, green5s2, greenkeymen_s2, green5s3, greenkeymen_s3,
                                  green5s4, greenkeymen_s4, green5s5, greenkeymen_s5, green5s6, greenkeymen_s6,
                                  green5s7, greenkeymen_s7, green5s8, greenkeymen_s8, green5s9, greenkeymen_s9,
                                  green5s10, greenkeymen_s10, 'Var1', 'Var2', 'Freq')

green_raab_2_mi<- se.raab.freq.mi(green2s1, greenkeywomen_s1, green2s2, greenkeywomen_s2, green2s3, greenkeywomen_s3,
                                  green2s4, greenkeywomen_s4, green2s5, greenkeywomen_s5, green2s6, greenkeywomen_s6,
                                  green2s7, greenkeywomen_s7, green2s8, greenkeywomen_s8, green2s9, greenkeywomen_s9,
                                  green2s10, greenkeywomen_s10, 'Var1', 'Var2', 'Freq')

green_raab_4_mi<- se.raab.freq.mi(green4s1, greenkeywomen_s1, green4s2, greenkeywomen_s2, green4s3, greenkeywomen_s3,
                                  green4s4, greenkeywomen_s4, green4s5, greenkeywomen_s5, green4s6, greenkeywomen_s6,
                                  green4s7, greenkeywomen_s7, green4s8, greenkeywomen_s8, green4s9, greenkeywomen_s9,
                                  green4s10, greenkeywomen_s10, 'Var1', 'Var2', 'Freq')

green_raab_6_mi<- se.raab.freq.mi(green6s1, greenkeywomen_s1, green6s2, greenkeywomen_s2, green6s3, greenkeywomen_s3,
                                  green6s4, greenkeywomen_s4, green6s5, greenkeywomen_s5, green6s6, greenkeywomen_s6,
                                  green6s7, greenkeywomen_s7, green6s8, greenkeywomen_s8, green6s9, greenkeywomen_s9,
                                  green6s10, greenkeywomen_s10, 'Var1', 'Var2', 'Freq')


gard_raab_mi_1<- se.raab.freq.mi(gard1hill_1, gard1key_1, gard1hill_2, gard1key_2, gard1hill_3, gard1key_3,
                                 gard1hill_4, gard1key_4, gard1hill_5, gard1key_5, gard1hill_6, gard1key_6,
                                 gard1hill_7, gard1key_7, gard1hill_8, gard1key_8, gard1hill_9, gard1key_9,
                                 gard1hill_10, gard1key_10, 'Var1', 'Var2', 'Freq')

gard_raab_mi_4<- se.raab.freq.mi(gard4hill_1, gard4key_1, gard4hill_2, gard4key_2, gard4hill_3, gard4key_3,
                                 gard4hill_4, gard4key_4, gard4hill_5, gard4key_5, gard4hill_6, gard4key_6,
                                 gard4hill_7, gard4key_7, gard4hill_8, gard4key_8, gard4hill_9, gard4key_9,
                                 gard4hill_10, gard4key_10, 'Var1', 'Var2', 'Freq')

gard_raab_mi_2a<- se.raab.freq.mi(gard2ahill_1, gard2akey_1, gard2ahill_2, gard2akey_2, gard2ahill_3, gard2akey_3,
                                  gard2ahill_4, gard2akey_4, gard2ahill_5, gard2akey_5, gard2ahill_6, gard2akey_6,
                                  gard2ahill_7, gard2akey_7, gard2ahill_8, gard2akey_8, gard2ahill_9, gard2akey_9,
                                  gard2ahill_10, gard2akey_10, 'Var1', 'Var2', 'Freq')

gard_raab_mi_2b<- se.raab.freq.mi(gard2bhill_1, gard2bkey_1, gard2bhill_2, gard2bkey_2, gard2bhill_3, gard2bkey_3,
                                  gard2bhill_4, gard2bkey_4, gard2bhill_5, gard2bkey_5, gard2bhill_6, gard2bkey_6,
                                  gard2bhill_7, gard2bkey_7, gard2bhill_8, gard2bkey_8, gard2bhill_9, gard2bkey_9,
                                  gard2bhill_10, gard2bkey_10, 'Var1', 'Var2', 'Freq')

gard_raab_mi_3a<- se.raab.freq.mi(gard3ahill_1, gard3akey_1, gard3ahill_2, gard3akey_2, gard3ahill_3, gard3akey_3,
                                  gard3ahill_4, gard3akey_4, gard3ahill_5, gard3akey_5, gard3ahill_6, gard3akey_6,
                                  gard3ahill_7, gard3akey_7, gard3ahill_8, gard3akey_8, gard3ahill_9, gard3akey_9,
                                  gard3ahill_10, gard3akey_10, 'Var1', 'Var2', 'Freq')

gard_raab_mi_3b<- se.raab.freq.mi(gard3bhill_1, gard3bkey_1, gard3bhill_2, gard3bkey_2, gard3bhill_3, gard3bkey_3,
                                  gard3bhill_4, gard3bkey_4, gard3bhill_5, gard3bkey_5, gard3bhill_6, gard3bkey_6,
                                  gard3bhill_7, gard3bkey_7, gard3bhill_8, gard3bkey_8, gard3bhill_9, gard3bkey_9,
                                  gard3bhill_10, gard3bkey_10, 'Var1', 'Var2', 'Freq')

bal_raab_mi_1<- se.raab.freq.mi(ballard1_1, ballard1key1, ballard1_2, ballard1key2, ballard1_3, ballard1key3,
                                ballard1_4, ballard1key4, ballard1_5, ballard1key5, ballard1_6, ballard1key6,
                                ballard1_7, ballard1key7, ballard1_8, ballard1key8, ballard1_9, ballard1key9,
                                ballard1_10, ballard1key10, 'Var1', 'Var2', 'Freq')

bal_raab_mi_2<- se.raab.freq.mi(ballard2_1, ballard2key1, ballard2_2, ballard2key2, ballard2_3, ballard2key3,
                                ballard2_4, ballard2key4, ballard2_5, ballard2key5, ballard2_6, ballard2key6,
                                ballard2_7, ballard2key7, ballard2_8, ballard2key8, ballard2_9, ballard2key9,
                                ballard2_10, ballard2key10, 'Var1', 'Var2', 'Freq')

bal_raab_mi_3<- se.raab.freq.mi(ballard3_1, ballard3key1, ballard3_2, ballard3key2, ballard3_3, ballard3key3,
                                ballard3_4, ballard3key4, ballard3_5, ballard3key5, ballard3_6, ballard3key6,
                                ballard3_7, ballard3key7, ballard3_8, ballard3key8, ballard3_9, ballard3key9,
                                ballard3_10, ballard3key10, 'Var1', 'Var2', 'Freq') #needschecking

bal_raab_mi_4<- se.raab.freq.mi(ballard4_1, ballard4key1, ballard4_2, ballard4key2, ballard4_3, ballard4key3,
                                ballard4_4, ballard4key4, ballard4_5, ballard4key5, ballard4_6, ballard4key6,
                                ballard4_7, ballard4key7, ballard4_8, ballard4key8, ballard4_9, ballard4key9,
                                ballard4_10, ballard4key10, 'Var1', 'Var2', 'Freq')

#ragunathan
champ_tot<-se.freq(champion_cart1, key_champ1, champion_cart2, key_champ2, champion_cart3, key_champ3, 
                   champion_cart4, key_champ4, champion_cart5, key_champ5, champion_cart6, key_champ6,
                   champion_cart7, key_champ7, champion_cart8, key_champ8, champion_cart9, key_champ9,
                   champion_cart10, key_champ10, 'Var1', 'Var2', 'Freq')

green_tot_1_mi<- se.freq(green1s1, greenkeymen_s1, green1s2, greenkeymen_s2, green1s3, greenkeymen_s3,
                         green1s4, greenkeymen_s4, green1s5, greenkeymen_s5, green1s6, greenkeymen_s6,
                         green1s7, greenkeymen_s7, green1s8, greenkeymen_s8, green1s9, greenkeymen_s9,
                         green1s10, greenkeymen_s10, 'Var1', 'Var2', 'Freq')

green_tot_3_mi<- se.freq(green3s1, greenkeymen_s1, green3s2, greenkeymen_s2, green3s3, greenkeymen_s3,
                         green3s4, greenkeymen_s4, green3s5, greenkeymen_s5, green3s6, greenkeymen_s6,
                         green3s7, greenkeymen_s7, green3s8, greenkeymen_s8, green3s9, greenkeymen_s9,
                         green3s10, greenkeymen_s10, 'Var1', 'Var2', 'Freq')

green_tot_5_mi<- se.freq(green5s1, greenkeymen_s1, green5s2, greenkeymen_s2, green5s3, greenkeymen_s3,
                         green5s4, greenkeymen_s4, green5s5, greenkeymen_s5, green5s6, greenkeymen_s6,
                         green5s7, greenkeymen_s7, green5s8, greenkeymen_s8, green5s9, greenkeymen_s9,
                         green5s10, greenkeymen_s10, 'Var1', 'Var2', 'Freq')

green_tot_2_mi<- se.freq(green2s1, greenkeywomen_s1, green2s2, greenkeywomen_s2, green2s3, greenkeywomen_s3,
                         green2s4, greenkeywomen_s4, green2s5, greenkeywomen_s5, green2s6, greenkeywomen_s6,
                         green2s7, greenkeywomen_s7, green2s8, greenkeywomen_s8, green2s9, greenkeywomen_s9,
                         green2s10, greenkeywomen_s10, 'Var1', 'Var2', 'Freq')

green_tot_4_mi<- se.freq(green4s1, greenkeywomen_s1, green4s2, greenkeywomen_s2, green4s3, greenkeywomen_s3,
                         green4s4, greenkeywomen_s4, green4s5, greenkeywomen_s5, green4s6, greenkeywomen_s6,
                         green4s7, greenkeywomen_s7, green4s8, greenkeywomen_s8, green4s9, greenkeywomen_s9,
                         green4s10, greenkeywomen_s10, 'Var1', 'Var2', 'Freq')

green_tot_6_mi<- se.freq(green6s1, greenkeywomen_s1, green6s2, greenkeywomen_s2, green6s3, greenkeywomen_s3,
                         green6s4, greenkeywomen_s4, green6s5, greenkeywomen_s5, green6s6, greenkeywomen_s6,
                         green6s7, greenkeywomen_s7, green6s8, greenkeywomen_s8, green6s9, greenkeywomen_s9,
                         green6s10, greenkeywomen_s10, 'Var1', 'Var2', 'Freq')

gard_tot_mi_1<- se.freq(gard1hill_1, gard1key_1, gard1hill_2, gard1key_2, gard1hill_3, gard1key_3,
                        gard1hill_4, gard1key_4, gard1hill_5, gard1key_5, gard1hill_6, gard1key_6,
                        gard1hill_7, gard1key_7, gard1hill_8, gard1key_8, gard1hill_9, gard1key_9,
                        gard1hill_10, gard1key_10, 'Var1', 'Var2', 'Freq')

gard_tot_mi_4<- se.freq(gard4hill_1, gard4key_1, gard4hill_2, gard4key_2, gard4hill_3, gard4key_3,
                        gard4hill_4, gard4key_4, gard4hill_5, gard4key_5, gard4hill_6, gard4key_6,
                        gard4hill_7, gard4key_7, gard4hill_8, gard4key_8, gard4hill_9, gard4key_9,
                        gard4hill_10, gard4key_10, 'Var1', 'Var2', 'Freq')

gard_tot_mi_2a<- se.freq(gard2ahill_1, gard2akey_1, gard2ahill_2, gard2akey_2, gard2ahill_3, gard2akey_3,
                         gard2ahill_4, gard2akey_4, gard2ahill_5, gard2akey_5, gard2ahill_6, gard2akey_6,
                         gard2ahill_7, gard2akey_7, gard2ahill_8, gard2akey_8, gard2ahill_9, gard2akey_9,
                         gard2ahill_10, gard2akey_10, 'Var1', 'Var2', 'Freq')

gard_tot_mi_2b<- se.freq(gard2bhill_1, gard2bkey_1, gard2bhill_2, gard2bkey_2, gard2bhill_3, gard2bkey_3,
                         gard2bhill_4, gard2bkey_4, gard2bhill_5, gard2bkey_5, gard2bhill_6, gard2bkey_6,
                         gard2bhill_7, gard2bkey_7, gard2bhill_8, gard2bkey_8, gard2bhill_9, gard2bkey_9,
                         gard2bhill_10, gard2bkey_10, 'Var1', 'Var2', 'Freq')

gard_tot_mi_3a<- se.freq(gard3ahill_1, gard3akey_1, gard3ahill_2, gard3akey_2, gard3ahill_3, gard3akey_3,
                         gard3ahill_4, gard3akey_4, gard3ahill_5, gard3akey_5, gard3ahill_6, gard3akey_6,
                         gard3ahill_7, gard3akey_7, gard3ahill_8, gard3akey_8, gard3ahill_9, gard3akey_9,
                         gard3ahill_10, gard3akey_10, 'Var1', 'Var2', 'Freq')

gard_tot_mi_3b<- se.freq(gard3bhill_1, gard3bkey_1, gard3bhill_2, gard3bkey_2, gard3bhill_3, gard3bkey_3,
                         gard3bhill_4, gard3bkey_4, gard3bhill_5, gard3bkey_5, gard3bhill_6, gard3bkey_6,
                         gard3bhill_7, gard3bkey_7, gard3bhill_8, gard3bkey_8, gard3bhill_9, gard3bkey_9,
                         gard3bhill_10, gard3bkey_10, 'Var1', 'Var2', 'Freq')


bal_tot_mi_1<- se.freq(ballard1_1, ballard1key1, ballard1_2, ballard1key2, ballard1_3, ballard1key3,
                       ballard1_4, ballard1key4, ballard1_5, ballard1key5, ballard1_6, ballard1key6,
                       ballard1_7, ballard1key7, ballard1_8, ballard1key8, ballard1_9, ballard1key9,
                       ballard1_10, ballard1key10, 'Var1', 'Var2', 'Freq')

bal_tot_mi_2<- se.freq(ballard2_1, ballard2key1, ballard2_2, ballard2key2, ballard2_3, ballard2key3,
                       ballard2_4, ballard2key4, ballard2_5, ballard2key5, ballard2_6, ballard2key6,
                       ballard2_7, ballard2key7, ballard2_8, ballard2key8, ballard2_9, ballard2key9,
                       ballard2_10, ballard2key10, 'Var1', 'Var2', 'Freq')

bal_tot_mi_3<- se.freq(ballard3_1, ballard3key1, ballard3_2, ballard3key2, ballard3_3, ballard3key3,
                       ballard3_4, ballard3key4, ballard3_5, ballard3key5, ballard3_6, ballard3key6,
                       ballard3_7, ballard3key7, ballard3_8, ballard3key8, ballard3_9, ballard3key9,
                       ballard3_10, ballard3key10, 'Var1', 'Var2', 'Freq') 

bal_tot_mi_4<- se.freq(ballard4_1, ballard4key1, ballard4_2, ballard4key2, ballard4_3, ballard4key3,
                       ballard4_4, ballard4key4, ballard4_5, ballard4key5, ballard4_6, ballard4key6,
                       ballard4_7, ballard4key7, ballard4_8, ballard4key8, ballard4_9, ballard4key9,
                       ballard4_10, ballard4key10, 'Var1', 'Var2', 'Freq')



#reiter
champ_tot_reiter<-se.freq.reiter(champion_cart1, key_champ1, champion_cart2, key_champ2, champion_cart3, key_champ3, 
                                 champion_cart4, key_champ4, champion_cart5, key_champ5, champion_cart6, key_champ6,
                                 champion_cart7, key_champ7, champion_cart8, key_champ8, champion_cart9, key_champ9,
                                 champion_cart10, key_champ10, key_champ_syn,'Var1', 'Var2', 'Freq')



green_reiter_1_mi<- se.freq.reiter(green1s1, greenkeymen_s1, green1s2, greenkeymen_s2, green1s3, greenkeymen_s3,
                                   green1s4, greenkeymen_s4, green1s5, greenkeymen_s5, green1s6, greenkeymen_s6,
                                   green1s7, greenkeymen_s7, green1s8, greenkeymen_s8, green1s9, greenkeymen_s9,
                                   green1s10, greenkeymen_s10, greenkeymen_orig,'Var1', 'Var2', 'Freq')

green_reiter_3_mi<- se.freq.reiter(green3s1, greenkeymen_s1, green3s2, greenkeymen_s2, green3s3, greenkeymen_s3,
                                   green3s4, greenkeymen_s4, green3s5, greenkeymen_s5, green3s6, greenkeymen_s6,
                                   green3s7, greenkeymen_s7, green3s8, greenkeymen_s8, green3s9, greenkeymen_s9,
                                   green3s10, greenkeymen_s10, greenkeymen_orig,'Var1', 'Var2', 'Freq')

green_reiter_5_mi<- se.freq.reiter(green5s1, greenkeymen_s1, green5s2, greenkeymen_s2, green5s3, greenkeymen_s3,
                                   green5s4, greenkeymen_s4, green5s5, greenkeymen_s5, green5s6, greenkeymen_s6,
                                   green5s7, greenkeymen_s7, green5s8, greenkeymen_s8, green5s9, greenkeymen_s9,
                                   green5s10, greenkeymen_s10, greenkeymen_orig,'Var1', 'Var2', 'Freq')

green_reiter_2_mi<- se.freq.reiter(green2s1, greenkeywomen_s1, green2s2, greenkeywomen_s2, green2s3, greenkeywomen_s3,
                                   green2s4, greenkeywomen_s4, green2s5, greenkeywomen_s5, green2s6, greenkeywomen_s6,
                                   green2s7, greenkeywomen_s7, green2s8, greenkeywomen_s8, green2s9, greenkeywomen_s9,
                                   green2s10, greenkeywomen_s10,greenkeywomen_orig, 'Var1', 'Var2', 'Freq')

green_reiter_4_mi<- se.freq.reiter(green4s1, greenkeywomen_s1, green4s2, greenkeywomen_s2, green4s3, greenkeywomen_s3,
                                   green4s4, greenkeywomen_s4, green4s5, greenkeywomen_s5, green4s6, greenkeywomen_s6,
                                   green4s7, greenkeywomen_s7, green4s8, greenkeywomen_s8, green4s9, greenkeywomen_s9,
                                   green4s10, greenkeywomen_s10,greenkeywomen_orig, 'Var1', 'Var2', 'Freq')

green_reiter_6_mi<- se.freq.reiter(green6s1, greenkeywomen_s1, green6s2, greenkeywomen_s2, green6s3, greenkeywomen_s3,
                                   green6s4, greenkeywomen_s4, green6s5, greenkeywomen_s5, green6s6, greenkeywomen_s6,
                                   green6s7, greenkeywomen_s7, green6s8, greenkeywomen_s8, green6s9, greenkeywomen_s9,
                                   green6s10, greenkeywomen_s10,greenkeywomen_orig, 'Var1', 'Var2', 'Freq')



gard_reiter_mi_1<- se.freq.reiter(gard1hill_1, gard1key_1, gard1hill_2, gard1key_2, gard1hill_3, gard1key_3,
                                  gard1hill_4, gard1key_4, gard1hill_5, gard1key_5, gard1hill_6, gard1key_6,
                                  gard1hill_7, gard1key_7, gard1hill_8, gard1key_8, gard1hill_9, gard1key_9,
                                  gard1hill_10, gard1key_10, gard1keyorig, 'Var1', 'Var2', 'Freq')

gard_reiter_mi_4<- se.freq.reiter(gard4hill_1, gard4key_1, gard4hill_2, gard4key_2, gard4hill_3, gard4key_3,
                                  gard4hill_4, gard4key_4, gard4hill_5, gard4key_5, gard4hill_6, gard4key_6,
                                  gard4hill_7, gard4key_7, gard4hill_8, gard4key_8, gard4hill_9, gard4key_9,
                                  gard4hill_10, gard4key_10, gard4keyorig, 'Var1', 'Var2', 'Freq')

gard_reiter_mi_2a<- se.freq.reiter(gard2ahill_1, gard2akey_1, gard2ahill_2, gard2akey_2, gard2ahill_3, gard2akey_3,
                                   gard2ahill_4, gard2akey_4, gard2ahill_5, gard2akey_5, gard2ahill_6, gard2akey_6,
                                   gard2ahill_7, gard2akey_7, gard2ahill_8, gard2akey_8, gard2ahill_9, gard2akey_9,
                                   gard2ahill_10, gard2akey_10, gard2akeyorig, 'Var1', 'Var2', 'Freq')

gard_reiter_mi_2b<- se.freq.reiter(gard2bhill_1, gard2bkey_1, gard2bhill_2, gard2bkey_2, gard2bhill_3, gard2bkey_3,
                                   gard2bhill_4, gard2bkey_4, gard2bhill_5, gard2bkey_5, gard2bhill_6, gard2bkey_6,
                                   gard2bhill_7, gard2bkey_7, gard2bhill_8, gard2bkey_8, gard2bhill_9, gard2bkey_9,
                                   gard2bhill_10, gard2bkey_10, gard2bkeyorig, 'Var1', 'Var2', 'Freq')

gard_reiter_mi_3a<- se.freq.reiter(gard3ahill_1, gard3akey_1, gard3ahill_2, gard3akey_2, gard3ahill_3, gard3akey_3,
                                   gard3ahill_4, gard3akey_4, gard3ahill_5, gard3akey_5, gard3ahill_6, gard3akey_6,
                                   gard3ahill_7, gard3akey_7, gard3ahill_8, gard3akey_8, gard3ahill_9, gard3akey_9,
                                   gard3ahill_10, gard3akey_10, gard3akeyorig, 'Var1', 'Var2', 'Freq')

gard_reiter_mi_3b<- se.freq.reiter(gard3bhill_1, gard3bkey_1, gard3bhill_2, gard3bkey_2, gard3bhill_3, gard3bkey_3,
                                   gard3bhill_4, gard3bkey_4, gard3bhill_5, gard3bkey_5, gard3bhill_6, gard3bkey_6,
                                   gard3bhill_7, gard3bkey_7, gard3bhill_8, gard3bkey_8, gard3bhill_9, gard3bkey_9,
                                   gard3bhill_10, gard3bkey_10, gard3bkeyorig, 'Var1', 'Var2', 'Freq')



bal_reiter_mi_1<- se.freq.reiter(ballard1_1, ballard1key1, ballard1_2, ballard1key2, ballard1_3, ballard1key3,
                                 ballard1_4, ballard1key4, ballard1_5, ballard1key5, ballard1_6, ballard1key6,
                                 ballard1_7, ballard1key7, ballard1_8, ballard1key8, ballard1_9, ballard1key9,
                                 ballard1_10, ballard1key10,ballard1keyorig, 'Var1', 'Var2', 'Freq')

bal_reiter_mi_2<- se.freq.reiter(ballard2_1, ballard2key1, ballard2_2, ballard2key2, ballard2_3, ballard2key3,
                                 ballard2_4, ballard2key4, ballard2_5, ballard2key5, ballard2_6, ballard2key6,
                                 ballard2_7, ballard2key7, ballard2_8, ballard2key8, ballard2_9, ballard2key9,
                                 ballard2_10, ballard2key10, ballard2keyorig, 'Var1', 'Var2', 'Freq')

bal_reiter_mi_3<- se.freq.reiter(ballard3_1, ballard3key1, ballard3_2, ballard3key2, ballard3_3, ballard3key3,
                                 ballard3_4, ballard3key4, ballard3_5, ballard3key5, ballard3_6, ballard3key6,
                                 ballard3_7, ballard3key7, ballard3_8, ballard3key8, ballard3_9, ballard3key9,
                                 ballard3_10, ballard3key10,ballard3keyorig, 'Var1', 'Var2', 'Freq') 

bal_reiter_mi_4<- se.freq.reiter(ballard4_1, ballard4key1, ballard4_2, ballard4key2, ballard4_3, ballard4key3,
                                 ballard4_4, ballard4key4, ballard4_5, ballard4key5, ballard4_6, ballard4key6,
                                 ballard4_7, ballard4key7, ballard4_8, ballard4key8, ballard4_9, ballard4key9,
                                 ballard4_10, ballard4key10,ballard4keyorig, 'Var1', 'Var2', 'Freq')

#Leventhal combining

lev1<- combine.mi.freqL(leventhalt1_1, leventhalt1_2, leventhalt1_3, leventhalt1_4, leventhalt1_5,
                        leventhalt1_6, leventhalt1_7, leventhalt1_8, leventhalt1_9, leventhalt1_10,
                        'Var1', 'Var2', 'Freq')

lev2<- combine.mi.freqL(leventhalt2_1, leventhalt2_2, leventhalt2_3, leventhalt2_4, leventhalt2_5,
                        leventhalt2_6, leventhalt2_7, leventhalt2_8, leventhalt2_9, leventhalt2_10,
                        'Var1', 'Var2', 'Freq')

lev3<- combine.mi.freqL(leventhalt3_1, leventhalt3_2, leventhalt3_3, leventhalt3_4, leventhalt3_5,
                        leventhalt3_6, leventhalt3_7, leventhalt3_8, leventhalt3_9, leventhalt3_10,
                        'Var1', 'Var2', 'Freq')

lev4<- combine.mi.freqL(leventhalt4_1, leventhalt4_2, leventhalt4_3, leventhalt4_4, leventhalt4_5,
                        leventhalt4_6, leventhalt4_7, leventhalt4_8, leventhalt4_9, leventhalt4_10,
                        'Var1', 'Var2', 'Freq')

lev5<- combine.mi.freqL(leventhalt5_1, leventhalt5_2, leventhalt5_3, leventhalt5_4, leventhalt5_5,
                        leventhalt5_6, leventhalt5_7, leventhalt5_8, leventhalt5_9, leventhalt5_10,
                        'Var1', 'Var2', 'Freq')

lev6<- combine.mi.freqL(leventhalt6_1, leventhalt6_2, leventhalt6_3, leventhalt6_4, leventhalt6_5,
                        leventhalt6_6, leventhalt6_7, leventhalt6_8, leventhalt6_9, leventhalt6_10,
                        'Var1', 'Var2', 'Freq')

lev7<- combine.mi.freqL(leventhalt7_1, leventhalt7_2, leventhalt7_3, leventhalt7_4, leventhalt7_5,
                        leventhalt7_6, leventhalt7_7, leventhalt7_8, leventhalt7_9, leventhalt7_10,
                        'Var1', 'Var2', 'Freq')










