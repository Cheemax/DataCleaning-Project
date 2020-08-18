
data <- read.csv('/Users/chiam/Documents/Second Semester/Data Analytics/Week 4/Income Dirty Data.csv',row.names=1)
sum(is.na(data))
sum(!is.na(data)) 
sum(!is.na(data))/prod(dim(data))*100
#Checking data that conforms to rules
E <- editset(c("age >=18", "tax==(income*0.15)","income>0"))
violatedEdits(E,data)
ve<-violatedEdits(E,data)
summary(ve)
cr<- correctionRules ( expression ( if ( !is.na(gender) & gender=="Man") gender<- "Male",  if ( !is.na(gender) & gender=="Men") gender<- "Male", if ( !is.na(gender) & gender=="Woman") gender<- "Female", if ( !is.na(gender) & gender=="Women") gender<- "Female"))
cor<- correctWithRules(cr, data)
new_data<-cor$corrected
plot(new_data$gender)
#Correction of null income
cr<- correctionRules(expression (if( !is.na(income) & income<0) income <-NA))
cor<- correctWithRules(cr,new_data)
new_data<- cor$corrected
new_data
#Correction of null tax
cr<- correctionRules(expression (if( !is.na(tax) & tax<0) tax <-NA))
cor<- correctWithRules(cr,new_data)
new_data<- cor$corrected
new_data
#Correction of null age
cr <- correctionRules(expression (if( !is.na(age) & age<18) age <-NA))
cor<- correctWithRules(cr, new_data)
new_data<-cor$corrected

#Correction of NA taxes and Income
cr<-correctionRules(expression(if (!is.na(income)& income>0 & is.na(tax)) tax <- (income * 0.15)))
cr<-correctionRules(expression(if (!is.na(tax) & tax>0 &is.na(income)) income <- (tax / 0.15)))
cor<- correctWithRules(cr,new_data)
new_data<- cor$corrected
new_data
#Imputing (inserting missing values) library("VIM")
data_imputation <- (cor$corrected)
data_imputation <-kNN(cor$corrected)
sum(is.na(data_imputation))



