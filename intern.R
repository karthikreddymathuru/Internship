
# reading the data 

diabetes = read.csv("diabetic_data.csv")
id_mapping = read.csv("IDs_mapping.csv") # Id mappings are for few columns (categorical) in diabetes data

#Dimensions

dim(diabetes)



#structure
str(diabetes)

# Converting attributes to relevant formats (as they should be).
diabetes$weight = as.numeric(diabetes$weight)
diabetes$admission_type_id = as.factor(diabetes$admission_type_id)
diabetes$admission_source_id = as.factor(diabetes$admission_source_id)
diabetes$discharge_disposition_id = as.factor(diabetes$discharge_disposition_id)

str(diabetes)# checking the structure 


#summary
summary(diabetes)



# dealing with missing values 
# '?' should be treated as a missing value in all the below columns

diabetes$race[diabetes$race == '?'] = NA
diabetes$race = droplevels(diabetes$race,exclude = "?")
diabetes$race = droplevels(diabetes$race,exclude = NA)
levels(diabetes$race)


diabetes$payer_code[diabetes$payer_code == "?"] = NA
diabetes$payer_code = droplevels(diabetes$payer_code, exclude = NA)
levels(diabetes$payer_code)

diabetes$medical_specialty[diabetes$medical_specialty == "?"] = NA
diabetes$medical_specialty = droplevels(diabetes$medical_specialty, exclude = "?")
diabetes$medical_specialty = droplevels(diabetes$medical_specialty, exclude = NA)
levels(diabetes$medical_specialty)

# Invalid can be a transgender but there are only three of them so 
#check for any extremeties in particular row , if any remove them or else impute them.
diabetes$gender[diabetes$gender == 'Unknown/Invalid'] = NA
diabetes$gender = droplevels(diabetes$gender,exclude = "Unknown/Invalid")
diabetes$gender = droplevels(diabetes$gender,exclude = NA)
levels(diabetes$gender)


# '?' tells us that they don't have any data about the diagnosis.
diabetes$diag_1[diabetes$diag_1 == '?'] = NA
diabetes$diag_1 = droplevels(diabetes$diag_1, exclude = '?')
diabetes$diag_1 = droplevels(diabetes$diag_1, exclude = NA)

diabetes$diag_2[diabetes$diag_2 == '?'] = NA
diabetes$diag_2 = droplevels(diabetes$diag_2, exclude = '?')
diabetes$diag_2 = droplevels(diabetes$diag_2, exclude = NA)

diabetes$diag_3[diabetes$diag_3 == '?'] = NA
diabetes$diag_3 = droplevels(diabetes$diag_3, exclude = '?')
diabetes$diag_3 = droplevels(diabetes$diag_3, exclude = NA)


summary(diabetes)


# Total number of missing values
sum(is.na(diabetes))

# individual attribute missing values

colSums(is.na(diabetes)) 
# race - 2273 , gender - 3,payer_code - 40256 , medical_speciality - 49949,
# diag_1 - 21, diag_2 - 358, diag_3 - 1423

# Number of missing values in rows

which(rowSums(is.na(diabetes))>4)




# NUmerical Attributes :

diabetes_Num_attr = subset(diabetes, select = c('encounter_id','patient_nbr','weight',
                                                'time_in_hospital','num_lab_procedures',
                                                'num_procedures','num_medications',
                                                'number_outpatient','number_emergency',
                                                'number_inpatient','number_diagnoses'))

#Categorical Attributes :

diabetes_ctgr_attr = subset(diabetes, select = -c(encounter_id,patient_nbr,time_in_hospital,
                                                  num_lab_procedures,weight,
                                                  num_procedures,num_medications,
                                                  number_outpatient,number_emergency,
                                                  number_inpatient,number_diagnoses))

# Changing the levels of the admission_type_id as in id_mapping.
levels(diabetes_ctgr_attr$admission_type_id) = c('emergency','urgent','elective','new_born',
                                                 'Not_Avail','null','trauma_canter','not_mapped')

summary(diabetes_ctgr_attr$admission_type_id)


# COrrelation plot
library(corrplot)

corrplot(cor(diabetes_Num_attr),method = 'number',diag = F)

# Class Imbalance

summary(diabetes$diabetesMed)



# Checking each attribute.
# NUmerical attributes.

hist(diabetes$time_in_hospital) # Max people have been only for a day 

hist(diabetes$num_medications) 

hist(diabetes$num_lab_procedures)

hist(diabetes$num_procedures)

summary(diabetes$number_diagnoses) 
hist(diabetes$number_diagnoses)



# Below numerical attributes have least Variance / no variance

hist(diabetes$number_outpatient) 
summary(diabetes$number_outpatient) 

summary(diabetes$number_emergency) # No variance
hist(diabetes$number_emergency)

summary(diabetes$number_inpatient) 
hist(diabetes$number_inpatient)



# categorical attributes
library(lattice)
#Gender	Nominal	Values: male, female, and unknown/invalid
histogram(diabetes$gender) # we have more females


# Race	Nominal	Values: Caucasian, Asian, African American, Hispanic, and other
histogram(diabetes$race) # we have Caucasian race as the highest


#Age	Nominal	Grouped in 10-year intervals
histogram(diabetes$age) # Age group of 70-80 are maximum

#Admission type 	Nominal	Integer identifier corresponding to 9 distinct values
#1-Emergency 2-Urgent 3-Elective 4-Newborn 5-Not Available 6-NULL  7-Trauma Center 8-Not Mapped

summary(diabetes_ctgr_attr$admission_type_id)
histogram(diabetes_ctgr_attr$admission_type_id) # Emergency is maximum

summary(diabetes_ctgr_attr$discharge_disposition_id)
histogram(diabetes_ctgr_attr$discharge_disposition_id) #Many people were dischared to home

histogram(diabetes_ctgr_attr$admission_source_id)# Max people were in emergency room

histogram(diabetes_ctgr_attr$payer_code) # max is MC

histogram(diabetes_ctgr_attr$medical_specialty)

histogram(d$diag_1)
histogram(diabetes_ctgr_attr$diag_2)
histogram(diabetes_ctgr_attr$diag_3)

histogram(diabetes_ctgr_attr$max_glu_serum) # 90% of glucose serum test is not measured
histogram(diabetes_ctgr_attr$A1Cresult) # 80% of the tests were not measured

# Medications

histogram(diabetes_ctgr_attr$metformin.pioglitazone) # Drug was not prescribed for any one
summary(diabetes_ctgr_attr$metformin.pioglitazone)

histogram(diabetes_ctgr_attr$metformin.rosiglitazone)# Drug was not prescribed for any one
summary(diabetes_ctgr_attr$metformin.rosiglitazone)

histogram(diabetes_ctgr_attr$glimepiride.pioglitazone)# Drug was not prescribed for any one
summary(diabetes_ctgr_attr$glimepiride.pioglitazone)

histogram(diabetes_ctgr_attr$glipizide.metformin)# Drug was not prescribed for any one
summary(diabetes_ctgr_attr$glipizide.metformin)

histogram(diabetes_ctgr_attr$glyburide.metformin)# Drug was only prescribed for 2-3% of patients
summary(diabetes_ctgr_attr$glyburide.metformin)


histogram(diabetes_ctgr_attr$citoglipton)
summary(diabetes_ctgr_attr$citoglipton)

histogram(diabetes_ctgr_attr$examide)
summary(diabetes_ctgr_attr$examide)


histogram(diabetes_ctgr_attr$tolazamide)
summary(diabetes_ctgr_attr$tolazamide)

histogram(diabetes_ctgr_attr$troglitazone)
summary(diabetes_ctgr_attr$troglitazone)

histogram(diabetes_ctgr_attr$miglitol)
summary(diabetes_ctgr_attr$miglitol)

histogram(diabetes_ctgr_attr$acarbose)
summary(diabetes_ctgr_attr$acarbose)

histogram(diabetes_ctgr_attr$tolbutamide)
summary(diabetes_ctgr_attr$tolbutamide)

histogram(diabetes_ctgr_attr$acetohexamide)
summary(diabetes_ctgr_attr$acetohexamide)

histogram(diabetes_ctgr_attr$chlorpropamide)
summary(diabetes_ctgr_attr$chlorpropamide)

histogram(diabetes_ctgr_attr$nateglinide)
summary(diabetes_ctgr_attr$nateglinide)

histogram(diabetes_ctgr_attr$repaglinide)
summary(diabetes_ctgr_attr$repaglinide)

# Above are the medications(Drugs) with least variance/no varince

histogram(diabetes_ctgr_attr$insulin)
histogram(diabetes_ctgr_attr$pioglitazone)
histogram(diabetes_ctgr_attr$rosiglitazone)
histogram(diabetes_ctgr_attr$glipizide)
histogram(diabetes_ctgr_attr$glyburide)
histogram(diabetes_ctgr_attr$glimepiride)
histogram(diabetes_ctgr_attr$metformin)

#Change of medications	:Nominal	Indicates if there was a change in diabetic medications
# (either dosage or generic name). Values: "change" and "no change"
histogram(diabetes_ctgr_attr$change)

#Days to inpatient readmission. Values: "<30" if the patient was readmitted in less than 
#30 days, ">30" if the patient was readmitted in more than 30 days, and "No" for no 
#record of readmission.
histogram(diabetes_ctgr_attr$readmitted) #55% patients were not readmitted

diabetes_new = cbind.data.frame(diabetes_Num_attr,diabetes_ctgr_attr)
str(diabetes_new)





# Feature engineeering

# we have inpatients,outpatients and emergency entries of patient preceding year. So combining all of 
#them to get total number of entries in preceding year.

histogram(diabetes$number_inpatient)
histogram(diabetes$number_outpatient)
histogram(diabetes$number_emergency)

diabetes_patients_preceding_year = (diabetes$number_inpatient
                                    +diabetes$number_outpatient
                                    +diabetes$number_emergency)

histogram(diabetes_patients_preceding_year)
summary(diabetes_patients_preceding_year)

# We have number of lab procedures encountered and number of procedures encountered other than 
# lab procedures . So, we have total procedures encountered.

diabetes_total_tests_performed = (diabetes$num_lab_procedures+diabetes$num_procedures)

hist(diabetes_total_tests_performed)
summary(diabetes_total_tests_performed)

#Adding to the data set 

diabetes_new$total_tets_performed = diabetes_total_tests_performed
diabetes_new$patients_preceeding_year = diabetes_patients_preceding_year

# remove unnecessary variables

hist(diabetes$weight) 
# By histogram we can observe that frequency of weight(in pounds) is 1 for almost 
# all of the data. New born babies have a weight of minimum 5 pounds, so this coloumn is dropped.

diabetes_new$weight = NULL
diabetes_new$encounter_id = NULL
diabetes_new$patient_nbr = NULL
diabetes_new$num_lab_procedures = NULL
diabetes_new$num_procedures = NULL
diabetes_new$payer_code = NULL
diabetes_new$number_outpatient = NULL
diabetes_new$number_emergency = NULL
diabetes_new$number_inpatient = NULL
diabetes_new$metformin.pioglitazone = NULL
diabetes_new$metformin.rosiglitazone = NULL
diabetes_new$glimepiride.pioglitazone = NULL
diabetes_new$glipizide.metformin = NULL
diabetes_new$glyburide.metformin = NULL
diabetes_new$citoglipton = NULL
diabetes_new$examide = NULL
diabetes_new$tolazamide = NULL
diabetes_new$troglitazone = NULL
diabetes_new$miglitol = NULL
diabetes_new$acarbose = NULL
diabetes_new$tolbutamide = NULL
diabetes_new$acetohexamide = NULL
diabetes_new$chlorpropamide = NULL
diabetes_new$nateglinide = NULL
diabetes_new$repaglinide = NULL


View(diabetes_new)


# imputation of data 

sum(is.na(diabetes_new))
library(DMwR)
imputed = centralImputation(diabetes_new)
sum(is.na(imputed))

# Box Plots

for (i in 1:11){
  print(i)
  par(mfrow=c(1,1))
  boxplot(x = diabetes_Num_attr[,i], xlab= names(diabetes_Num_attr[i]))
  dev.copy(jpeg,filename = paste(names(diabetes_Num_attr[i]),".jpg"))
  dev.off()
}
  
# Histograms
for (i in 1:39){
  print(histogram(diabetes_ctgr_attr[,i],xlab = names(diabetes_ctgr_attr[i])))
  dev.copy(jpeg,filename = paste(names(diabetes_ctgr_attr[i]),"plot.jpg"))
  dev.off()
}

boxplot(diabetes_Num_attr$num_procedures)
summary(diabetes_Num_attr$patient_nbr)


d1 = cbind.data.frame(time_in_hospital = imputed$time_in_hospital,
                    num_medications=  imputed$num_medications,
                    number_diagnoses = imputed$number_diagnoses,
                    patients_preceeding_year = imputed$patients_preceeding_year,
       total_tets_performed = imputed$total_tets_performed)

# Making some missing data 

# Time in hospital
for(j in seq(1,101766,12)){
  d1$time_in_hospital = replace(d1$time_in_hospital,j,NA)
}

# Num of medications
for(j in seq(1,101766,11)){
  d1$num_medications = replace(d1$num_medications,j,NA)
}

#Num of diagnoses
for(j in seq(1,101766,10)){
  d1$number_diagnoses = replace(d1$number_diagnoses,j,NA)
}
# Total tests performed
for(j in seq(1,101766,17)){
  d1$total_tets_performed = replace(d1$total_tets_performed,j,NA)
}
#Patients in preceeding year
for(j in seq(1,101766,14)){
  d1$patients_preceeding_year = replace(d1$patients_preceeding_year,j,NA)
}


# KNN IMPUTATION

d1_imputed = centralImputation(d1)

for (i in 1:5){
  par(mfrow=c(1,2))
  boxplot(d1[,i],xlab = names(d1[i]))
  boxplot(d1_imputed[,i],xlab =names(d1[i]))
  dev.copy(jpeg,filename = paste(names(d1[i]),".jpg"))
  dev.off()
}

# diagnosis ICD 9 codes 

imputed$diag_1 =  as.character(diabetes$diag_1)
imputed$diag_1[ imputed$diag_1 <= 139] = 'infectious'
imputed$diag_1[imputed$diag_1  >= 140 & imputed$diag_1 <= 239] = 'neoplasams'
imputed$diag_1[imputed$diag_1  >= 240 & imputed$diag_1 <= 279]= 'immunity'
imputed$diag_1[imputed$diag_1  >= 280 & imputed$diag_1 <= 289]= 'blood'
imputed$diag_1[imputed$diag_1  >= 290 & imputed$diag_1 <= 319]= 'mental disorders'
imputed$diag_1[imputed$diag_1  >= 320 & imputed$diag_1 <= 389]= 'nervous & sense organs'
imputed$diag_1[imputed$diag_1  >= 390 & imputed$diag_1 <= 459]= 'circulatory sys'
imputed$diag_1[imputed$diag_1  >= 460 & imputed$diag_1 <= 519]= 'respiratory'
imputed$diag_1[imputed$diag_1  >= 520 & imputed$diag_1 <= 579]='digestive'
imputed$diag_1[imputed$diag_1  >= 580 & imputed$diag_1 <= 629]= 'genitourinary'
imputed$diag_1[imputed$diag_1  >= 630 & imputed$diag_1 <= 679]='pergnency/childbirth'
imputed$diag_1[imputed$diag_1  >= 680 & imputed$diag_1 <= 709]='skin'
imputed$diag_1[imputed$diag_1  >= 710 & imputed$diag_1 <= 739] = 'musculoskeletal'
imputed$diag_1[imputed$diag_1  >= 740 & imputed$diag_1 <= 759]= 'congenital anomalies'
imputed$diag_1[imputed$diag_1  >= 760 & imputed$diag_1 <= 779]= 'perinatal period'
imputed$diag_1[imputed$diag_1  >= 780 & imputed$diag_1 <= 799]= 'ill-defined'
imputed$diag_1[imputed$diag_1  >= 800 & imputed$diag_1 <= 999]= 'injury & poisining'
imputed$diag_1[imputed$diag_1  == 8] ='infectious'
imputed$diag_1[imputed$diag_1  == 52] ='infectious'
imputed$diag_1[imputed$diag_1  == 39] ='infectious'
imputed$diag_1[imputed$diag_1  == 58] ='infectious'
imputed$diag_1[imputed$diag_1  == 78] ='infectious'


str(imputed$diag_1)

imputed$diag_1 = as.factor(imputed$diag_1)
str(imputed$diag_1)
summary(imputed$diag_1)

#install.packages('rockchalk')
library(rockchalk)

imputed$diag_1 = combineLevels(imputed$diag_1,levs = c('E909','V07','V25','V26','V43','V45','V51','V53',
                                                       'V55','V54','V56','V57','V58','V60','V63','V66','V67',
                                                       'V70','V71'),newLabel = 'External Injury')
summary(imputed$diag_1)



# Realtionship between attributes


par(mfrow= c(1,1))
plot(imputed$readmitted,imputed$diabetesMed,xlab = "readmitted",ylab = "diabetesmed")
dev.copy(jpeg,"readmitted_diabetesmed.jpg")
dev.off()

plot(imputed$diabetesMed,imputed$diag_1,xlab = "diabetes Medication",ylab = "diagnosis 1" )
dev.copy(jpeg,"diabetesmed_diag1.jpg")
dev.off()

plot(imputed$readmitted,imputed$diag_1,xlab = "readmitted",ylab = "diagnosis 1" )
dev.copy(jpeg,"readmitted_diag1.jpg")
dev.off()


plot(imputed$max_glu_serum,imputed$A1Cresult,xlab = "Glucose serum test",
     ylab = "A1c result")
dev.copy(jpeg,"glucose-test-res_a1c-res.jpg")
dev.off()


par(mfrow=c(1,2))
plot(imputed$max_glu_serum,imputed$diabetesMed,xlab = "Glucose serum test",
     ylab = "Diabetes Medication")

plot(imputed$A1Cresult,imputed$diabetesMed,xlab = "A1c result",
     ylab = "Diabetes Medication")
dev.copy(jpeg,"glucose&A1c-Diabeticmed.jpg")
dev.off()




