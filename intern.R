
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
summary(diabetes_Num_attr)



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
levels(diabetes_ctgr_attr$admission_type_id) = c('emergency','urgent','elective','new_born','Not_Avail','null','trauma_canter','not_mapped')

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

histogram(diabetes_ctgr_attr$diag_1)
histogram(diabetes_ctgr_attr$diag_2)
histogram(diabetes_ctgr_attr$diag_3)

histogram(diabetes_ctgr_attr$max_glu_serum) # 90% of glucose serum test is not measured
histogram(diabetes_ctgr_attr$A1Cresult) # 80% of the tests were not measured

# Medications

histogram(diabetes_ctgr_attr$metformin.pioglitazone) # Drug was not prescribed for any one
histogram(diabetes_ctgr_attr$metformin.rosiglitazone)# Drug was not prescribed for any one
histogram(diabetes_ctgr_attr$glimepiride.pioglitazone)# Drug was not prescribed for any one
histogram(diabetes_ctgr_attr$glipizide.metformin)# Drug was not prescribed for any one
histogram(diabetes_ctgr_attr$glyburide.metformin)# Drug was only prescribed for 2-3% of patients
histogram(diabetes_ctgr_attr$citoglipton)
histogram(diabetes_ctgr_attr$examide)
histogram(diabetes_ctgr_attr$tolazamide)
histogram(diabetes_ctgr_attr$troglitazone)
histogram(diabetes_ctgr_attr$miglitol)
histogram(diabetes_ctgr_attr$acarbose)
histogram(diabetes_ctgr_attr$tolbutamide)
histogram(diabetes_ctgr_attr$acetohexamide)
histogram(diabetes_ctgr_attr$chlorpropamide)
histogram(diabetes_ctgr_attr$nateglinide)
histogram(diabetes_ctgr_attr$repaglinide)

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


# remove unnecessary variables

hist(diabetes$weight) 
# By histogram we can observe that frequency of weight(in pounds) is 1 for almost 
# all of the data. New born babies have a weight of minimum 5 pounds, so this coloumn is dropped.

#diabetes$weight = NULL


summary(diabetes)


dput(colnames(diabetes_ctgr_attr))


library(car)

