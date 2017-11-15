# reading the data 

diabetes = read.csv("diabetic_data.csv")
id_mapping = read.csv("IDs_mapping.csv") # Id mappings are for few columns (categorical) in diabetes data

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

# NUmerical Attributes :

diabetes_Num_attr = subset(diabetes, select = c('encounter_id','patient_nbr',
                                                'time_in_hospital','num_lab_procedures',
                                                'num_procedures','num_medications',
                                                'number_outpatient','number_emergency',
                                                'number_inpatient','number_diagnoses'))

#Categorical Attributes :

diabetes_ctgr_attr = subset(diabetes, select = -c(encounter_id,patient_nbr,time_in_hospital,num_lab_procedures,
                                                   num_procedures,num_medications,
                                                   number_outpatient,number_emergency,
                                                   number_inpatient,number_diagnoses))


# Changing the levels of the admission_type_id as in id_mapping.
levels(diabetes_ctgr_attr$admission_type_id) = c('emergency','urgent','elective','new_born','Not_Avail','null','trauma_canter','not_mapped')

summary(diabetes_ctgr_attr$admission_type_id)


# dealing with missing values 
# '?' should be treated as a missing value in all the below columns

diabetes$race[diabetes$race == "?"] = NA
diabetes$race = droplevels(diabetes$race,exclude = "?")
levels(diabetes$race)

diabetes$payer_code[diabetes$payer_code == "?"] = NA
diabetes$payer_code = droplevels(diabetes$payer_code, exclude = "?")
levels(diabetes$payer_code)

diabetes$medical_specialty[diabetes$medical_specialty == "?"] = NA
diabetes$medical_specialty = droplevels(diabetes$medical_specialty, exclude = "?")
levels(diabetes$medical_specialty)

# Invalid can be a transgender but there are only three of them so 
#check for any extremeties in particular row , if any remove them or else impute them.
diabetes$gender[diabetes$gender == 'Unknown/Invalid'] = NA
diabetes$gender = droplevels(diabetes$gender,exclude = "Unknown/Invalid")
levels(diabetes$gender)

# '?' tells us that they don't have any data about the diagnosis.
diabetes$diag_1[diabetes$diag_1 == '?'] = NA
diabetes$diag_1 = droplevels(diabetes$diag_1, exclude = '?')

diabetes$diag_2[diabetes$diag_2 == '?'] = NA
diabetes$diag_2 = droplevels(diabetes$diag_2, exclude = '?')

diabetes$diag_3[diabetes$diag_3 == '?'] = NA
diabetes$diag_3 = droplevels(diabetes$diag_3, exclude = '?')


summary(diabetes)


# Checking each attribute.
# NUmerical attributes.

hist(diabetes$time_in_hospital) # Max people have been only for a day 
hist(diabetes$num_medications)
hist(diabetes_Num_attr$num_lab_procedures)
hist(diabetes_Num_attr$num_procedures)
hist(diabetes_Num_attr$number_outpatient)
hist(diabetes_Num_attr$number_emergency)
hist()


str(diabetes_ctgr_attr)

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



histogram(diabetes_ctgr_attr$discharge_disposition_id)
histogram(diabetes_ctgr_attr$admission_source_id)
histogram(diabetes_ctgr_attr$payer_code)
histogram(diabetes_ctgr_attr$medical_specialty)
histogram(diabetes_ctgr_attr$diag_1)
histogram(diabetes_ctgr_attr$diag_2)
histogram(diabetes_ctgr_attr$diag_3)
histogram(diabetes_ctgr_attr$max_glu_serum)
histogram(diabetes_ctgr_attr$A1Cresult)


# remove unnecessary variables

hist(diabetes$weight) 
# By histogram we can observe that frequency of weight(in pounds) is 1 for almost 
# all of the data. New born babies have a weight of minimum 5 pounds, so this coloumn is dropped.

diabetes$weight = NULL