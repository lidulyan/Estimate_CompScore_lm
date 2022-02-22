
###Inputs:
#all_patients - full data of 62 patients (62 patients x 372 regions)
#patient_num - the patient disconnection pattern (1 x 372 regions) - dependent variable
#components - the Disconnectome (46 components x 372 regions) - 46 predictors

###Outputs:
# 46 betas for the patient (patient_num)
# explained variance (R2 adjusted) for the patient (patient_num)
# linear regression model for the patient (patient_num)

Estimate_CompScore_lm <- function(all_patients, patient_num, components) {
  #create a matrix with data of a patient#patient_num and 46 components
  patient = cbind(all_patients[patient_num], components)
  #perform linear model
  patient_model = lm(formula(patient), data = patient)
  #extract beta coefficients
  beta = summary(patient_model)$coefficients[,1]
  beta = as.data.frame(beta)
  #extract R2 adjusted
  adjR2 = summary(patient_model)$adj.r.squared
  return(list(beta, adjR2, patient_model))
}

# Perform estimate_score_lm for each patients 
component_n = 0:46 # in total 47 because the first beta is intercept
df_total = as.data.frame(component_n)
adjR2_total = vector()

for(i in 1:62) {
  # patients_mot - data from 62 patients (62 disconnetcion patterns that each includes 372 regions); 
  # components - data from the Disconnectome (46 components that each includes 372 regions)
  df = as.data.frame(my_lm(patients_mot,i,components)[1]) 
  df_total <- cbind(df_total,df) #append the result to the df_total data frame
  
  adjR2 = my_lm(patients_mot,i,components)[2]
  adjR2_total <- c(adjR2_total,adjR2) # append the value to the list adjR2_total
}

adjR2 = unlist(adjR2_total, use.names=FALSE)

mean(adjR2)
sd(adjR2)



