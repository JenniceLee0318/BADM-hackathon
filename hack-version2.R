library("pdp")
library("car")
library("gam")
library("nnet")
library("dplyr")
library("rpart")
library("sqldf")
library("gplots")
library("foreign")
library("forcats")
library("effects")
library("corrplot")
library("rpart.plot")
library("randomForest")
source("BCA_functions_source_file.R")

# Reading the datasets
fsa = read.csv("FSA_DATASET.csv", stringsAsFactors = TRUE)
survey = read.csv("SURVEY_DATASET.csv", stringsAsFactors = TRUE)
usage = read.csv("USAGE_DATASET.csv", stringsAsFactors = TRUE)

####################### DATA CLEANING #################################

# Renaming columns
colnames(usage)[which(names(usage) == "clinic_fsa")] <- "fsa"

# Merging survey and usage datasets based on patient ID
allCustomers = merge(x = survey, y = usage, by = "pt_id")

# Merging the above merged dataset with fsa data based on FSA 
allCustomers = merge(x = allCustomers, y = fsa, by = "fsa")

# Removing rows with null values in perc_weight and bmi_class for estimation and validation sample
allCustomers = allCustomers[!(is.na(allCustomers$bmi_class) & is.na(allCustomers$perc_weight) & !(allCustomers$Sample %in% c("Holdout"))),]

# Recoding perc_weight level "Just About Right" to "Normal Weight" to match with bmi_class
levels(allCustomers$perc_weight)[levels(allCustomers$perc_weight)=="Just About Right"] = "Normal Weight"
summary(allCustomers)

colSums(is.na(allCustomers))

allCustomers[is.na(allCustomers$perc_weight),]$perc_weight = "Normal Weight"
allCustomers[is.na(allCustomers$bmi_class),]$bmi_class = "Normal Weight"

allCustomers$clinic_id = as.factor(allCustomers$clinic_id)

uncorrelated = select(allCustomers, power_us, pt_id, income, age, edu, perc_health, perc_weight, bmi_class, arthritis, highBP, diabetes, stroke,
                      repstrain, injstatus, physactivityindicator, gave_birth_last5, perc_mentalHealth, perc_lifstress,  perc_workstress,
                      care_language, othercare, spending, median_age_fsa, avg_insur_prem, tot_spend_toba_alco, Sample)

# Correlation Matrix
corrMatrix <- cor(select_if(filter(uncorrelated, Sample != "Holdout"), is.numeric))
corrplot(corrMatrix, method="number", type="lower", diag = FALSE, number.cex = 0.7)

uncorrelated$othercare = as.factor(uncorrelated$othercare)
uncorrelated$arthritis = as.factor(uncorrelated$arthritis)
uncorrelated$highBP = as.factor(uncorrelated$highBP)
uncorrelated$stroke = as.factor(uncorrelated$stroke)
uncorrelated$diabetes = as.factor(uncorrelated$diabetes)
uncorrelated$median_age_fsa = as.factor(uncorrelated$median_age_fsa)
uncorrelated$gave_birth_last5 = as.factor(uncorrelated$gave_birth_last5)
summary(uncorrelated)
########################### Data Modelling #################################

estimation = filter(uncorrelated, Sample == "Estimation")
validation = filter(uncorrelated, Sample == "Validation")
holdout = filter(uncorrelated, Sample == "Holdout")

###################### Decision Tree ###############################

VMForest <- randomForest(power_us ~ income + age + edu + perc_health + perc_weight + bmi_class + arthritis + highBP + diabetes + stroke +
                          repstrain + injstatus + physactivityindicator + gave_birth_last5 + perc_mentalHealth + perc_lifstress +  perc_workstress +
                          care_language + othercare + spending + median_age_fsa + avg_insur_prem + tot_spend_toba_alco, data = filter(uncorrelated, Sample != "Holdout"),
                          importance = TRUE, ntree = 500, mtry = 4,) # keep a copy of the model for later plotting

predictForest = predict(VMForest, newdata = filter(uncorrelated, Sample == "Holdout"))

submit = filter(uncorrelated, Sample == "Holdout")
submit$score = predictForest
submit = select(submit, pt_id, score)
write.csv(submit, "NA Omitters - Forest.csv")

table_mat <- table(filter(allCustomers,Sample == "Validation")$power_us, predictTree > 0.5)
table_mat

GamMV <- gam(formula = power_us ~ income + age + edu + perc_health + perc_weight + bmi_class + arthritis + highBP + diabetes + stroke +
                  repstrain + injstatus + physactivityindicator + gave_birth_last5 + perc_mentalHealth + perc_lifstress +  perc_workstress +
                  care_language + othercare + s(spending) + median_age_fsa + s(avg_insur_prem) + s(tot_spend_toba_alco), data = estimation)

summary(GamMV)

UncorrelatedMV.Linear <- glm(formula = power_us ~ income + age + edu + perc_health + perc_weight + bmi_class + arthritis + highBP + diabetes + stroke +
                        repstrain + injstatus + physactivityindicator + gave_birth_last5 + perc_mentalHealth + perc_lifstress +  perc_workstress +
                        care_language + othercare + s(spending) + median_age_fsa + avg_insur_prem + tot_spend_toba_alco, data = estimation)


predictPowerUS.linear <- predict(UncorrelatedMV.Linear, validation, type = 'response')
table_mat <- table(validation$power_us, predictPowerUS.linear > 0.5)
# Print the results
table_mat


VMStep = step(UncorrelatedMV.Linear, direction="both")
summary(VMStep)

predictPowerUS.step <- predict(VMStep, newdata = validation)
table_mat <- table(validation$power_us, predictPowerUS.step > 0.5)
# Print the results
table_mat
