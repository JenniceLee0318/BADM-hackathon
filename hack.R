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

allCustomers[is.na(allCustomers$perc_weight),]$perc_weight = "Normal Weight"
allCustomers[is.na(allCustomers$bmi_class),]$bmi_class = "Normal Weight"


allCustomers$clinic_id = as.factor(allCustomers$clinic_id)
allCustomers$othercare = as.factor(allCustomers$othercare)
allCustomers$arthritis = as.factor(allCustomers$arthritis)
allCustomers$highBP = as.factor(allCustomers$highBP)
allCustomers$stroke = as.factor(allCustomers$stroke)
allCustomers$diabetes = as.factor(allCustomers$diabetes)
allCustomers$median_age_fsa = as.factor(allCustomers$median_age_fsa)
allCustomers$gave_birth_last5 = as.factor(allCustomers$gave_birth_last5)

summary(allCustomers)

colSums(is.na(allCustomers))

# Correlation Matrix
corrMatrix <- cor(select_if(filter(allCustomers.clean, Sample != "Holdout"), is.numeric))
corrplot(corrMatrix, method="number", type="lower", diag = FALSE, number.cex = 0.7)

paste(names(allCustomers),collapse = " , ")

allCustomers.clean = select(allCustomers, fsa , pt_id , income , age , edu , perc_health , perc_weight , bmi_class , arthritis , highBP , diabetes , stroke , repstrain , injstatus , physactivityindicator , gave_birth_last5 , perc_mentalHealth , perc_lifstress , perc_workstress , care_language , othercare , spending , Sample , clinic_id , power_us , median_age_fsa , hhold_work_health , avg_dcost , avg_insur_prem , tot_spend_toba_alco)
summary(allCustomers.clean)
########################### Data Modelling #################################

estimation = filter(allCustomers.clean, Sample == "Estimation")
validation = filter(allCustomers.clean, Sample == "Validation")
holdout = filter(allCustomers.clean, Sample == "Holdout")
paste(names(allCustomers.clean),collapse = " + ")


######################### Logistic Regression ###################


LinearMV <- glm(power_us ~ income + age + edu + perc_health + perc_weight + bmi_class + arthritis + highBP + diabetes + stroke + repstrain + injstatus + physactivityindicator + gave_birth_last5 + perc_mentalHealth + perc_lifstress + perc_workstress + care_language + othercare + spending + median_age_fsa + hhold_work_health + avg_dcost + avg_insur_prem + tot_spend_toba_alco, data = estimation)
predictPowerUS.linear <- predict(LinearMV, validation, type = 'response')
table_mat <- table(validation$power_us, predictPowerUS.linear > 0.5)
(table_mat[[1]] + table_mat[[4]])/nrow(validation) 


###################### Random Forest ###############################

ForestMV <- randomForest(power_us ~ income + age + edu + perc_health + perc_weight + bmi_class + arthritis + highBP + diabetes + stroke + repstrain + injstatus + physactivityindicator + gave_birth_last5 + perc_mentalHealth + perc_lifstress + perc_workstress + care_language + othercare + spending + median_age_fsa + hhold_work_health + avg_dcost + avg_insur_prem + tot_spend_toba_alco, data = estimation,
                          importance = TRUE, ntree = 500, mtry = 4,) # keep a copy of the model for later plotting
predictForest = predict(ForestMV, newdata = validation)
forest_table <- table(validation$power_us, predictForest > 0.5)
(forest_table[[1]] + forest_table[[4]])/nrow(validation) 
varImpPlot(ForestMV)

table_mat <- table(filter(allCustomers,Sample == "Validation")$power_us, predictTree > 0.5)
table_mat

###################### Non Linear Logistics #########################



##################### Submission #################
submit = filter(uncorrelated, Sample == "Holdout")
submit$score = predictForest
submit = select(submit, pt_id, score)
write.csv(submit, "NA Omitters - Forest.csv")
