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
colSums(is.na(allCustomers))

# Removing patient ID from list of predictors
rownames(allCustomers) <- allCustomers$pt_id
allCustomers$pt_id <- NULL

# Recoding perc_weight level "Just About Right" to "Normal Weight" to match with bmi_class
levels(allCustomers$perc_weight)[levels(allCustomers$perc_weight)=="Just About Right"] = "Normal Weight"
summary(allCustomers)

train = allCustomers[!is.na(allCustomers$perc_weight),]
test = allCustomers[is.na(allCustomers$perc_weight),]

perc_weight_rf <- randomForest(perc_weight ~ perc_health + arthritis + highBP + 
                                 diabetes + stroke + repstrain + injstatus + 
                                 physactivityindicator + perc_mentalHealth + 
                                 perc_lifstress + perc_workstress, data=train, 
                                 importance = TRUE, ntree = 500, mtry = 4)

predict_weight = predict(perc_weight_rf, test, type = 'class')
test$perc_weight = predict_weight

############## Everything above this. Do not remove #############################

train$Sample_weight = create.samples(train, 0.95, 0.05)
WeightNnet <- Nnet(perc_weight ~ perc_health + arthritis + highBP + 
                     diabetes + stroke + repstrain + injstatus + 
                     physactivityindicator + perc_mentalHealth + 
                     perc_lifstress + perc_workstress, data=filter(train, Sample_weight == "Estimation"),
                     decay = 0.15, size = 4)
perc_weight_rf_test <- rpart(perc_weight ~ perc_health + arthritis + highBP + 
                                 diabetes + stroke + repstrain + injstatus + 
                                 physactivityindicator + perc_mentalHealth + 
                                 perc_lifstress + perc_workstress, data=filter(train, Sample_weight == "Estimation")) 
#                               importance = TRUE, ntree = 500, mtry = 4)

predict_weight = predict(perc_weight_rf_test, filter(train, Sample_weight == "Validation"), type = 'class')
table_mat <- table(filter(train, Sample_weight == "Validation")$perc_weight, predict_weight)
table_mat









perc_bmi_rf <- randomForest(bmi_class ~ perc_weight  + perc_health + arthritis + highBP + 
                                 diabetes + stroke + repstrain + injstatus + 
                                 physactivityindicator + perc_mentalHealth + 
                                 perc_lifstress + perc_workstress, importance = TRUE, ntree = 500, mtry = 4,
                            data=filter(train, Sample == "Estimation"))


predict_bmi <- predict(perc_bmi_rf, filter(train, Sample == "Validation"), type = 'class')
table_mat <- table(filter(train, Sample == "Validation")$bmi_class, predict_bmi)
table_mat
