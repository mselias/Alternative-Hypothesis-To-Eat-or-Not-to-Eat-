## Alternate Hypothesis (Data Cleaning) ##
## Renames the Columns ##
colnames(DNA)[which(names(DNA) == "high_d_lipid")] <- "HDL"
colnames(DNA)[which(names(DNA) == "low_d_lipid")] <- "LDL"
colnames(DNA)[which(names(DNA) == "cmv_tertile")] <- "Tertile"
colnames(DNA)[which(names(DNA) == "cholesterol")] <- "Chol"
colnames(DNA)[which(names(DNA) == "R22_bpgroup")] <- "BP"

## Recoding the Values for the Tertile Column ##
DNA_Cleaned$Tertile[DNA_Cleaned$Tertile == "Low"] = 1
DNA_Cleaned$Tertile[DNA_Cleaned$Tertile == "Medium"] = 2
DNA_Cleaned$Tertile[DNA_Cleaned$Tertile == "High"] = 3
DNA_Cleaned$Tertile[DNA_Cleaned$Tertile == "Negative"] = 0

## Recoding the Values for the BP Column ##
DNA_Cleaned$BP[DNA_Cleaned$BP == "Pre-hypertension"] = 1
DNA_Cleaned$BP[DNA_Cleaned$BP == "Hypertension stage 1"] = 2
DNA_Cleaned$BP[DNA_Cleaned$BP == "Hypertension stage 2"] = 3
DNA_Cleaned$BP[DNA_Cleaned$BP == "Normal"] = 0

## Recoding the Values for Gender Column ##
DNA_Cleaned$sex[DNA_Cleaned$sex == "Male"] <- 1
DNA_Cleaned$sex[DNA_Cleaned$sex == "Female"] <- 0

## Splitting Data Set ##
DNA[which(DNA == is.na(DNA)),] <- "NA"
DNA_Cleaned <- DNA[-c(which(DNA == "NA")),]
DNA_Cleaned_Test <- DNA_Cleaned[-c(which(DNA_Cleaned$cmvstatus == 0)),]
DNA_Cleaned_2 <- DNA_Cleaned_Test[which(DNA_Cleaned_Test$Tertile == 2 | DNA_Cleaned_Test$Tertile == 3),]

## Recoding Values to be Numeric ##
DNA_Cleaned$LDL <- as.numeric(DNA_Cleaned$LDL)
DNA_Cleaned$LDL <- as.numeric(DNA_Cleaned$HDL)
DNA_Cleaned$Chol <- as.numeric(DNA_Cleaned$Chol)
DNA_Cleaned$cmv <- as.numeric(DNA_Cleaned$cmv)
DNA_Cleaned$cmvstatus <- as.numeric(DNA_Cleaned$cmvstatus)
DNA_Cleaned$hiv <- as.numeric(DNA_Cleaned$hiv)
DNA_Cleaned$BMI <- as.numeric(DNA_Cleaned$BMI)
DNA_Cleaned$Tertile <- as.numeric(DNA_Cleaned$Tertile)
DNA_Cleaned$hba1c <- as.numeric(DNA_Cleaned$hba1c)
DNA_Cleaned$mean_syst <- as.numeric(DNA_Cleaned$mean_syst)
DNA_Cleaned$mean_diast <- as.numeric(DNA_Cleaned$mean_diast)
DNA_Cleaned$sex <- as.numeric(DNA_Cleaned$sex)
DNA_Cleaned$BP <- as.numeric(DNA_Cleaned$BP)

## Linear Regression Model ##
## Factoring Data ##
DNA_Cleaned$sex <- factor(DNA_Cleaned$sex)
DNA_Cleaned$hiv <- factor(DNA_Cleaned$hiv)
DNA_Cleaned$BP <- factor(DNA_Cleaned$BP)

DNA_Cleaned_Test$sex <- factor(DNA_Cleaned_Test$sex)
DNA_Cleaned_Test$hiv <- factor(DNA_Cleaned_Test$hiv)
DNA_Cleaned_Test$BP <- factor(DNA_Cleaned_Test$BP)

## Using DNA_Cleaned ##
# Iteration 1
reg_data_1 <- DNA_Cleaned
regression_1 <- lm(cmv ~ sex  + mean_syst + mean_diast + BP + Chol + HDL, data=reg_data_1)
summary(regression_1)
# Iteration 2
reg_data_1 <- DNA_Cleaned
regression_1 <- lm(cmv ~ sex  + mean_syst + mean_diast + BP + Chol + LDL, data=reg_data_1)
summary(regression_1)
# Iteration 3
reg_data_1 <- DNA_Cleaned
regression_1 <- lm(cmv ~ Chol + LDL, data=reg_data_1)
summary(regression_1)
# Iteration 4
reg_data_1 <- DNA_Cleaned
regression_1 <- lm(cmv ~ Chol + HDL, data=reg_data_1)
summary(regression_1)

## Using DNA_Cleaned_Test ##
# Iteration 1
reg_data_2 <- DNA_Cleaned_Test
regression_2 <- lm(cmv ~ sex  + mean_syst + mean_diast + BP + Chol + HDL, data=reg_data_2)
summary(regression_2)
# Iteration 2
reg_data_2 <- DNA_Cleaned_Test
regression_2 <- lm(cmv ~ sex  + mean_syst + mean_diast + BP + Chol + LDL, data=reg_data_2)
summary(regression_2)
#Iteration 3
reg_data_2 <- DNA_Cleaned_Test
regression_2 <- lm(cmv ~ Chol + LDL, data=reg_data_2)
summary(regression_2)
#Iteration 4
reg_data_2 <- DNA_Cleaned_Test
regression_2 <- lm(cmv ~ Chol + HDL, data=reg_data_2)
summary(regression_2)

## Using DNA_Cleaned_2 ##
# Iteration 1
reg_data_3 <- DNA_Cleaned_2
regression_3 <- lm(cmv ~ sex  + mean_syst + mean_diast + BP + Chol + HDL, data=reg_data_3)
summary(regression_3)
# Iteration 2
reg_data_3 <- DNA_Cleaned_2
regression_3 <- lm(cmv ~ Chol + HDL, data=reg_data_3)
summary(regression_3)
