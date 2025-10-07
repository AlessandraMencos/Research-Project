library(readxl)
library(dplyr)
library(ggplot2)
#import the data (HAS TO BE IN THE SAME FOLDER YOURE WORKING IN OR YOU HAVE TO WRITE THE WHOLE PATH:
#IE. "/user/johndoe/folder-1/foldertwo/DATA_SET_HERE"). 
OriginalData <- read_excel("g1_s1_dataset_v251001.xlsx")
View(OriginalData)

#rename the columns so everything is easier to read and keep track of
colnames(OriginalData) <- c('Patient ID', 'Age at Diagnosis (years)', 'Time since Diagnosis (years)', 
                            'Age (years)', 'BMI (kg/m^2)', 'IFN type I [IU/mL]', 
                            'Ethnicity', 'Menopausal Status', 'SLEDAI 2k Score', 
                            'Plasma OPG [pg/mL]', 'vWF [IU/mL]', 'sDC1 [ng/mL]', 
                            'TM [ng/mL]', 'ox LDL [ng/mL]', 'sVCAM-1 [ng/mL]', 
                            'LDH [u/L]')

##OriginalData <- OriginalData %>% mutate(time_since_diagnosis_years = ifelse(OriginalData$time_since_diagnosis_years == 0, NA, time_since_diagnosis_years))
##^^ we'll ask about this tomorrow
#since time since diagnosis has values equal to 0, we're going to add 1 to each of those (ie. 0+1 =1, etc)
OriginalData <- OriginalData %>% 
  mutate(`Time since Diagnosis (years)` = `Time since Diagnosis (years)` + 1)

#some stat tests for OPG levels to see how it's distributed and possible correlation to SLEDAI score
shapiro.test(OriginalData$`Plasma OPG [pg/mL]`)
cor.test(OriginalData$`SLEDAI 2k Score`, OriginalData$`Plasma OPG [pg/mL]`, method = ("pearson"))

#make a list of all the column names so that you can create functions
col_names <- c('Patient ID', 'Age at Diagnosis (years)', 'Time since Diagnosis (years)', 
         'Age (years)', 'BMI (kg/m^2)', 'IFN type I [IU/mL]', 
         'Ethnicity', 'Menopausal Status', 'SLEDAI 2k Score', 
         'Plasma OPG [pg/mL]', 'vWF [IU/mL]', 'sDC1 [ng/mL]', 
         'TM [ng/mL]', 'ox LDL [ng/mL]', 'sVCAM-1 [ng/mL]', 
         'LDH [u/L]')
#how many times does each value of a variable appear? 
#this is the function
Counts <- lapply(col_names, function(col) {
  OriginalData %>%
    group_by(.data[[col]]) %>%
    summarise(count = n())
  })
#re-establish column names
names(Counts) <- col_names

#some things are unique to each patinet, so we don't want to plot them to see the counts:
names_to_skip_in_plots <- c('Patient ID', 'BMI (kg/m^2)', 'IFN type I [IU/mL]', 
                            'SLEDAI 2k Score','Plasma OPG [pg/mL]', 
                            'vWF [IU/mL]', 'sDC1 [ng/mL]', 'TM [ng/mL]', 
                            'ox LDL [ng/mL]', 'sVCAM-1 [ng/mL]', 'LDH [u/L]')

#this is the function for the barplots of the counts:
for (name in names(Counts)) {
  if (name %in% names_to_skip_in_plots) next
  df <- Counts[[name]]
  par(mfrow = c(1,1))
  bars <- barplot(
    df$count,
    names.arg = df[[1]],
    main = paste("Counts of", name),
    col = "lightblue")
  text(x = bars, y = df$count, labels = df$count, pos = 1, cex = 0.5, col = 'black')
}
<<<<<<< HEAD
rm(df, name, bars)
=======
rm(df)
rm(name)
>>>>>>> c61c496c117361bc4d04cf695405dba557a93aab

par(cex.axis = 0.7, mfrow = c(1,2))
plot(OriginalData$`SLEDAI 2k Score`, OriginalData$`Plasma OPG [pg/mL]`,
     col = 'black', xlab = 'SLEDAI score', ylab = "OPG plasma levels (pg/mL)", 
     xlim = c(0, 28), xaxt = 'n')
axis(side = 1, at = seq(0, 28, by = 1))
mtext("Plasma OPG [pg/mL] vs patient SLEDAI 2K score",
      outer = FALSE, cex = 1, side = 3, line = 1, adj = 0)
boxplot(OriginalData$`Plasma OPG [pg/mL]`~OriginalData$`SLEDAI 2k Score`, 
        col = 'grey', xlab = 'SLEDAI score', ylab = NULL,
        xlim = c(0, 28), xaxt = 'n')
axis(side = 1, at = seq(0, 28, by = 1))

##Linear Regression
model <- glm(`Plasma OPG [pg/mL]` ~ `Age at Diagnosis (years)` + `Time since Diagnosis (years)` +
            `Age (years)` + `BMI (kg/m^2)` + `IFN type I [IU/mL]` + Ethnicity + `Menopausal Status`, 
            data = OriginalData)
summary(model)
#model_sledai <- glm(`SLEDAI 2k Score` ~ `Age at Diagnosis (years)` + `Time since Diagnosis (years)` +
               #`Age (years)` + `BMI (kg/m^2)` + `IFN type I [IU/mL]` + Ethnicity + `Menopausal Status`, 
            # data = OriginalData)
#summary(model_sledai)
cor.test(OriginalData$`SLEDAI 2k Score`, model[["fitted.values"]], method = 'pearson')
par(mfrow = c(2,2))
plot(model)
opg_sledai <- data.frame(OriginalData$`SLEDAI 2k Score`, model[["fitted.values"]])
#or: model_sledai[["fitted.values"]]
colnames(opg_sledai) <- c('SLEDAI 2k score', 'Adjusted OPG [pg/mL}')

##I also like to make one extra data frame with the patient IDs to make sure R is putting together 
##the right OPG value and SLEDAI score
opg_sledai_patientID <- data.frame(OriginalData$`Patient ID`, OriginalData$`SLEDAI 2k Score`, 
                                   model[["fitted.values"]])
colnames(opg_sledai_patientID) <- c('Patient ID', 'SLEDAI 2k score', 'Adjusted OPG [pg/mL}')

par(mfrow = c(1,2), cex.axis = 0.7)
plot(opg_sledai$`SLEDAI 2k score`, opg_sledai$`Adjusted OPG [pg/mL}`, 
     xlab = 'SLEDAI 2K score', ylab = 'Adjusted OPG [pg/mL]', col = 'deepskyblue4',
     xlim = c(0, 28), xaxt = 'n')
axis(side = 1, at = seq(0, 28, by = 1))
mtext("Adjusted OPG [pg/mL] vs patient SLEDAI 2K score",
      outer = FALSE, cex = 1, side = 3, line = 1, adj = 0)
boxplot(opg_sledai$`Adjusted OPG [pg/mL}`~opg_sledai$`SLEDAI 2k score`, 
        xlab = 'SLEDAI 2K score', ylab = NULL, col = 'deepskyblue4',
        xlim = c(0, 28), xaxt = 'n')
axis(side = 1, at = seq(0, 28, by = 1))

##Moving on from the confounding, we can do a similar thing for the biomarkers
#first, extract the biomarkes from the original data frame into a separate one
biomarkers <- data.frame(OriginalData$`vWF [IU/mL]`, OriginalData$`sDC1 [ng/mL]`, 
                         OriginalData$`TM [ng/mL]`,
                         OriginalData$`ox LDL [ng/mL]`, OriginalData$`sVCAM-1 [ng/mL]`, 
                         OriginalData$`LDH [u/L]`)
#rename the columns to make them easier to identify
names(biomarkers) <- c('von Willebrand Factor', 'soluble DC-1', 
                       'Thrombomodulin', 'oxidized LDL', 'soluble VCAM-1', 'LDH')
summary(biomarkers)

#create a linear model for each of the biomarkers
vWF_model <- glm(`vWF [IU/mL]` ~ `Age at Diagnosis (years)` + `Time since Diagnosis (years)` +
                  `Age (years)` + `BMI (kg/m^2)` + `IFN type I [IU/mL]` + Ethnicity + `Menopausal Status`, 
            data = OriginalData)
sDC1_model <- glm(`sDC1 [ng/mL]` ~ `Age at Diagnosis (years)` + `Time since Diagnosis (years)` +
                   `Age (years)` + `BMI (kg/m^2)` + `IFN type I [IU/mL]` + Ethnicity + `Menopausal Status`,
                data = OriginalData)
tm_model <- glm(`TM [ng/mL]` ~ `Age at Diagnosis (years)` + `Time since Diagnosis (years)` +
                 `Age (years)` + `BMI (kg/m^2)` + `IFN type I [IU/mL]` + Ethnicity + `Menopausal Status`,
                data = OriginalData)
oxLDL_model <- glm(`ox LDL [ng/mL]` ~ `Age at Diagnosis (years)` + `Time since Diagnosis (years)` +
                    `Age (years)` + `BMI (kg/m^2)` + `IFN type I [IU/mL]` + Ethnicity + `Menopausal Status`,
                data = OriginalData)
sVCAM1_model <- glm(`sVCAM-1 [ng/mL]` ~ `Age at Diagnosis (years)` + `Time since Diagnosis (years)` +
                     `Age (years)` + `BMI (kg/m^2)` + `IFN type I [IU/mL]` + Ethnicity + `Menopausal Status`,
                data = OriginalData)
LDH_model <- glm(`LDH [u/L]` ~ `Age at Diagnosis (years)` + `Time since Diagnosis (years)` +
                  `Age (years)` + `BMI (kg/m^2)` + `IFN type I [IU/mL]` + Ethnicity + `Menopausal Status`,
                data = OriginalData)

#exctract the adjusted values of each biomarker into a new dataframe, add the adjusted OPG from 
#previous linear model, and rename the columns. 
adjusted_biomarkers <- data.frame(model[["fitted.values"]], vWF_model[["fitted.values"]], 
                                 sDC1_model[["fitted.values"]], 
                                 tm_model[["fitted.values"]], 
                                 oxLDL_model[["fitted.values"]], 
                                 sVCAM1_model[["fitted.values"]], 
                                 LDH_model[["fitted.values"]])
colnames(adjusted_biomarkers) <- c('Plasma OPG [pg/mL]' ,'vWF [IU/mL]', 'sDC1 [ng/mL]', 
                                  'TM [ng/mL]', 'ox LDL [ng/mL]', 
                                  'sVCAM-1 [ng/mL]', 'LDH [u/L]')

#extra df to make sure R is grouping right: 
adjusted_biomarkers_patientID <- data.frame(OriginalData$`Patient ID`, OriginalData$`SLEDAI 2k Score`, model[["fitted.values"]], vWF_model[["fitted.values"]], 
                                  sDC1_model[["fitted.values"]], 
                                  tm_model[["fitted.values"]], 
                                  oxLDL_model[["fitted.values"]], 
                                  sVCAM1_model[["fitted.values"]], 
                                  LDH_model[["fitted.values"]])
colnames(adjusted_biomarkers_patientID) <- c('Patient ID', 'SLEDAI 2k score', 'Plasma OPG [pg/mL]' ,'vWF [IU/mL]', 'sDC1 [ng/mL]', 
                                   'TM [ng/mL]', 'ox LDL [ng/mL]', 
                                   'sVCAM-1 [ng/mL]', 'LDH [u/L]')

#correlation tests
cor.test(OriginalData$`SLEDAI 2k Score`, vWF_model[["fitted.values"]], method = 'pearson')
cor.test(OriginalData$`SLEDAI 2k Score`, sDC1_model[["fitted.values"]], method = 'pearson')
cor.test(OriginalData$`SLEDAI 2k Score`, tm_model[["fitted.values"]], method = 'pearson')
cor.test(OriginalData$`SLEDAI 2k Score`, oxLDL_model[["fitted.values"]], method = 'pearson')
cor.test(OriginalData$`SLEDAI 2k Score`, sVCAM1_model[["fitted.values"]], method = 'pearson')
cor.test(OriginalData$`SLEDAI 2k Score`, LDH_model[["fitted.values"]], method = 'pearson')


par(mfrow = c(1, 2), cex.axis = 0.7)
#plot vWF
plot(OriginalData$`SLEDAI 2k Score`, adjusted_biomarkers$`vWF [IU/mL]`,
     xlab = 'SLEDAI 2K score', ylab = 'Adjusted vWF [IU/mL]', col = 'deepskyblue',
     xlim = c(0, 28), xaxt = 'n')
axis(side = 1, at = seq(0, 28, by = 1))
mtext("Adjusted vWF values [IU/mL] vs Patient SLEDAI 2k Score",
      outer = FALSE, cex = 1, col = "darkslategray", side = 3, line = 1, adj = 0)
boxplot(adjusted_biomarkers$`vWF [IU/mL]`~OriginalData$`SLEDAI 2k Score`,
        ylab = 'Adjusted vWF [IU/mL]', col = 'chartreuse4',
        xlim = c(0, 28), xaxt = 'n')
axis(side = 1, at = seq(0, 28, by = 1))
#plot sDC1
plot(OriginalData$`SLEDAI 2k Score`, adjusted_biomarkers$`sDC1 [ng/mL]`,
     xlab = 'SLEDAI 2K score', ylab = 'Adjusted sDC1 [ng/mL]', col = 'deepskyblue2', 
     xlim = c(0, 28), xaxt = 'n')
axis(side = 1, at = seq(0, 28, by = 1))
mtext('Adjusted sDC1 values [ng/mL] vs Patient SLEDAI 2k score',
      outer = FALSE, cex = 1, col = "darkslategray", side = 3, line = 1, adj = 0)
boxplot(adjusted_biomarkers$`sDC1 [ng/mL]`~OriginalData$`SLEDAI 2k Score`,
        xlab = 'SLEDAI 2K score', ylab = NULL, col = 'forestgreen',
        xlim = c(0, 28), xaxt = 'n')
axis(side = 1, at = seq(0, 28, by = 1))
#plot TM
plot(OriginalData$`SLEDAI 2k Score`, adjusted_biomarkers$`TM [ng/mL]`,
     xlab = 'SLEDAI 2K score', ylab = 'Adjusted TM [ng/mL]', col = 'deepskyblue3', 
     xlim = c(0, 28), xaxt = 'n')
axis(side = 1, at = seq(0, 28, by = 1))
mtext('Adjusted TM values [ng/mL] vs Patient SLEDAI 2k score',
      outer = FALSE, cex = 1, col = "darkslategray", side = 3, line = 1, adj = 0)
boxplot(adjusted_biomarkers$`TM [ng/mL]`~OriginalData$`SLEDAI 2k Score`,
        xlab = 'SLEDAI 2K score', ylab = NULL, col = 'palegreen',
        xlim = c(0, 28), xaxt = 'n')
axis(side = 1, at = seq(0, 28, by = 1))
#plot ox LDL
plot(OriginalData$`SLEDAI 2k Score`, adjusted_biomarkers$`ox LDL [ng/mL]`,
     xlab = 'SLEDAI 2K score', ylab = 'Adjusted ox LDL [ng/mL]', col = 'royalblue', 
     xlim = c(0, 28), xaxt = 'n')
axis(side = 1, at = seq(0, 28, by = 1))
mtext('Adjusted ox LDL values [ng/mL] vs Patient SLEDAI 2k score',
      outer = FALSE, cex = 1, col = "darkslategray", side = 3, line = 1, adj = 0)
boxplot(adjusted_biomarkers$`ox LDL [ng/mL]`~OriginalData$`SLEDAI 2k Score`,
        xlab = 'SLEDAI 2K score', ylab = NULL, col = 'palegreen2',
        xlim = c(0, 28), xaxt = 'n')
axis(side = 1, at = seq(0, 28, by = 1))
#plot sVCAM1
plot(OriginalData$`SLEDAI 2k Score`, adjusted_biomarkers$`sVCAM-1 [ng/mL]`,
     xlab = 'SLEDAI 2K score', ylab = 'Adjusted s-VCAM1 [ng/mL]', col = 'royalblue3', 
     xlim = c(0, 28), xaxt = 'n')
axis(side = 1, at = seq(0, 28, by = 1))
mtext('Adjusted s-VCAM1 values [ng/mL] vs Patient SLEDAI 2k score',
      outer = FALSE, cex = 1, col = "darkslategray", side = 3, line = 1, adj = 0)
boxplot(adjusted_biomarkers$`sVCAM-1 [ng/mL]`~OriginalData$`SLEDAI 2k Score`,
        xlab = 'SLEDAI 2K score', ylab = NULL, col = 'seagreen',
        xlim = c(0, 28), xaxt = 'n')
axis(side = 1, at = seq(0, 28, by = 1))
#plot LDH
plot(OriginalData$`SLEDAI 2k Score`, adjusted_biomarkers$`LDH [u/L]`,
     xlab = 'SLEDAI 2K score', ylab = 'Adjusted LDH [U/L]', col = 'royalblue4', 
     xlim = c(0, 28), xaxt = 'n')
axis(side = 1, at = seq(0, 28, by = 1))
mtext('Adjusted LDH values [IU/mL] vs Patient SLEDAI 2k score',
      outer = FALSE, cex = 1, col = "darkslategray", side = 3, line = 1, adj = 0)
boxplot(adjusted_biomarkers$`LDH [u/L]`~OriginalData$`SLEDAI 2k Score`,
        xlab = 'SLEDAI 2K score', ylab = NULL, col = 'seagreen3',
        xlim = c(0, 28), xaxt = 'n')
axis(side = 1, at = seq(0, 28, by = 1))

##end
