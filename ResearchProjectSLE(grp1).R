library(readxl)
library(dplyr)
OriginalData <- read_excel("g1_s1_dataset_v251001.xlsx")
View(OriginalData)
##OriginalData <- OriginalData %>% mutate(time_since_diagnosis_years = ifelse(OriginalData$time_since_diagnosis_years == 0, NA, time_since_diagnosis_years))
OriginalData <- OriginalData %>% 
  mutate(time_since_diagnosis_years = time_since_diagnosis_years + 1)
shapiro.test(OriginalData$opg_pg_ml)
cor.test(OriginalData$sledai_score, OriginalData$opg_pg_ml, method = ("pearson"))

##do this for all variables and then combine into a list
SLEDAI_counts <- OriginalData %>% group_by(sledai_score) %>% reframe(count = n())

barplot(SLEDAI_counts$count, 
        axes = T, axisnames = T, xlab = "SLEDAI score", ylab = "Count", 
        col = "cyan", names.arg = SLEDAI_counts$sledai_score, cex.names = 0.8)
patientsSLescore <- OriginalData %>% group_by(sledai_score) %>% reframe(opg_pg_ml)
par(cex.axis = 0.7, mfrow = c(1,1))
boxplot(patientsSLescore$opg_pg_ml ~ patientsSLescore$sledai_score, 
        col = 'pink', xlab = 'SLEDAI score', ylab = "OPG plasma levels (pg/mL)")

##normalization
dummy_menopause <- data.frame(OriginalData$sub_id, OriginalData$menopausal_status)
dummy_menopause <- dummy_menopause %>% mutate(menopause_numeric = 
                                                 case_when(
                                                    OriginalData.menopausal_status == 'pre' ~ 1, 
                                                    OriginalData.menopausal_status == 'post' ~ 2))
dummy_ethnicity <- data.frame(OriginalData$sub_id, OriginalData$ethnicity)
dummy_ethnicity <- dummy_ethnicity %>% mutate(ethnicity_numeric = case_when
                                              (OriginalData.ethnicity == 'caucasian' ~ 1,
                                                 OriginalData.ethnicity == 'non_caucasian' ~ 2))

#NORMALIZATION
##By patient age
OPGbyAge <- data.frame(OriginalData$sub_id, OriginalData$sledai_score, OriginalData$opg_pg_ml, 
                       OriginalData$age_years, 
                       (OriginalData$opg_pg_ml/OriginalData$age_years))
colnames(OPGbyAge) <- c('Patient ID', 'SLEDAI 2k score', 'OPG [pg/mL]', 
                        'Age (years)', 'OPG [pg/mL] normalized by Age')
cor.test(OPGbyAge$`OPG [pg/mL] normalized by Age`, OPGbyAge$`SLEDAI 2k score`)
boxplot(OPGbyAge$`OPG [pg/mL] normalized by Age` ~ OPGbyAge$`SLEDAI 2k score`,
        xlab = 'SLEDAI 2k score', ylab = 'OPG [pg/mL] normalized by age')

##One singular data frame
Normalizedbyconfounding <- data.frame(OriginalData$sub_id, OriginalData$sledai_score, 
                                      (OriginalData$opg_pg_ml/OriginalData$age_at_diagnosis_years), 
                                      (OriginalData$opg_pg_ml/OriginalData$time_since_diagnosis_years), 
                                      (OriginalData$opg_pg_ml/OriginalData$age_years), 
                                      (OriginalData$opg_pg_ml/OriginalData$bmi_kg_m2), 
                                      (OriginalData$opg_pg_ml/OriginalData$ifn_type1_iu_ml), 
                                      (OriginalData$opg_pg_ml/dummy_ethnicity$ethnicity_numeric), 
                                      (OriginalData$opg_pg_ml/dummy_menopause$menopause_numeric))
colnames(Normalizedbyconfounding) <- c('Patient ID', 'SLEDAI 2k score', 
                                       'OPG [pg/ml] normalized by Age at diagnosis (years)',
                                       'OPG [pg/ml] normalized by Time since diagnosis (years)', 
                                       'OPG [pg/ml] normalized by Age (years)', 
                                       'OPG [pg/ml] normalized by BMI (kg/m^2)', 
                                       'OPG [pg/ml] normalized by IFN expression',
                                       'OPG [pg/ml] normalized by Ethnictiy', 
                                       'OPG [pg/ml] normalized by Menopausal Status')
par(cex.axis = 0.7, mfrow = c(1,2), cex = 0.7)
boxplot(Normalizedbyconfounding$`OPG [pg/ml] normalized by Menopausal Status`~dummy_menopause$OriginalData.menopausal_status,
        xlab = 'Menopausal Status', ylab = 'OPG [pg/mL] normalized by menopausal status', col = "magenta")
boxplot(Normalizedbyconfounding$`OPG [pg/ml] normalized by Menopausal Status`~Normalizedbyconfounding$`SLEDAI 2k score`, 
        xlab = 'SLEDAI 2k score', ylab = 'OPG [pg.mL] normalized by menopausal status', col = "darkmagenta")

par(cex.axis = 0.7, mfrow = c(1,2), cex = 0.7)
boxplot(Normalizedbyconfounding$`OPG [pg/ml] normalized by Ethnictiy`~dummy_ethnicity$OriginalData.ethnicity,
        xlab = 'Ethnicity', ylab = 'OPG [pg/mL] normalized by ethnicity', col = "darkslategrey")
boxplot(Normalizedbyconfounding$`OPG [pg/ml] normalized by Ethnictiy`~Normalizedbyconfounding$`SLEDAI 2k score`,
        xlab = 'SLEDAI 2K score', ylab = 'OPG [pg/mL] normalized by ethnicity', col = "slategrey")

par(cex.axis = 0.7, mfrow = c(1,2), cex = 0.7)
#age at diagnosis
boxplot(Normalizedbyconfounding$`OPG [pg/ml] normalized by Age at diagnosis (years)`~Normalizedbyconfounding$`SLEDAI 2k score`, 
        xlab = 'SLEDAI score', ylab = 'OPG [pg/mL] normalized by Age at diagnosis (years)', 
        col = 'blue')
boxplot(Normalizedbyconfounding$`OPG [pg/ml] normalized by Age at diagnosis (years)`~OriginalData$age_at_diagnosis_years, 
        xlab = 'Age at diagnosis (years)', ylab = 'OPG [pg/mL] normalized by Age at diagnosis (years)', 
        col = 'cyan')
#time since diagnosis
boxplot(Normalizedbyconfounding$`OPG [pg/ml] normalized by Time since diagnosis (years)`~Normalizedbyconfounding$`SLEDAI 2k score`, 
        xlab = 'SLEDAI score', ylab = 'OPG [pg/mL] normalized by time since diagnosis (years)',
        col = 'green')
boxplot(Normalizedbyconfounding$`OPG [pg/ml] normalized by Time since diagnosis (years)`~OriginalData$time_since_diagnosis_years, 
        xlab = 'Time since diagnosis (years)', ylab = 'OPG [pg/mL] normalized by Time since diagnosis (years)', 
        col = "darkgreen")
#age
boxplot(Normalizedbyconfounding$`OPG [pg/ml] normalized by Age (years)`~Normalizedbyconfounding$`SLEDAI 2k score`, 
        xlab = 'SLEDAI score', ylab = 'OPG [pg/mL] normalized by Age (years)', 
        col = "yellow")
boxplot(Normalizedbyconfounding$`OPG [pg/ml] normalized by Age (years)`~OriginalData$age_years, 
        xlab = 'Patient Age (years)', ylab = 'OPG [pg/mL] normalized by Age (years)', 
        col = "gold")
#BMI
boxplot(Normalizedbyconfounding$`OPG [pg/ml] normalized by BMI (kg/m^2)`~Normalizedbyconfounding$`SLEDAI 2k score`, 
        xlab = 'SLEDAI score', ylab = 'OPG [pg/mL] normalized by BMI (kg/m^2)', 
        col = "brown")
#plot(OriginalData$bmi_kg_m2, Normalizedbyconfounding$`OPG [pg/ml] normalized by BMI (kg/m^2)`,
        #xlab = 'Patient BMI (kg/m^2)', ylab = 'OPG [pg/mL] normalized by BMI (kg/m^2)')
plot(OriginalData$bmi_kg_m2, Normalizedbyconfounding$`OPG [pg/ml] normalized by BMI (kg/m^2)`,
     xlab = 'Patient BMI (kg/m^2)', ylab = 'OPG [pg/mL] normalized by BMI (kg/m^2)', 
     col = "darkorange")
#IFN expression
boxplot(Normalizedbyconfounding$`OPG [pg/ml] normalized by IFN expression`~Normalizedbyconfounding$`SLEDAI 2k score`, 
        xlab = 'SLEDAI score', ylab = 'OPG [pg/mL] normalized by IFN expression [IU/mL)', 
        col = "red")
plot(OriginalData$ifn_type1_iu_ml, Normalizedbyconfounding$`OPG [pg/ml] normalized by IFN expression`, 
     xlab = 'IFN [IU/mL]', ylab = 'OPG [pg/mL] normalized by IFN expression [IU/mL]', 
     col = "pink")

#CORRELATION
##1 issue: some of the times since diagnosis are '0', which means that when normalizing the value is
##infinite, how to solve? I made them NA values (NOT STR) so that the stat test would just kind of ignore them. 

cols <- c('OPG [pg/ml] normalized by Age at diagnosis (years)', 
          'OPG [pg/ml] normalized by Time since diagnosis (years)', 
          'OPG [pg/ml] normalized by Age (years)', 
          'OPG [pg/ml] normalized by BMI (kg/m^2)', 
          'OPG [pg/ml] normalized by IFN expression', 
          'OPG [pg/ml] normalized by Ethnictiy', 
          'OPG [pg/ml] normalized by Menopausal Status')
print(cols)
all(cols %in% names(Normalizedbyconfounding)) #if true, proceed; if not, some name(s) don't match)
shapiro_results <- lapply(cols, function(col) {
  data <- na.omit(Normalizedbyconfounding[[col]])
  if (length(data) >= 3) {
    shapiro.test(data)$statistic
  } else {
    NA
  }
})
names(shapiro_results) <- cols
shapiro_results <- t(as.data.frame(shapiro_results))
shapiro_results_p <- lapply(cols, function(col) {
  data <- na.omit(Normalizedbyconfounding[[col]]) 
  if (length(data) >= 3) {
    shapiro.test(data)$p
    } else {
      NA
      }
  })
names(shapiro_results_p) <- cols
shapiro_results_p <- t(as.data.frame(shapiro_results_p))
shapiro_results_confounding <- data.frame(shapiro_results, shapiro_results_p)
colnames(shapiro_results_confounding) <- c('W statistic', 'P-value')
correlations <- sapply(cols, function(col) {cor(Normalizedbyconfounding[['SLEDAI 2k score']], 
                                                Normalizedbyconfounding[[col]], use = "complete.obs")
  })
correlations <- as.data.frame(correlations)
stat_norm_conf <- data.frame(correlations$correlations, 
                             shapiro_results_confounding$`W statistic`, 
                             shapiro_results_confounding$`P-value`)
row.names(stat_norm_conf) <- cols
colnames(stat_norm_conf) <- c('Correlation Coefficient', 'Shapiro W statistic', 'Shapiro P-value')
rm(correlations, shapiro_results, shapiro_r$etsults_p)

##plots
model <- lm(opg_pg_ml ~ age_at_diagnosis_years + time_since_diagnosis_years +
              age_years + bmi_kg_m2 + ifn_type1_iu_ml + ethnicity + menopausal_status, 
            data = OriginalData)
summary(model)
plot(model)
model1 <- as.data.frame(model[["fitted.values"]])
opg_sledai <- data.frame(OriginalData$sledai_score, model$fitted.values)
opg_sledai <- data.frame(opg_sledai$OriginalData.sledai_score, opg_sledai$model.fitted.values)
colnames(opg_sledai) <- c('SLEDAI 2k score', 'Adjusted OPG [pg/mL}')
rm(opg_sleda)
ggplot(data = opg_sledai, aes(`SLEDAI 2k score`, `Adjusted OPG [pg/mL}`)) +
  + geom_point(size = 2, shape = 21) 
plot(opg_sledai$`SLEDAI 2k score`, opg_sledai$`Adjusted OPG [pg/mL}`)

library(psych)
describe(Normalizedbyconfounding)

##Moving on from the confounding, we can do a similar thing for the biomarkers
biomarkers <- data.frame(OriginalData$vwf_iu_dl, OriginalData$sdc1_ng_ml, 
                         OriginalData$tm_ng_ml, OriginalData$ox_ldl_ng_ml, 
                         OriginalData$svcam1_ng_ml, OriginalData$ldh_u_l)
names(biomarkers) <- c('von Willebrand Factor', 'soluble DC-1', 
                       'Thrombomodulin', 'oxidized LDL', 'soluble VCAM-1', 'LDH')
summary(biomarkers)
describe(biomarkers)
vWF_model <- lm(vwf_iu_dl ~ age_at_diagnosis_years + time_since_diagnosis_years +
              age_years + bmi_kg_m2 + ifn_type1_iu_ml + ethnicity + menopausal_status, 
            data = OriginalData)
sDC1_model <- lm(sdc1_ng_ml ~ age_at_diagnosis_years + time_since_diagnosis_years +
                  age_years + bmi_kg_m2 + ifn_type1_iu_ml + ethnicity + menopausal_status, 
                data = OriginalData)
tm_model <- lm(tm_ng_ml~ age_at_diagnosis_years + time_since_diagnosis_years +
                  age_years + bmi_kg_m2 + ifn_type1_iu_ml + ethnicity + menopausal_status, 
                data = OriginalData)
oxLDL_model <- lm(OriginalData$ox_ldl_ng_ml ~ age_at_diagnosis_years + time_since_diagnosis_years +
                  age_years + bmi_kg_m2 + ifn_type1_iu_ml + ethnicity + menopausal_status, 
                data = OriginalData)
sVCAM1_model <- lm(svcam1_ng_ml ~ age_at_diagnosis_years + time_since_diagnosis_years +
                  age_years + bmi_kg_m2 + ifn_type1_iu_ml + ethnicity + menopausal_status, 
                data = OriginalData)
LDL_model <- lm(ldh_u_l ~ age_at_diagnosis_years + time_since_diagnosis_years +
                  age_years + bmi_kg_m2 + ifn_type1_iu_ml + ethnicity + menopausal_status, 
                data = OriginalData)
