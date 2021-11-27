# ***********************************************************************
#
# IMPACT OF COVID 19 PANDEMIC ON HEALTH COVERAGE AND ECONOMIC INDICATORS
#
# Date: September 2021
#
# Author: Alen Jimenez. E-mail: jimenezalen [at] gmail.com
#
# Objective: Evaluate the impact of COVID-19 on a set of health and 
# economic measures in Argentina
#
# DataBases used: - EPH, National Institute of Statistics and Censuses (INDEC)
#
# ***********************************************************************

# INTRODUCTION: we clean the memory and load the packages

rm(list = ls())
library(pacman)
p_load(tidyverse, shiny, #basics
       rio, readxl, openxlsx, haven, googledrive, #import y export
       GGally, grid, ggrepel, ggpubr, patchwork, colorspace, ggflags, ggthemes, treemapify, #graphs
       jtools, huxtable, stargazer, #Regression tables format
       Hmisc, lubridate, collapse, imputeTS, pracma, expss, 
       DataCombine, fmsb, stringi, countrycode, extrafont, fastDummies,
       data.table)
#font_import() #we import fonts
loadfonts(device = "win") #we load the sources
options(scipen = 999) #so you dont use scientific notation
Sys.setlocale("LC_TIME", "English") #to change the default language of dates
'%notin%' <- Negate('%in%') # complementary function of% in%

# ****************************************************************************************

# DIRECTORY AND ROUTES

#set main directory

setwd("")

# ****************************************************************************************

# WE LOAD THE BASES

eph_ind_19_3 <- import("Usu_individual_T319.txt", dec = ",") #edit with appropiate route

eph_ind_20_3 <- import("usu_individual_T320.txt", dec = ",") #edit with appropiate route

eph <- plyr::rbind.fill(eph_ind_19_3, eph_ind_20_3)

# ****************************************************************************************

# EDITION OF DATABASES

# We create variable time

eph$time <- paste("T", eph$TRIMESTRE, "_", eph$ANO4, sep = "")

# We create a binary variable indicative of STATUS = occupied

'ACTIVITY CONDITION
0 = Individual interview not carried out (no response to the Individual Questionnaire)
1 = Occupied
2 = Unoccupied
3 = Inactive
4 = Under 10 years old'

eph$ocupado <- 0
eph$ocupado[eph$ESTADO == 1] <- 1
eph$ocupado[is.na(eph$ESTADO) == TRUE] <- NA

# We create a binary variable indicative of EAP (Economically active population)

'The EAP is defined according to the INDEC as the sum of employed and unemployed persons.'

eph$pea <- 0
eph$pea[eph$ESTADO %in% c(1,2)] <- 1
eph$pea[is.na(eph$ESTADO) == TRUE] <- NA

# We transform PP04B_COD to factor

'What does the business / company / institution do or produce?
(See Classifier of Economic Activities for Mercosur Sociodemographic Surveys - CAES- MERCOSUR)'

eph$PP04B_COD <- as.factor(eph$PP04B_COD)

# Binary Has Health Coverage (CH08)

'Do you have any type of health coverage that you pay for or are discounted?
1 = Social work (includes PAMI)
2 = Mutual / Prepaid / Emergency service
3 = Public insurance and plans
4 = You are not paid or discounted
9 = Ns./Nr.
12 = Social and mutual work / prepaid / emergency service
13 = Social Work and Public Plans and Insurance
23 = Mutual / prepaid / emergency service / Public Plans and Insurance
123 = social work, mutual / prepaid / emergency service and Public Plans and Insurance'

cubierto <- c(1, 2, 12, 13, 23, 123)

descubierto <- c(3, 4)

eph$cubierto <- NA
eph$cubierto[eph$CH08 %in% descubierto] <- 0
eph$cubierto[eph$CH08 %in% cubierto] <- 1

# We build age categories

eph$edad_cat <- NA
eph$edad_cat[eph$CH06 < 18] <- 1
eph$edad_cat[eph$CH06 %in% 18:49] <- 2
eph$edad_cat[eph$CH06 > 49] <- 3

# Hours worked in Main and Secondary Occupations Last week

'documentation: "The codes 9, 99, 999, 9999 correspond, unless otherwise indicated,
to the category Does not know / Does not respond. "'

eph$PP3E_TOT[eph$PP3E_TOT == 999] <- NA
eph$PP3F_TOT[eph$PP3F_TOT == 999] <- NA
eph$horas_trab <- eph$PP3E_TOT + eph$PP3F_TOT
eph$horas_trab[eph$horas_trab == 0] <- NA

eph$horas_trab_pea <- eph$horas_trab
eph$horas_trab_pea[eph$ESTADO == 2] <- 0

# Main occupation income (P21) as a proportion of total income (P47T)

'documentation: "the amounts of income, in which case the non-response will be
Identify with the code -9. "'

eph$P21[eph$P21 == -9] <- NA
eph$P47T[eph$P47T == -9] <- NA
eph$ingppal_tot <- eph$P21 / eph$P47T

# Labor income per hour (T_VI: non-labor income)

eph$T_VI[eph$T_VI == -9] <- NA
eph$inglab <- eph$P47T - eph$T_VI

#We transform nominal amounts to real amounts: base August 2019

'In the questionnaire the question about income is asked about monthly income.
As we work in quarters 3 (months 7, 8 and 9), we consider the month of August
as the benchmark for the quarter.
According to the report https://www.indec.gob.ar/uploads/informesdeprensa/ipc_09_20D39002C437.pdf,
the variation August 2020-August 2019 was 40.7% '

eph$inglab[eph$time == "T3_2020"] <- eph$inglab[eph$time == "T3_2020"] / (1.407)

'When doing the calculation, we consider that the hours you work are measured in weeks and the income
work are measured per month '

eph$horas_trab_mes <- eph$horas_trab*4

eph$inglab_hora <- eph$inglab / (eph$horas_trab*4)

# Number of Occupations

'we change the 0 to 1 because the distribution does not present 1s
According to documentation, the code 0 identifies the cases for which
the sequence analyzed does not correspond to them. '

eph$PP03D[eph$ESTADO != 1] <- NA
eph$PP03D[eph$ESTADO == 1 & eph$PP03D == 0] <- 1

# MERCOSUR activities classifier

eph$clf_act <- NA
#Rest
eph$clf_act[is.na(eph$PP04B_COD) == FALSE] <- 8
#Services of the public administration and public provision of services to the community
eph$clf_act[eph$PP04B_COD == 8401] <- 1
#Trade of food, beverages and tobacco; Food and beverage vending services, except for street vendors
eph$clf_act[eph$PP04B_COD %in% c(4803,5601)] <- 2
#Initial and primary, secondary, tertiary, university and postgraduate education
eph$clf_act[eph$PP04B_COD == 8501] <- 3
# Household activities as employers of domestic staff
eph$clf_act[eph$PP04B_COD == 9700] <- 4
#Human health care activities
eph$clf_act[eph$PP04B_COD == 8600] <- 5
#Construction
eph$clf_act[eph$PP04B_COD == 4000] <- 6
#Trade of merchandise n.c.p. even used merchandise
eph$clf_act[eph$PP04B_COD == 4807] <- 7

# Classifier of activities: related to covid and not related to covid

covid_norel <- c(8401, 8501, 8600)

eph$act_covid <- NA
eph$act_covid[eph$PP04B_COD %in% covid_norel] <- 0
eph$act_covid[eph$PP04B_COD %notin% covid_norel] <- 1

# Social Programs / Unemployment Insurance

'V4_M: Amount of unemployment insurance income
V5_M: Amount of income from subsidy or social aid (in money) from the government, churches, etc. '

ayuda_no <- (eph$V4_M == 0)&(eph$V5_M == 0)

ayuda_si <- (eph$V4_M > 0)|(eph$V5_M > 0)

eph$ayuda_estatal <- NA

eph$ayuda_estatal[ayuda_no] <- 0

eph$ayuda_estatal[ayuda_si] <- 1

# ****************************************************************************************

# USE OF WEIGHTS REPLICATED WITH THE DATABASE FOR USERS
# It has to be done once all the variables to be used for the analysis have been prepared

library(survey)

eph_svy <- svydesign(data = eph,
                     id = ~1,
                     weights = ~PONDERA,
                     strata = ~time)

# ****************************************************************************************

#reports to verify calculations

'https://www.indec.gob.ar/indec/web/Institucional-Indec-InformesTecnicos-58'

# ****************************************************************************************

# Function to build "average tables"

'Function that builds a table, where the rows refer to categories of a variable
independent and the columns refer to a dependent variable.

For the category of independent variable there are two rows: one with the average and one with the
standard deviation of the dependent variable for that category of the independent one.

The number of columns is determined by the number of categories of the dependent variable
and the number of databases whose data are to be compared. For example, the dependent variable
STATE has 5 categories, if we want to compare the base of III19 vs III20, then they will
have 1 + (5 * 2) columns (one more for the categories of the independent variable.)

Note: the varind must be numeric, not factor, if it doesnt throw error.'

tabla <- function(base, #survey designed base
                   periodo1, #Comparison period 1
                   periodo2, #Comparison period 2
                   vardep, #dependent variable, string
                   varind, #independent variable, string
                   varind_etiq, # independent var categories tag list
                   tgroups_etiq) #list of labels for each base (base0 and base1)
                  
        {
        
        # We transform the strings of vardep and varind into formula format
        
        vardep_frm <- as.formula(paste("~", vardep, sep = ""))
        
        varind_frm <- as.formula(paste("~", varind, sep = ""))
        
        # We calculate the average and standard error for each category of varind,
        # for each period
        
        means_1 <- svyby(vardep_frm, 
                         design = subset(base, time == periodo1), 
                         by = varind_frm, 
                         FUN = svymean, 
                         na.rm.by = TRUE,
                         na.rm.all = TRUE,
                         na.rm = TRUE)
        
        means_2 <- svyby(vardep_frm, 
                         design = subset(base, time == periodo2), 
                         by = varind_frm, 
                         FUN = svymean, 
                         na.rm.by = TRUE,
                         na.rm.all = TRUE,
                         na.rm = TRUE)
        
        # Independent variable categories: coding and sizes
        
        varind_cats <- sort(unique(base$variables[varind])[,1])
        
        varind_numcat <- length(varind_cats)
        
        # We set up auxiliary base with differences between periods
        
        test_results <- data.frame(varind = varind_cats,
                                   diff = NA)
        
        # We put together a formula to exercise the difference of means
        
        test_frm <- as.formula(paste(vardep,"~","time", sep = ""))
        
        # For each category of varind, test of differences between periods
        
        for(i in 1:varind_numcat){
                
                # We take only the rows of the corresponding varind category
                
                logical_aux <- base$variables[varind] == varind_cats[i]
                
                #We perform the test
                test <- summary(svyglm(test_frm, design = subset(base, logical_aux)))
                
                # We extract difference, standard error and p-value
                test_beta <- round(test$coefficients[[2]], 2)
                test_se <- test$coefficients[[4]]
                test_pvalue <- test$coefficients[[8]]
                
                #We arm a symbol of statistical significance based on the p-value
                simbolo <- ""
                
                if(test_pvalue < 0.01){
                        simbolo <- "***"
                } else{
                        if(test_pvalue < 0.05){
                                simbolo <- "**"
                        } else{
                                if(test_pvalue < 0.10){
                                        simbolo <- "***"
                                } else{
                                        simbolo <- ""
                                }
                        }
                }
                
                #Complete the data frame created above with the test results
                test_results[i, 2] <- paste(test_beta, simbolo, sep = "")
                
                test_results[i, 3] <- test_se
        }
        
        # We build a final base by gathering the data from the calculation of means, standard errors,
        # differences and statistical significance
        
        base_final <- data.frame(varind = sort(rep(varind_cats, 2)),
                                 diff = NA)
        
        for(i in 1:varind_numcat){
                base_final[2*i-1, 2] <- means_1[i, 2]
                
                base_final[2*i, 2] <- means_1[i, 3]
                
                base_final[2*i-1, 3] <- means_2[i, 2]
                
                base_final[2*i, 3] <- means_2[i, 3]
                
                base_final[2*i-1, 4] <- test_results[i, 2]
                
                base_final[2*i, 4] <- test_results[i, 3]
        }
        
        # Name of columns of the final base
        
        colnames(base_final) <- c(varind_etiq, tgroups_etiq, "Diferencia entre períodos")
        
        return(base_final)
}

# ****************************************************************************************


tabla_ratio <- function(base, #base diseniada
                        numerador, #numerador de ratio a estimar
                        denominador, #denominador de ratio a estimar
                        varind, #variable independiente, string
                        varind_etiq) #etiqueta de nombre de variable
        {
        # Transformamos a formato formula los strings de numerador, denominador y varind
        
        numer_frm <- as.formula(paste("~", numerador, sep = ""))
        
        denom_frm <- as.formula(paste("~", denominador, sep = ""))
        
        varind_frm <- as.formula(paste("~", varind, sep = ""))
        
        # Calculamos el promedio y error estandar para cada categoria de varind, 
        # para cada periodo
        
        logical_1 <- (is.na(base$variables[numerador][,1]) == FALSE)&
                (is.na(base$variables[denominador][,1]) == FALSE)&
                (is.na(base$variables[varind][,1]) == FALSE)
        
        base_final <- data.frame()
        
        # Categorias de variable independiente: codificacion y tamanios
        
        varind_cats <- sort(unique(base$variables[varind])[,1])
        
        varind_numcat <- length(varind_cats)
        
        for(i in 1:varind_numcat){
                
                logical_2 <- logical_1 & (base$variables[varind][,1] == varind_cats[i])
                
                ratios_1 <- svyby(numer_frm,
                                  by = ~time,
                                  denominator = denom_frm,
                                  design = subset(base, logical_2),
                                  svyratio,
                                  covmat = TRUE)
                
                diferencia <- svycontrast(ratios_1, c( -1, 1))
                
                ci_low_90 <- confint(diferencia, level = 0.90)[1]
                ci_upp_90 <- confint(diferencia, level = 0.90)[2]
                ci_low_95 <- confint(diferencia, level = 0.95)[1]
                ci_upp_95 <- confint(diferencia, level = 0.95)[2]
                ci_low_99 <- confint(diferencia, level = 0.99)[1]
                ci_upp_99 <- confint(diferencia, level = 0.99)[2]
                
                if((ci_low_99 < 0 & 0 < ci_upp_99) == FALSE){
                        mensaje <- "***"
                } else {
                        if((ci_low_95 < 0 & 0 < ci_upp_95) == FALSE){
                                mensaje <- "**"
                        } else {
                                if((ci_low_90 < 0 & 0 < ci_upp_90) == FALSE){
                                        mensaje <- "*"
                                } else {mensaje <- ""}
                        }
                }
                
                ratios_2 <- data.table::transpose(ratios_1, 
                                                  make.names = "time")
                ratios_2[,"Diferencia"] <- NA
                
                ratios_2[1,"Diferencia"] <- paste(round(as.numeric(diferencia),2),
                                                  mensaje,
                                                  sep = "")
                
                ratios_2[2,"Diferencia"] <- SE(diferencia)[1]
                
                ratios_2[, varind_etiq] <- varind_cats[i]
                
                ratios_2 <- ratios_2[,c(4,1,2,3)]
                
                base_final <- base_final %>% plyr::rbind.fill(ratios_2)
        }
        
        return(base_final)
}

# ****************************************************************************************

# OCCUPATIONAL STATUS: PROPORTION OF EMPLOYED

# We open excel

wb <- createWorkbook()

# Disaggregated by Sex

base_aux <- tabla(base = eph_svy,
                  periodo1 = "T3_2019",
                  periodo2 = "T3_2020",
                  varind = "CH04",
                  vardep = "ocupado",
                  varind_etiq = "Sexo",
                  tgroups_etiq = c("T3-19", "T3-20"))


addWorksheet(wb, 
             sheetName = "sexo") #we create sheet

writeData(wb,
          "sexo",
          base_aux) #We assign data frame to sheet

# Disaggregated by Age

base_aux <- tabla(base = eph_svy,
                  periodo1 = "T3_2019",
                  periodo2 = "T3_2020", 
                  varind = "edad_cat",
                  vardep = "ocupado",
                  varind_etiq = "Categoría de Edad",
                  tgroups_etiq = c("T3-19", "T3-20"))

addWorksheet(wb, 
             sheetName = "edad") #we create sheet

writeData(wb,
          "edad",
          base_aux) #We assign data frame to sheet

# Disaggregated by Region

base_aux <- tabla(base = eph_svy,
                  periodo1 = "T3_2019",
                  periodo2 = "T3_2020", 
                  varind = "REGION",
                  vardep = "ocupado",
                  varind_etiq = "Región Geográfica",
                  tgroups_etiq = c("T3-19", "T3-20"))

addWorksheet(wb, 
             sheetName = "region") #we create sheet

writeData(wb,
          "region",
          base_aux) #We assign data frame to sheet

#No hacemos por tipo de actividad economica porque todos los que tiene actividad economica
#son del ocupado == Ocupados

# We save excel

saveWorkbook(wb, 
             "doc/output/descriptivos/tablas_eph_proporcion_ocupados.xlsx",
             overwrite = TRUE) # we save excel

# ****************************************************************************************

# SALARY FROM MAIN OCCUPATION AS A PROPORTION OF THE TOTAL INCOME

# We open excel

wb <- createWorkbook()

# Disaggregated by Sex

base_aux <- tabla_ratio(base = eph_svy,
                        numerador = "P21",
                        denominador = "P47T",
                        varind = "CH04",
                        varind_etiq = "Sexo")

addWorksheet(wb, 
             sheetName = "sexo") #we create sheet

writeData(wb,
          "sexo",
          base_aux) #We assign data frame to sheet

# Disaggregated by Age

base_aux <- tabla_ratio(base = eph_svy,
                        numerador = "P21",
                        denominador = "P47T",
                        varind = "edad_cat",
                        varind_etiq = "Categoría de Edad")

addWorksheet(wb, 
             sheetName = "edad") #we create sheet

writeData(wb,
          "edad",
          base_aux) #We assign data frame to sheet

# Disaggregated by Region

base_aux <- tabla_ratio(base = eph_svy,
                        numerador = "P21",
                        denominador = "P47T",
                        varind = "REGION",
                        varind_etiq = "Región Geográfica")

addWorksheet(wb, 
             sheetName = "region") #we create sheet

writeData(wb,
          "region",
          base_aux) #We assign data frame to sheet

# Disaggregated by Economic Activity

base_aux <- tabla_ratio(base = eph_svy,
                        numerador = "P21",
                        denominador = "P47T",
                        varind = "clf_act",
                        varind_etiq = "Rama de Actividad")

addWorksheet(wb, 
             sheetName = "clasificacion_actividad") #we create sheet

writeData(wb,
          "clasificacion_actividad",
          base_aux) #We assign data frame to sheet

# Disaggregated by COVID Classification of Activity 

base_aux <- tabla_ratio(base = eph_svy,
                        numerador = "P21",
                        denominador = "P47T",
                        varind = "act_covid",
                        varind_etiq = "Rama de Actividad - COVID")

addWorksheet(wb, 
             sheetName = "clasificacion_actividad_covid") #we create sheet

writeData(wb,
          "clasificacion_actividad_covid",
          base_aux) #We assign data frame to sheet

# We save excel

saveWorkbook(wb, 
             "doc/output/descriptivos/tablas_eph_proporcion_ingreso_laboral_ppal_sobre_total.xlsx",
             overwrite = TRUE) # we save excel

# ****************************************************************************************

# HOURS WORKED

# We open excel

wb <- createWorkbook()

# Disaggregated by Sex

base_aux <- tabla(base = eph_svy,
                  periodo1 = "T3_2019",
                  periodo2 = "T3_2020", 
                  varind = "CH04",
                  vardep = "horas_trab",
                  varind_etiq = "Sexo",
                  tgroups_etiq = c("T3-19", "T3-20"))

addWorksheet(wb, 
             sheetName = "sexo") #we create sheet

writeData(wb,
          "sexo",
          base_aux) #We assign data frame to sheet

# Disaggregated by Age

base_aux <- tabla(base = eph_svy,
                  periodo1 = "T3_2019",
                  periodo2 = "T3_2020", 
                  varind = "edad_cat",
                  vardep = "horas_trab",
                  varind_etiq = "Categoría de Edad",
                  tgroups_etiq = c("T3-19", "T3-20"))

addWorksheet(wb, 
             sheetName = "edad") #we create sheet

writeData(wb,
          "edad",
          base_aux) #We assign data frame to sheet

# Disaggregated by Region

base_aux <- tabla(base = eph_svy,
                  periodo1 = "T3_2019",
                  periodo2 = "T3_2020", 
                  varind = "REGION",
                  vardep = "horas_trab",
                  varind_etiq = "Región Geográfica",
                  tgroups_etiq = c("T3-19", "T3-20"))

addWorksheet(wb, 
             sheetName = "region") #we create sheet

writeData(wb,
          "region",
          base_aux) #We assign data frame to sheet

# Disaggregated by Economic Activity

base_aux <- tabla(base = eph_svy,
                  periodo1 = "T3_2019",
                  periodo2 = "T3_2020", 
                  varind = "clf_act",
                  vardep = "horas_trab",
                  varind_etiq = "Rama de Actividad",
                  tgroups_etiq = c("T3-19", "T3-20"))

addWorksheet(wb, 
             sheetName = "clasificacion_actividad") #we create sheet

writeData(wb,
          "clasificacion_actividad",
          base_aux) #We assign data frame to sheet

# Disaggregated by COVID Classification of Activity 

base_aux <- tabla(base = eph_svy,
                  periodo1 = "T3_2019",
                  periodo2 = "T3_2020", 
                  varind = "act_covid",
                  vardep = "horas_trab",
                  varind_etiq = "Rama de Actividad-COVID",
                  tgroups_etiq = c("T3-19", "T3-20"))

addWorksheet(wb, 
             sheetName = "clasificacion_actividad_covid") #we create sheet

writeData(wb,
          "clasificacion_actividad_covid",
          base_aux) #We assign data frame to sheet

# We save excel

saveWorkbook(wb, 
             "doc/output/descriptivos/tablas_eph_horas_trabajadas.xlsx",
             overwrite = TRUE) # we save excel

# ****************************************************************************************

# WORKED HOURS PEA

# We open excel

wb <- createWorkbook()

# Disaggregated by Sex

base_aux <- tabla(base = eph_svy,
                  periodo1 = "T3_2019",
                  periodo2 = "T3_2020", 
                  varind = "CH04",
                  vardep = "horas_trab_pea",
                  varind_etiq = "Sexo",
                  tgroups_etiq = c("T3-19", "T3-20"))

addWorksheet(wb, 
             sheetName = "sexo") #we create sheet

writeData(wb,
          "sexo",
          base_aux) #We assign data frame to sheet

# Disaggregated by Age

base_aux <- tabla(base = eph_svy,
                  periodo1 = "T3_2019",
                  periodo2 = "T3_2020", 
                  varind = "edad_cat",
                  vardep = "horas_trab_pea",
                  varind_etiq = "Categoría de Edad",
                  tgroups_etiq = c("T3-19", "T3-20"))

addWorksheet(wb, 
             sheetName = "edad") #we create sheet

writeData(wb,
          "edad",
          base_aux) #We assign data frame to sheet

# Disaggregated by Region

base_aux <- tabla(base = eph_svy,
                  periodo1 = "T3_2019",
                  periodo2 = "T3_2020", 
                  varind = "REGION",
                  vardep = "horas_trab_pea",
                  varind_etiq = "Región Geográfica",
                  tgroups_etiq = c("T3-19", "T3-20"))

addWorksheet(wb, 
             sheetName = "region") #we create sheet

writeData(wb,
          "region",
          base_aux) #We assign data frame to sheet

# Disaggregated by Economic Activity

base_aux <- tabla(base = eph_svy,
                  periodo1 = "T3_2019",
                  periodo2 = "T3_2020", 
                  varind = "clf_act",
                  vardep = "horas_trab_pea",
                  varind_etiq = "Rama de Actividad",
                  tgroups_etiq = c("T3-19", "T3-20"))

addWorksheet(wb, 
             sheetName = "clasificacion_actividad") #we create sheet

writeData(wb,
          "clasificacion_actividad",
          base_aux) #We assign data frame to sheet

# Disaggregated by COVID Classification of Activity 

base_aux <- tabla(base = eph_svy,
                  periodo1 = "T3_2019",
                  periodo2 = "T3_2020", 
                  varind = "act_covid",
                  vardep = "horas_trab_pea",
                  varind_etiq = "Rama de Actividad-COVID",
                  tgroups_etiq = c("T3-19", "T3-20"))

addWorksheet(wb, 
             sheetName = "clasificacion_actividad_covid") #we create sheet

writeData(wb,
          "clasificacion_actividad_covid",
          base_aux) #We assign data frame to sheet

# We save excel

saveWorkbook(wb, 
             "doc/output/descriptivos/tablas_eph_horas_trabajadas_pea.xlsx",
             overwrite = TRUE) # we save excel

# ****************************************************************************************

# WORK INCOME PER HOUR WORKED

# We open excel

wb <- createWorkbook()

# Disaggregated by Sex

base_aux <- tabla_ratio(base = eph_svy,
                  numerador = "inglab",
                  denominador = "horas_trab_mes",
                  varind = "CH04",
                  varind_etiq = "Sexo")

addWorksheet(wb, 
             sheetName = "sexo") #we create sheet

writeData(wb,
          "sexo",
          base_aux) #We assign data frame to sheet

# Disaggregated by Age

base_aux <- tabla_ratio(base = eph_svy,
                  numerador = "inglab",
                  denominador = "horas_trab_mes",
                  varind = "edad_cat",
                  varind_etiq = "Categoría de Edad")

addWorksheet(wb, 
             sheetName = "edad") #we create sheet

writeData(wb,
          "edad",
          base_aux) #We assign data frame to sheet

# Disaggregated by Region

base_aux <- tabla_ratio(base = eph_svy,
                        numerador = "inglab",
                        denominador = "horas_trab_mes",
                        varind = "REGION",
                        varind_etiq = "Región Geográfica")

addWorksheet(wb, 
             sheetName = "region") #we create sheet

writeData(wb,
          "region",
          base_aux) #We assign data frame to sheet

# Disaggregated by Economic Activity

base_aux <- tabla_ratio(base = eph_svy,
                        numerador = "inglab",
                        denominador = "horas_trab_mes",
                        varind = "clf_act",
                        varind_etiq = "Rama de Actividad")

addWorksheet(wb, 
             sheetName = "clasificacion_actividad") #we create sheet

writeData(wb,
          "clasificacion_actividad",
          base_aux) #We assign data frame to sheet

# Disaggregated by COVID Classification of Activity 

base_aux <- tabla_ratio(base = eph_svy,
                        numerador = "inglab",
                        denominador = "horas_trab_mes",
                        varind = "act_covid",
                        varind_etiq = "Rama de Actividad - COVID")

addWorksheet(wb, 
             sheetName = "clasificacion_actividad_covid") #we create sheet

writeData(wb,
          "clasificacion_actividad_covid",
          base_aux) #We assign data frame to sheet

# We save excel

saveWorkbook(wb, 
             "doc/output/descriptivos/tablas_eph_ingreso_laboral_horario.xlsx",
             overwrite = TRUE) # we save excel

# ****************************************************************************************

# POSSESSION OF ANY HEALTH COVERAGE

# We open excel

wb <- createWorkbook()

# Disaggregated by Sex

base_aux <- tabla(base = eph_svy,
                  periodo1 = "T3_2019",
                  periodo2 = "T3_2020", 
                  varind = "CH04",
                  vardep = "cubierto",
                  varind_etiq = "Sexo",
                  tgroups_etiq = c("T3-19", "T3-20"))

addWorksheet(wb, 
             sheetName = "sexo") #we create sheet

writeData(wb,
          "sexo",
          base_aux) #We assign data frame to sheet

# Disaggregated by Age

base_aux <- tabla(base = eph_svy,
                  periodo1 = "T3_2019",
                  periodo2 = "T3_2020", 
                  varind = "edad_cat",
                  vardep = "cubierto",
                  varind_etiq = "Categoría de Edad",
                  tgroups_etiq = c("T3-19", "T3-20"))

addWorksheet(wb, 
             sheetName = "edad") #we create sheet

writeData(wb,
          "edad",
          base_aux) #We assign data frame to sheet

# Disaggregated by Region

base_aux <- tabla(base = eph_svy,
                  periodo1 = "T3_2019",
                  periodo2 = "T3_2020", 
                  varind = "REGION",
                  vardep = "cubierto",
                  varind_etiq = "Región Geográfica",
                  tgroups_etiq = c("T3-19", "T3-20"))

addWorksheet(wb, 
             sheetName = "region") #we create sheet

writeData(wb,
          "region",
          base_aux) #We assign data frame to sheet

# Disaggregated by Economic Activity

base_aux <- tabla(base = eph_svy,
                  periodo1 = "T3_2019",
                  periodo2 = "T3_2020", 
                  varind = "clf_act",
                  vardep = "cubierto",
                  varind_etiq = "Rama de Actividad",
                  tgroups_etiq = c("T3-19", "T3-20"))

addWorksheet(wb, 
             sheetName = "clasificacion_actividad") #we create sheet

writeData(wb,
          "clasificacion_actividad",
          base_aux) #We assign data frame to sheet

# Disaggregated by COVID Classification of Activity 

base_aux <- tabla(base = eph_svy,
                  periodo1 = "T3_2019",
                  periodo2 = "T3_2020", 
                  varind = "act_covid",
                  vardep = "cubierto",
                  varind_etiq = "Rama de Actividad-COVID",
                  tgroups_etiq = c("T3-19", "T3-20"))

addWorksheet(wb, 
             sheetName = "clasificacion_actividad_covid") #we create sheet

writeData(wb,
          "clasificacion_actividad_covid",
          base_aux) #We assign data frame to sheet

# We save excel

saveWorkbook(wb, 
             "doc/output/descriptivos/tablas_eph_cobertura_salud.xlsx",
             overwrite = TRUE) # we save excel

# ****************************************************************************************

# RECEIVE UNEMPLOYMENT INSURANCE OR SOCIAL PROGRAM

# We open excel

wb <- createWorkbook()

# Disaggregated by Sex

base_aux <- tabla(base = eph_svy,
                  periodo1 = "T3_2019",
                  periodo2 = "T3_2020", 
                  varind = "CH04",
                  vardep = "ayuda_estatal",
                  varind_etiq = "Sexo",
                  tgroups_etiq = c("T3-19", "T3-20"))

addWorksheet(wb, 
             sheetName = "sexo") #we create sheet

writeData(wb,
          "sexo",
          base_aux) #We assign data frame to sheet

# Disaggregated by Age

base_aux <- tabla(base = eph_svy,
                  periodo1 = "T3_2019",
                  periodo2 = "T3_2020", 
                  varind = "edad_cat",
                  vardep = "ayuda_estatal",
                  varind_etiq = "Categoría de Edad",
                  tgroups_etiq = c("T3-19", "T3-20"))

addWorksheet(wb, 
             sheetName = "edad") #we create sheet

writeData(wb,
          "edad",
          base_aux) #We assign data frame to sheet

# Disaggregated by Region

base_aux <- tabla(base = eph_svy,
                  periodo1 = "T3_2019",
                  periodo2 = "T3_2020", 
                  varind = "REGION",
                  vardep = "ayuda_estatal",
                  varind_etiq = "Región Geográfica",
                  tgroups_etiq = c("T3-19", "T3-20"))

addWorksheet(wb, 
             sheetName = "region") #we create sheet

writeData(wb,
          "region",
          base_aux) #We assign data frame to sheet

# Disaggregated by Economic Activity

base_aux <- tabla(base = eph_svy,
                  periodo1 = "T3_2019",
                  periodo2 = "T3_2020", 
                  varind = "clf_act",
                  vardep = "ayuda_estatal",
                  varind_etiq = "Rama de Actividad",
                  tgroups_etiq = c("T3-19", "T3-20"))

addWorksheet(wb, 
             sheetName = "clasificacion_actividad") #we create sheet

writeData(wb,
          "clasificacion_actividad",
          base_aux) #We assign data frame to sheet

# Disaggregated by COVID Classification of Activity

base_aux <- tabla(base = eph_svy,
                  periodo1 = "T3_2019",
                  periodo2 = "T3_2020", 
                  varind = "act_covid",
                  vardep = "ayuda_estatal",
                  varind_etiq = "Rama de Actividad-COVID",
                  tgroups_etiq = c("T3-19", "T3-20"))

addWorksheet(wb, 
             sheetName = "clasificacion_actividad_covid") #we create sheet

writeData(wb,
          "clasificacion_actividad_covid",
          base_aux) #We assign data frame to sheet

# We save excel

saveWorkbook(wb, 
             "doc/output/descriptivos/tablas_eph_ayuda_estatal.xlsx",
             overwrite = TRUE) # we save excel

# ****************************************************************************************