##############
#### AMGv2 basic R version
##############

##############################################################################################

# R script for running AMGv2
# This script corresponds to the AMGv2 model presented in Clivot et al. (2019)
# The following options are chosen for the input variables:
    # C inputs calculated by the Bolinder method (Bolinder et al., 2017)
    # Fixed values of pH and soil C:N ratio
# This "basic" version does not include the calculation of C3 and C4 stocks.

##############################################################################################

# INSTRUCTIONS
    # The computation is done in 5 steps. The user can specify the working directory.
    # All the inputs and the parameter files (.csv files) must be located in the "inputs" directory.
    # The structure and the names of the input and the parameter files SHOULD NOT BE CHANGED. Do not add columns with the same names as the existing ones.
    # The output files are saved in the "outputs" directory.

##############################################################################################

# Load libraries --------------------------------------------------------------

library(readr)
library(dplyr)
library(tidyr)
library(stringr)


# Define working directory ----------------------------------------------------

# REP <- "C:/...." # To be specified by the user
# setwd(REP) 


# STEP 1: Import input data and parameters ------------------------------------

# Crop management data
MANAG_DB <- read_delim("./inputs/inputs_MANAGEMENT.csv", delim = ";", locale = readr::locale(encoding = "latin1")) #Database
MANAG <- select(MANAG_DB, Trial:Irrigation) #Input data

# Soil data
SOIL_DB <- read_delim("./inputs/inputs_SOIL.csv", delim = ";", locale = readr::locale(encoding = "latin1")) #Database
SOIL <- select(SOIL_DB, Trial:Initial_SOC_stock) #Input data

# Meteorological data
METEO_DB <- read_delim("./inputs/inputs_METEO.csv", delim = ";", locale = readr::locale(encoding = "latin1")) #Database
METEO <- select(METEO_DB, Trial:PET) #Input data

# Plant C input parameters (Bolinder coefficients)
Plant_Param_DB <- read_delim("./inputs/parameters_PLANT.csv", delim = ";", locale = readr::locale(encoding = "latin1")) #Database
Plant_Param <- select(Plant_Param_DB, Crop_type:C_conc_BG) #Parameters

# Exoegous organic matter (EOM) parameters
EOM_Param_DB <- read_delim("./inputs/parameters_EOM.csv", delim = ";", locale = readr::locale(encoding = "latin1")) #Database
EOM_Param <- select(EOM_Param_DB, EOM_name:h_EOM) #Parameters

# Mineralization parameters
Mineralization_Param_DB <- read_delim("./inputs/parameters_MINERALIZATION.csv", delim = ";", locale = readr::locale(encoding = "latin1"))
Mineralization_Param <- select(Mineralization_Param_DB, Parameter:Value) #Parameters

rm(MANAG_DB, SOIL_DB, METEO_DB, Plant_Param_DB, EOM_Param_DB, Mineralization_Param_DB)


# STEP 2: Check input data ------------------------------------------------

if(!all(unique(paste(MANAG$Trial, MANAG$Treatment)) %in% unique(paste(SOIL$Trial, SOIL$Treatment)))) { warning("Missing Trial or Treatment in SOIL") }

if(!all(unique(paste(MANAG$Trial, MANAG$Year)) %in% unique(paste(METEO$Trial, METEO$Year)))) { warning("Missing Trial or Year in METEO") }

if(!all(unique(paste(MANAG$Crop_type, MANAG$Crop_name)) %in% unique(paste(Plant_Param$Crop_type, Plant_Param$Crop_name)))) { warning("Missing Crop in parameters_PLANT") }

if(!all(unique(c(MANAG$EOM_name_1, MANAG$EOM_name_2, MANAG$EOM_name_3)) %in% unique(c(EOM_Param$EOM_name, NA)))) { warning("Missing EOM in parameters_EOM") }


# STEP 3: Calculation of C inputs ------------------------------------------

## 3.1: C inputs from crops -----------------------------------------------------

C_inputs_PLT_data <- select(MANAG, c(Trial, Treatment, Year, Crop_type, Crop_name, Yield, Hum, Residues, Tillage_depth)) %>%
  mutate(Yield_DW = Yield*(1-Hum/100)) # Dry yield

C_PLT <- left_join(C_inputs_PLT_data, Plant_Param) %>%
  mutate(
    PSS = ifelse((Residues == "Returned" | is.na(Residues) == TRUE), 1, PSS), # Forcing the value of the fraction of CSS (PSS) to 1 when all crop residues are returned to the soil
    CP = ifelse(Crop_type == "CC", 0, Yield_DW * C_conc_AG), # C in the harvested organ
    CSS = ifelse(Crop_type == "CC", Yield_DW * C_conc_AG, Yield_DW * ((1 - HI) / HI) * C_conc_AG), # C in straw and stubble
    CR = ifelse(Crop_type == "CC", (Yield_DW / SR) * C_conc_BG, Yield_DW / (SR * HI) * C_conc_BG), # C in roots
    CE = CR * 0.65 # Extra-root C (Root exudates)
  )

Calculation_depth <- select(SOIL, c(Trial, Treatment, Considered_depth)) # SOC calculation depth

C_inputs_PLT <- left_join(C_PLT, Calculation_depth) %>%
  group_by(Trial, Treatment, Year, Crop_type, Crop_name) %>%
  mutate(
    CR_considered_depth = (1 - Beta^Considered_depth) * CR, # C from roots in the considered depth
    CE_considered_depth = CR_considered_depth * 0.65, # Extra-root C in the considered depth
    CAG = PSS * CSS, # Aboveground C inputs
    CBG = sum(CR_considered_depth, CE_considered_depth, na.rm = TRUE), # Belowground C inputs
    CPLT = sum(CAG, CBG, na.rm = TRUE), # Total C from plant
    CAG_hum = CAG * h_AG, # Humified C from aboveground
    CBG_hum = CBG * h_BG, # Humified C from belowground
    CPLT_hum = sum(CAG_hum, CBG_hum, na.rm = TRUE) # Total humified C from plant
  ) %>%
  ungroup()

rm(C_inputs_PLT_data, C_PLT, Calculation_depth)


## 3.2: C inputs from exogenous organic matter (EOM) ----------------------------

C_inputs_EOM_type_lines <- select(MANAG, c(Trial, Treatment, Year, Crop_type, Crop_name, EOM_name_1, EOM_name_2, EOM_name_3)) %>%
  pivot_longer(cols = c(EOM_name_1, EOM_name_2, EOM_name_3), names_to = "EOM_n", values_to = "EOM_name", values_drop_na = FALSE) %>%
  mutate(EOM_n = str_remove(EOM_n, "_name"))

C_inputs_EOM_dose_lines <- select(MANAG, c(Trial, Treatment, Year, Crop_type, Crop_name, EOM_dose_1, EOM_dose_2, EOM_dose_3)) %>%
  pivot_longer(cols = c(EOM_dose_1, EOM_dose_2, EOM_dose_3), names_to = "EOM_n", values_to = "EOM_dose", values_drop_na = FALSE) %>%
  mutate(EOM_n = str_remove(EOM_n, "_dose"))

C_inputs_EOM_data_lines <- left_join(C_inputs_EOM_type_lines, C_inputs_EOM_dose_lines)

rm(C_inputs_EOM_type_lines, C_inputs_EOM_dose_lines)

# Calculation of C inputs from EOM

C_inputs_EOM_lines <- left_join(C_inputs_EOM_data_lines, EOM_Param) %>%
  mutate(
    C_EOM = EOM_dose * C_conc_EOM /1000, # C inputs from EOM in tC.ha-1
    C_hum_EOM = C_EOM * h_EOM, # Humified C from EOM in tC.ha-1
  )

C_inputs_EOM <- C_inputs_EOM_lines %>%
  pivot_wider(names_from = EOM_n, values_from = c(EOM_name:C_hum_EOM)) %>%
  rename_with(~ str_replace(string = .x, pattern = "_EOM_", replacement = "_")) %>%
  group_by(Trial, Treatment, Year, Crop_type, Crop_name) %>%
  mutate(
    CEOM = sum(C_EOM_1, C_EOM_2, C_EOM_3, na.rm = TRUE), # C inputs from all EOMs in tC.ha-1
    CEOM_hum = sum(C_hum_EOM_1, C_hum_EOM_2, C_hum_EOM_3, na.rm = TRUE) # Humified C from all EOMs in tC.ha-1
    ) %>%
  ungroup()

rm(C_inputs_EOM_data_lines, C_inputs_EOM_lines)

## 3.3: Preparation of the C input table ----------------------------------------

Data_C_input <- left_join(
    select(C_inputs_PLT, c(Trial, Treatment, Year, Crop_type, Crop_name, Considered_depth, CAG, CBG, CPLT, CAG_hum, CBG_hum, CPLT_hum)),
    select(C_inputs_EOM ,c(Trial, Treatment, Year, Crop_type, Crop_name, CEOM, CEOM_hum))
    ) %>%
  group_by(Trial, Treatment, Year, Crop_type, Crop_name) %>%
  mutate(
    CTOT = sum(CPLT, CEOM, na.rm = TRUE), # Total C inputs (Plant + EOM) in tC.ha-1
    CTOT_hum = sum(CPLT_hum, CEOM_hum, na.rm = TRUE) # Total humified C (Plant + EOM) in tC.ha-1
  ) %>%
  arrange(Trial, Treatment, Year)


# STEP 4: Calculation of the simulated SOC stock ------------------------------
    
## 4.1: Preparation of the SOC stock table -------------------------------

# Annual tillage table
Data_Tillage_year <- select(MANAG, c(Trial, Treatment, Year, Tillage_depth)) %>%
  group_by(Trial, Treatment, Year) %>%
  summarise(Tillage_depth_year = max(Tillage_depth, na.rm = TRUE)) %>% # Maximum Tillage depth for each year
  ungroup()

# Annual irrigation table
Data_Irr_year <- select(MANAG, c(Trial, Treatment, Year, Irrigation)) %>%
  group_by(Trial, Treatment, Year) %>%
  summarise(Irrigation_year = sum(Irrigation, na.rm = TRUE)) %>% # Annual irrigation
  ungroup()

# Annual C humified table
Data_C_hum_year <- select(Data_C_input, c(Trial, Treatment, Year, CTOT_hum)) %>%
  group_by(Trial, Treatment, Year) %>%
  summarise(CTOT_hum_year = sum(CTOT_hum, na.rm = TRUE)) %>% # Annual humified C
  ungroup()

# SOC stock table
Data_Stock <- select(MANAG, c(Trial, Treatment, Year, Crop_name, Crop_type)) %>%
  mutate(Crop_type = paste0("Crop_name_", Crop_type)) %>%
  pivot_wider(names_from = Crop_type, values_from = Crop_name) %>%
  left_join(METEO) %>%
  left_join(select(SOIL, c(Trial, Treatment, Clay, CaCO3, pH, CN_ratio, Considered_depth, Initial_SOC_stock))) %>%
  left_join(Data_Tillage_year) %>%
  left_join(Data_Irr_year) %>%
  left_join(Data_C_hum_year) %>%
  arrange(Trial, Treatment, Year) %>%
  group_by(Trial, Treatment) %>%
  mutate(
    Time = 1:n(),
    Time = Time - 1) %>%
  ungroup() %>%
  select(Trial, Treatment, Year, Time, everything())

rm(Data_Tillage_year, Data_Irr_year, Data_C_hum_year)


## 4.2: Calculation of the mineralization functions ------------------

# Retrieve values of mineralization parameters
for (i in 1:nrow(Mineralization_Param)) {
  assign(Mineralization_Param[[i,c("Parameter")]], Mineralization_Param[[i,c("Value")]])
}
rm(i)

# Calculation

Data_Stock <- Data_Stock %>%
  mutate(
    fT = ifelse(T_mean <= 0, 1.00E-06, round(aT /(1 + (((aT - 1) * exp(cT * Tref)) * exp(-cT * round(T_mean,1)))), 6)), # Temperature function
    fH = round(1 /(1 + (aH * exp(-bH * ((round(Rainfall + Irrigation_year - PET, 0)) /1000)))), 6), # Humidity function
    fA = round(exp(-am * Clay), 6), # Clay function
    fC = round(1 /(1 + (cm * CaCO3)), 6), # CaCO3 function
    fpH = round(exp(-apH * ((pH - bpH)^2)), 6), # pH function
    fCN = round(0.8 * exp(-aCN * ((CN_ratio - bCN)^2)) + 0.2, 6), # C:N ratio function
    k = round(k0 * fT * fH * fA * fC * fpH * fCN, 7) # k AMGv2
  )


## 4.3: Calculation of simulated SOC stock -------------------------------

Data_Stock <- Data_Stock %>%
  mutate(
    QC = ifelse(Time == 0, round(Initial_SOC_stock, 4), NA), # Simulated initial total SOC stock
    QCS = round(Initial_SOC_stock * Ps, 4), # Simulated stable SOC stock (= initial SOC stock * Cs/C0)
    QCA = QC - QCS # Simulated initial active SOC stock (= (1-Ps) * initial SOC)
  )

# Simulated active and total SOC pools
for (i in 1:nrow(Data_Stock)) {
  
  if (is.na(Data_Stock[i, "QC"]) == TRUE) {
    
    Data_Stock[i, "QCA"] <- round(((Data_Stock[i-1, "QC"] - Data_Stock[i, "QCS"]) * exp(-Data_Stock[i, "k"])) + ((Data_Stock[i-1, "CTOT_hum_year"] / Data_Stock[i, "k"]) * (1-exp(-Data_Stock[i, "k"]))), 4)
    
    Data_Stock[i, "QC"] <- round(Data_Stock[i, "QCS"] + Data_Stock[i, "QCA"], 4)
    
  }
  
}
rm(i)


# STEP 5: Export results ------------------------------------------------------

# C inputs
write_delim(Data_C_input,"./outputs/outputs_C_SUPPLY.csv", delim = ";")

# Simulated SOC stocks
write_delim(Data_Stock,"./outputs/outputs_SOC_STOCKS.csv", delim = ";")

