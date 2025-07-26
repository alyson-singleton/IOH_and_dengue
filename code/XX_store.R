## load raw mdd case data (all diseases)
#case_data <- read.csv("~/Desktop/doctorate/ch2 mdd highway/data/mdd_case_data.csv")
case_data <- read.csv("~/Desktop/doctorate/ch2 mdd highway/data/Datos vigilancia MDD 2000-2022_SubsetStanford.csv")
head(case_data)

#################################
### preprocess dengue data
#################################
## keep dengue data only (in this case do not include A97.1--alarm--or A97.2--severe
##  because neither were reported before paving)
dengue_data <- case_data[which(case_data$DIAGNOSTIC %in% c("A97.0","A97.1","A97.2")),]
#dengue_data <- case_data[which(case_data$DIAGNOSTIC %in% c("B55.1","B55.2")),]
table(dengue_data$TIPO_DX)

## remove "descartado" cases (options are D/C/P)
#dengue_data <- dengue_data[which(dengue_data$TIPO_DX=="C" | dengue_data$TIPO_DX=="P"),]
#dengue_data <- dengue_data[which(dengue_data$TIPO_DX=="C"),]

## remove cases listed as being from outside madre de dios
#mdd_districts <- unique(dengue_data$UBIGEO)[1:11]
#dengue_data <- dengue_data[which(dengue_data$UBIGEO %in% mdd_districts),]

## group into months
dengue_data$SEMANA <- ifelse(dengue_data$SEMANA==53,52,dengue_data$SEMANA)
dengue_data <- dengue_data %>%
  mutate(month = lubridate::month(as.Date(paste0(ANO, "-", SEMANA, "-", 1), format = "%Y-%U-%u")))
dengue_data$month_date <- as.Date(paste0(dengue_data$ANO, "-", dengue_data$month, "-", 01), format = "%Y-%m-%d")

monthly_dengue_data <- dengue_data %>%
  group_by(month = month_date, 
           e_salud = E_SALUD, 
           dx_type = TIPO_DX,
           dx_code = DIAGNOSTIC,
           UBIGEO = UBIGEO) %>%
  summarize(dengue_cases = n()) %>%
  dplyr::select(month, UBIGEO, e_salud, dx_code, dx_type, dengue_cases)

write.csv(monthly_dengue_data, "data/diresa_data/diresa_dengue_data.csv", row.names = FALSE)
