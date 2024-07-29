require(rio)
require(dplyr)
require(stringr)
require(tidyr)

source('./tidy_funcs.R')

#define demographics
feadir <- 'data'

feapath <- file.path(feadir,"DATA_ses-01_2024-06-02.csv")
dfea <- import(feapath)
dfea <- dfea %>% mutate(ID = str_extract(BASIC_INFO_ID, '\\d+'))

fses01path <- file.path(feadir,"motor_summary_ses-01.csv")
fses02path <- file.path(feadir,"motor_summary_ses-02.csv")

dmotor01 <- import(fses01path)
dmotor01 <- dmotor01 %>% mutate(ID = str_extract(ID, '\\d+'))
dmotor02 <- import(fses02path)
dmotor02 <- dmotor02 %>% 
  select(ID, AGE, SEX, 
         starts_with("GFORCE"), 
         starts_with("BILPRESS"), 
         starts_with("GOFITTS"), 
         starts_with("ST")) %>%
  mutate(ID = str_extract(ID, '\\d+'),
         ses = 2) 

dmotor01 <- dmotor01 %>%
  filter(ID %in% unique(dmotor02$ID)) %>%
  select(ID, AGE, SEX, 
         starts_with("GFORCE"), 
         starts_with("BILPRESS"), 
         starts_with("GOFITTS"), 
         starts_with("ST")) %>%
  mutate(ses = 1)

# listing mismatch column names
list_mismatch_coln(dmotor01, dmotor02)

# dmotor02 has fewer columns, select dmotor01 columns that also exist in dmotor2
dmotor01 <- dmotor01 %>%
  select(one_of(colnames(dmotor02)))

dmotor_all <- bind_rows(dmotor01, dmotor02)

# Assuming dmotor_all is your tibble
dmotor_all <- dmotor_all %>%
  separate(ST_RAW_BPrest, 
           into = c("ST_RAW_Systolic", "ST_RAW_Diastolic"), 
           sep = "/") %>%
  mutate(ST_RAW_Systolic = as.numeric(ST_RAW_Systolic),
         ST_RAW_Diastolic = as.numeric(ST_RAW_Diastolic))


save(list=c("dmotor_all"), file = 'proc_data/dmotor_all.RData')



