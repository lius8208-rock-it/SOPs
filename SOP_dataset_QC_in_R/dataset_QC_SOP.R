library(tidyverse)
library(skimr)
library(naniar)
library(lubridate)

NKA_IP <- read.csv("/Users/Sherry/Desktop/Documents/in_Canada/study/NKA/NKA_for_2017/NKA_2017_FF/NKA_IP_2017_FF.csv", sep = "\t")
head(NKA_IP)
##Inspect file structure
glimpse(NKA_IP)
skim(NKA_IP)
NKA_IP <- read.delim("/Users/Sherry/Desktop/Documents/in_Canada/study/NKA/NKA_for_2017/NKA_2017_FF/NKA_IP_2017_FF.csv", sep="\t") %>%
  # Separate the combined first column into multiple columns
  separate(col = 1, into = c("Region", "Pop", "NKAIP"), 
           sep = ",", convert = TRUE) %>%
  # Clean up any whitespace
  mutate(across(everything(), ~trimws(.)))
glimpse(NKA_IP)
##Check missing value
NKA_IP %>% summarise_all(~sum(is.na(.)))
vis_miss(NKA_IP)  
##Validate data types
sapply(NKA_IP, class)
##Check duplicates in unique IDs
sum(duplicated(NKA_IP$Pop))
##factor levels check
NKA_IP %>% count(Region)
unique(NKA_IP$Region)
#	Identify outliers for numeric fields
NKA_IP <- NKA_IP %>%
  mutate(NKAIP = as.numeric(NKAIP)) ##NKA_IP is character as indicated from above command
NKA_IP %>% 
  summarise(across(where(is.numeric), 
                   list(min = min, max = max, mean = mean, sd = sd), 
                   na.rm = TRUE))
#	Date validation
NKA_IP %>% summarise(min_date = min(NKAIP, na.rm = TRUE),
                   max_date = max(NKAIP, na.rm = TRUE))
#Create a summary QC report

qc_summary <- NKA_IP %>%
  summarise(
    total_rows = n(),
    missing_any = sum(!complete.cases(.)),
    duplicates = sum(duplicated(Pop)),
    mean_NKAIP = mean(NKAIP, na.rm = TRUE),
  )

write_csv(qc_summary, "/Users/Sherry/Desktop/Documents/in_Canada/study/NKA/NKA_for_2017/NKA_2017_FF/QC_summary.csv")
