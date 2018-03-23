#Loading Libraries

library(curl)
library(tidyr)
library(dplyr)
library(stringr)
library(tm)
library(pdftools)
library(countrycode)

setwd("E:/PGDDA/case study 1/Data")

################################################################################
###########              Check Point 1 : Data Cleaning             ############
################################################################################
#1Load the companies and rounds data (provided on the previous page) into two data frames
#and name them companies and rounds2 respectively.

companies <-
  read.csv(
    "companies.txt",
    header = TRUE,
    sep = "\t",
    quote = "\"",
    stringsAsFactors = F,
    na.strings = c("", "NA")
  )

rounds2 <-
  read.csv("rounds2.csv",
           stringsAsFactors = F,
           na.strings = c("", "NA")) #114949 obs

#How many unique companies are present in rounds2?
length(unique(tolower(rounds2$company_permalink))) #66368

#How many unique companies are present in companies?
length(unique(tolower(companies$permalink))) #66368

#In the companies data frame, which column can be used as the unique key for each company?
#Write the name of the column.
find_primary <- function(x) {
  if (!anyNA(x) && (length(which(duplicated(x) == T)) == 0)) {
    return(T)
  }
  return(F)
}
names(companies)[sapply(companies, find_primary)]
#permalink

#Are there any companies in the rounds2 file which are not present in companies?
#Answer yes or no: Y/N

#change to lower case
companies$permalink <- tolower(companies$permalink)
rounds2$company_permalink <- tolower(rounds2$company_permalink)

length(which(
  !unique(companies$permalink) %in% unique(rounds2$company_permalink)
))
#No

#Merge the two data frames so that all variables (columns) in the companies frame are added to the rounds2 data frame. Name the merged frame master_frame.
#How many observations are present in master_frame?

master_frame <-
  merge (rounds2,
         companies,
         by.x = "company_permalink",
         by.y = "permalink",
         all.x = TRUE)
write.csv(master_frame, "master_frame.csv")

#observations
dim(master_frame)
#114949     15

################################################################################
###########              Checkpoint 2: Funding Type Analysis       ############
################################################################################

#Calculate the average investment amount for each of the four funding types
#(venture, angel, seed, and private equity) and report the answers

funding_round_types <-
  c("venture", "angel", "seed", "private_equity")

avg_funding_amt <- master_frame %>%
  filter(funding_round_type %in% funding_round_types) %>%
  group_by(funding_round_type) %>%
  summarise(mean = mean(raised_amount_usd, na.rm = TRUE))

print("##Average investment amount##")
print(avg_funding_amt)

most_suitable_investment <- master_frame %>%
  filter(funding_round_type %in% funding_round_types) %>%
  group_by(funding_round_type) %>%
  summarise(Avg = mean(raised_amount_usd, na.rm = TRUE)) %>%
  filter(Avg > 5000000 & Avg < 15000000) %>%
  arrange(desc(Avg)) %>% head(1)

print("Most Suitable Investment type for SPARK FUNDS")
print(most_suitable_investment$funding_round_type)



################################################################################
###########              Checkpoint 3: Country Analysis       ############
################################################################################


#Spark Funds wants to see the top nine countries which have received the
#highest total funding (across ALL sectors for the chosen investment type)

top9 <- master_frame %>%
  filter(funding_round_type %in% most_suitable_investment$funding_round_type) %>%
  group_by(country_code) %>%
  drop_na(country_code) %>%
  summarise(SUM = sum(raised_amount_usd, na.rm = TRUE)) %>%
  arrange(desc(SUM)) %>%
  head(n = 9L)

print("##Top 9 Countries received the highest total funding for Venture")
print(top9)

#Loading English Speaking countries
country_file <-
  "Countries_where_English_is_an_official_language.pdf"

country_list <- pdf_text(country_file)
country_list <- gsub("[ ]{2,}", ",", country_list, perl = TRUE)
country_list <- gsub("\\r\\n", ",", country_list, perl = TRUE)
country_list <-
  unlist(strsplit(noquote(country_list), ",", fixed = TRUE))
country_list <- countrycode(country_list, 'country.name', 'iso3c')
country_list <- country_list[!is.na(country_list)]

top3 <- top9 %>%
  filter(top9$country_code %in% country_list) %>%
  head(3)

print("##Top 3 English speaking countries")
print(top3)


################################################################################
###########              Checkpoint 4: Sector Analysis 1       ############
################################################################################

#Extract the primary sector of each category list from the category_list column

master_frame_sector <- master_frame %>%
  separate(category_list,
           into = c("primary_category"),
           sep = "\\|")

#Loading the mapping.csv to mapping_data dataframe
mapping_data <-
  read.csv("mapping.csv",
           stringsAsFactors = F,
           na.strings = c("", "NA"))


#Converting the mapping_data to long data format


mapping_data_long <- mapping_data %>%
  drop_na(category_list) %>%
  mutate(category_list = str_replace_all(
    category_list,
    c("^0([a-z|A-Z])" = "Na\\1", "([a-z|A-Z])0" = "\\1na")
  )) %>%
  gather(Sector,
         Val,
         Automotive...Sports:Social..Finance..Analytics..Advertising) %>%
  filter (Val != 0) %>%
  select(-Val)


#Merged data frame with each primary sector mapped to its main sector
master_frame_sector$primary_category <-
  tolower(master_frame_sector$primary_category)
mapping_data_long$category_list <-
  tolower(mapping_data_long$category_list)

#row reduced because of Non matched category_list
master_frame_sector_DF <-
  merge(master_frame_sector,
        mapping_data_long,
        by.x = "primary_category",
        by.y = "category_list")

################################################################################
###########              Checkpoint 5: Sector Analysis 2       ############
################################################################################

get_byCountryOrder <- function(n) {
  return(
    master_frame_sector_DF %>%
      #drop_na(country_code) %>%
      filter(
        master_frame_sector_DF$country_code == top3$country_code[n] &
          funding_round_type == most_suitable_investment$funding_round_type &
          raised_amount_usd  > 5000000 &
          raised_amount_usd < 15000000
      ) %>%
      group_by(Sector) %>%
      mutate(
        count = n(),
        sum = sum(raised_amount_usd, na.rm = TRUE)
      )
  )
}

D1 <- get_byCountryOrder(1)
D2 <- get_byCountryOrder(2)
D3 <- get_byCountryOrder(3)


###################################################
######### Sector wise Investment Analysis    ######
###################################################

get_CountryAnalysis <- function(country_df) {
  # 1. Total number of investments (count)
  sector_wise <-
    country_df %>% group_by(Sector, count, sum) %>% summarise(tmp = all()) %>% select(-tmp)
  
  # 1. Total number of investments (count)
  print("1. Total Number of Investment")
  print(sum(sector_wise$count))
  
  #2. Total amount of investment (USD)
  print("2. Total Amount of investment")
  print(sum(sector_wise$sum))
  
  #Sorth by Count of investment
  sector_wise_count_sort <-
    sector_wise[with(sector_wise, order(-count)), ]
  #3. Top sector (based on count of investments)
  print("3. Top sector (based on count of investments)")
  top_1_sector <- sector_wise_count_sort[1, ]
  print(top_1_sector$Sector)
  
  #4. Second-best sector (based on count of investments)
  print("4. Second-best sectorr (based on count of investments)")
  top_2_sector <- sector_wise_count_sort[2, ]
  print(top_2_sector$Sector)
  
  #5. Third-best sector (based on count of investments)
  print("5. Third-best sectorr (based on count of investments)")
  top_3_sector <- sector_wise_count_sort[3, ]
  print(top_3_sector$Sector)
  
  #6. Number of investments in the top sector (refer to point 3)
  print("6. Number of investments in the top sector (refer to point 3)")
  print(top_1_sector$count)
  
  #7. Number of investments in the second-best sector (refer to point 4)
  print("7. Number of investments in the second-best sector (refer to point 4)")
  print(top_2_sector$count)
  
  #8. Number of investments in the third-best sector (refer to point 5)
  print("8. Number of investments in the third-best sector (refer to point 5)")
  print(top_3_sector$count)
  
  #9. For the top sector count-wise (point 3), which company received the highest investment?
  print(
    "9. For the top sector count-wise (point 3), which company received the highest investment?"
  )
  company_highest_investment_top1 <- country_df %>%
    filter(Sector == sector_wise_count_sort$Sector[1]) %>%
    group_by(company_permalink, name) %>%
    summarise(SUM = sum(raised_amount_usd, na.rm = TRUE)) %>%
    arrange(desc(SUM)) %>% ungroup() %>% filter(row_number() == 1) %>% select(name)
  print(company_highest_investment_top1$name)
  
  #10. For the second-best sector count-wise (point 4), which company received the highest investment?
  print(
    "10. For the second-best sector count-wise (point 4), which company received the highest investment?"
  )
  company_highest_investment_top2 <- country_df %>%
    filter(Sector == sector_wise_count_sort$Sector[2]) %>%
    group_by(company_permalink, name) %>%
    summarise(SUM = sum(raised_amount_usd, na.rm = TRUE)) %>%
    arrange(desc(SUM)) %>% ungroup() %>% filter(row_number() == 1) %>% select(name)
  
  print(company_highest_investment_top2$name)
  
  
}

## Sector Wise Analysis

get_CountryAnalysis(D1)
get_CountryAnalysis(D2)
get_CountryAnalysis(D3)
