###########
#Drug Info exploration

library(tidyverse)

########
#Read in test data
dcom<-read_tsv("drugsComTest_raw.tsv")
dlib<-read_tsv("drugLibTest_raw.tsv")
names(dcom)
dcom
dcom %>%
  group_by(drugName, condition) %>%
  count(drugName) %>%
  arrange(desc(n))

dcom %>%
  count(condition) %>%
  arrange (desc(n))

dcom$review[1]

dlib %>%
  #group_by(urlDrugName, condition) %>%
  count(urlDrugName) %>%
  arrange(desc(n))
dcom[dcom$drugName=="Accutane",]

########
#Clean up dcom
library(xml2)
unescape_html <- function(str){
  xml2::xml_text(xml2::read_html(paste0("<x>", str, "</x>")))
}

for(i in 1:length(dcom$review)){
dcom$review[i]<-unescape_html(dcom$review[i])
}
library(tidyverse)
dcom_save<-dcom %>%
  select(drugName, condition, review, rating) %>%
  mutate(drug = str_to_lower(drugName),
         review = str_remove( review, pattern = '["\"]')) %>%
  mutate(review = str_remove( review, pattern = '[\"]')) %>%
  select(drug, condition, review, rating)

dcom1<-read_tsv("drugsComTrain_raw.tsv")
for(i in 1:length(dcom1$review)){
  dcom1$review[i]<-unescape_html(dcom1$review[i])
}
library(tidyverse)
dcom1_save<-dcom1 %>%
  select(drugName, condition, review, rating) %>%
  mutate(drug = str_to_lower(drugName),
         review = str_remove( review, pattern = '["\"]')) %>%
  mutate(review = str_remove( review, pattern = '[\"]')) %>%
  select(drug, condition, review, rating)
save(dcom1_save, file="dcom.r")

##########
#dlib cleanup
dlib<-read_tsv("drugLibTrain_raw.tsv")
dlib_save<-dlib %>%
  select(urlDrugName, condition, benefitsReview, rating) %>%
  mutate(review = str_remove_all(benefitsReview, pattern = '[\r]')) %>%
  mutate(review = str_remove_all(benefitsReview, pattern = '[\n]')) %>%
  mutate(review = str_remove_all(benefitsReview, pattern = '[\"]')) %>%
  rename(drug =urlDrugName ) %>%
  select (-benefitsReview) %>%
  select(drug, condition, review, rating)
dlib_save %>%
  count(rating)
save(dlib_save, file="dlib.r")
load(file="dcom.r")

#############
#combine
drev<-rbind(dcom_save, dlib_save)
drev
drev<-rbind(drev, dcom1_save)
save(drev, file="reviews_combined.r")

rm(dlib, dlib_save, dcom, dcom_save, data)
rm(dcom1, dcom1_save)
rm(dcom1_save)
#########
use<-read_csv(file="Drug_Utilization_2017_-_National_Total.csv")
smalluse<-use %>%
  select(`Product Name`, `Number of Prescriptions`,`Total Amount Reimbursed`) %>%
  na.omit() %>%
  mutate(drug = str_to_lower(`Product Name`)) %>%
  select(-`Product Name`) %>%
  group_by(drug)%>%
  summarise(cost=sum(`Total Amount Reimbursed`), numscrip=sum(`Number of Prescriptions`))

smalluse
save(smalluse, file="spending_and_perscribed.r")

use

######
#Looking at comments
library(stringr)
library(tidyverse)
revcount<-drev %>%
  mutate(lovec=str_count( review, "love"),
         hatec=str_count( review, "hate"),
         angryc=str_count( review, "angry"),
         illegalc=str_count( review, "illegal"),
         nightmarec=str_count( review, "nightmare"),
         burnsc=str_count( review, "burns"),
         wastec =str_count( review, "waste of money"),
         wonderfulc = str_count( review, "wonderful"),
         savec=str_count( review, "saved my life"),
         greatc=str_count( review, "great"),
         irritablec=str_count( review, "irritable"),
         ineffectivec=str_count( review, "ineffective"),
         happyc=str_count( review, "happy")) %>%
  group_by(drug, condition) %>%
  summarise(s_love = sum(lovec),
            s_wond = sum(wonderfulc),
            s_save = sum(savec),
            s_great = sum(greatc),
            s_happy = sum(happyc),
            s_hate = sum(hatec),
            s_angry = sum(angryc),
            s_illegal = sum(illegalc),
            s_nightmare = sum(nightmarec),
            s_burns = sum(burnsc),
            s_waste = sum(wastec),
            s_irrit = sum(irritablec),
            s_ineff = sum(ineffectivec),
            a_rating = mean(rating)
            )




save(revcount,file="review_word_count.r")

revcount %>%
  arrange(desc(s_love)) %>%
  select (drug, condition, a_rating, s_love, s_happy)

revcount %>%
  select(condition) %>%
  group_by(condition) %>%
  count(condition) %>%
  arrange(desc(n))

pain_rev<- revcount %>%
  filter(condition =="Pain")
save(pain_rev, file="just_pain_reviews.r")

########
#Drug Names
nam<-read_tsv(file="drug_names.tsv")
nam
smalluse

partd<-read_tsv(file="PartD_Prescriber_PUF_NPI_Drug_16.txt")
cost<-partd %>%
  select(drug_name, generic_name, total_day_supply, total_drug_cost) %>%
  group_by(drug_name, generic_name) %>%
  summarise (supply=sum(total_day_supply),
             cost=sum(total_drug_cost))
rm(partd)
cost<-cost %>%
  mutate(drug = str_to_lower(drug_name),
         generic = str_to_lower(generic_name)) %>%
  select(drug, generic, supply, cost)
cost<-cost %>%
  ungroup() %>%
  group_by(drug, generic) %>%
  select(-drug_name) 
  



save(cost, file="partdcost.r")

##########
#
agrep(pain_rev$drug[1],cost$generic, value=T)
cost
cost$drug[1]      
cost$drug[2] 
cost[cost$generic %in% agrep(pain_rev$drug[1],cost$generic, value=T),]

##########
#Testing stack overflow answer to problem
library(stringdist)


match<-character()
threshold<-14 # max 14 characters of divergence
mindist<-integer()
sortedmatches<-character()

for (i in 1:length(pain_rev$drug) ) {
  matchdist<-adist(pain_rev$drug[i],cost$drug)[1,]
  # matchdist<-stringdist(revcount$drug[i],cost$drug) # several methods available
  
  matchdist<-ifelse(matchdist>threshold,NA,matchdist)
  sortedmatches[i]<-paste(cost$drug[order(matchdist, na.last=NA)], collapse = ", ")
  mindist[i]<- tryCatch(ifelse(is.integer(which.min(matchdist)),matchdist[which.min(matchdist)],NA), error = function(e){NA})
  match[i]<-ifelse(length(cost$drug[which.min(matchdist)])==0,NA,
                   cost$drug[which.min(matchdist)] )
}
res<-data.frame(pain_rev$drug=pain_rev$drug,match=match,divergence=mindist, sortedmatches=sortedmatches, stringsAsFactors = F)
res
