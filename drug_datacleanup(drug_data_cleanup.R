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
use %>%
  select(`Product Name`)

use

