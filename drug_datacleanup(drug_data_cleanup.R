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

for (i in 1:length(revcount$drug) ) {
  matchdist<-adist(revcount$drug[i],cost$drug)[1,]
  # matchdist<-stringdist(revcount$drug[i],cost$drug) # several methods available
  
  matchdist<-ifelse(matchdist>threshold,NA,matchdist)
  sortedmatches[i]<-paste(cost$drug[order(matchdist, na.last=NA)], collapse = ", ")
  mindist[i]<- tryCatch(ifelse(is.integer(which.min(matchdist)),matchdist[which.min(matchdist)],NA), error = function(e){NA})
  match[i]<-ifelse(length(cost$drug[which.min(matchdist)])==0,NA,
                   cost$drug[which.min(matchdist)] )
}
res_big<-data.frame(revdrug=revcount$drug,costmatch=match,divergence=mindist,  stringsAsFactors = F) #sortedmatches=sortedmatches,
save(res_big, file="full_match_file.r")
res_big_clean<-res_big[res_big$divergence<2,]
res_big_clean<-res_big_clean[is.na(res_big_clean$divergence)==F,]
res_big_clean<-res_big_clean[,-3]
res_big_clean<-as_tibble(res_big_clean)

save(res_big_clean, file="cleaned_match_file.r")
matchfile<-res_big_clean %>%
  group_by(revdrug) %>%
  summarise(costmatch=costmatch[1])

##########
#Looking at the common words found in previous step have to do some cleanup to 
#make things match better.  Sigh.

cost[cost$generic %in% agrep("codeine",cost$generic, value=T),]
agrep("codeine",cost$generic)
paincost<-cost
cod<-paincost[paincost$generic %in% agrep("codeine",paincost$generic, value=T),]
paincost<-paincost[-agrep("codeine",paincost$generic),]
cod$drug<-"codeine"
cod$generic<-"codeine"
cod1<-cod %>%
  ungroup() %>%
  group_by(drug, generic) %>%
  summarise(supply=sum(supply), cost = sum(cost))
cod1
paincost<-rbind(paincost, cod1)
rm(cod1, cod)

cost[cost$generic %in% agrep("aspirin",cost$generic, value=T),]
pain_rev[pain_rev$drug %in% agrep("aspirin",pain_rev$drug, value=T), c("drug")]


#########
#review res file and shrink
res<-res[c(5,13, 17,19, 34, 35, 37, 43, 44, 49, 50, 54, 56:57, 
           62:65, 67:70,74:75, 77, 79:81, 84:85, 87:89, 91:94,
           96, 103, 105, 109:110, 112, 122, 129, 132:139,141:144,
           149, 153, 155, 159, 166, 169, 171, 176)]

###########
#Left join res_big_clean and revcount

reviewdata<-matchfile %>%
  left_join(revcount, by =c("revdrug"="drug")) %>%
  filter(condition %in% agrep("pain", condition, value=T)) %>%
  group_by(revdrug,costmatch, condition) %>%
  summarise(good=sum(s_love,s_wond,s_save,s_great,s_happy),
            bad=sum(s_hate, s_angry, s_illegal, s_nightmare, s_burns, s_waste, s_irrit,
                    s_ineff),
            rate=mean(a_rating, na.rm=T)) %>%
  arrange(desc(good)) 
rev_and_cost<-reviewdata %>%
  left_join(cost, by=c("costmatch" = "drug"))

save(rev_and_cost, file="joined_review_and_cost_files.r")

#rm(revcount)
#rm(reviewdata)
#rm(smalluse,use, res_big, res_big_clean, res, pain_rev, paincost, nam, matchfile, drev, cost)
##########
#Explore file
table(rev_and_cost$condition)
randc<-rev_and_cost %>%
  group_by(revdrug, condition, good, bad, rate) %>%
  summarise(supply = sum(supply), cost=sum(cost)) %>%
  mutate(norm_good=(good/supply)*10000, 
         norm_bad=(bad/supply)*10000,
         drug=revdrug) %>%
  ungroup() %>%
    select(drug, condition, good, bad, rate, supply, cost, norm_good, norm_bad)



randc %>%
  filter(condition %in% agrep("pain", condition, ignore.case = T, value = T),
                              good>0) %>%
  ggplot(aes(cost, norm_good))+
  geom_point()

reg<-lm(cost ~ good+bad+rate, data=rev_and_cost[rev_and_cost$good>0,])

##########
#
library(ggplot)
topsupply<-randc %>%
  filter(supply >1000000) %>%
  group_by(drug) %>%
  summarise(good=sum(good),
            bad=sum(bad),
            rate=mean(rate),
            supply=sum(supply),
            cost=sum(cost),
            percost=sum(cost)/sum(supply)) #%>%
 
topsupply %>%
  filter(percost<150) %>%
  ggplot( aes(rate, percost)) +
  geom_boxplot(aes(cut_width(rate, 1))) +
  theme_classic()+
  scale_y_continuous(name="Per Pill Cost for Top 125 Prescribed Drugs\n (Cost<$100 excludes 3 drugs)",limits=c(0,100),
                     breaks=c(0,25,50,75,100 ),
                     labels=c("$0", "$25", "$50", "$75","$100"))+
  scale_x_discrete(name="Average Drug Review Raings", labels= c("<1.5",
                                                                "1.5-2.5", "2.5-3.5",
                                                                "3.5-4.5", "4.5-5.5",
                                                                "5.5-6.5", "6.5-7.5",
                                                                "7.5-8.5", "8.5-9.5",
                                                                ">9.5"))
  
