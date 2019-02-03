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
  add_tally() %>%
  group_by(drug, condition, n)%>%
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
            s_rating = sum(rating)
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
#Combining data sets using fuzzy logic (gonna drop a few obvi but better quick than perfect)
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


###########
#Left join res_big_clean and revcount

reviewdata<-matchfile %>%
  left_join(revcount, by =c("revdrug"="drug")) %>%
  #filter(condition %in% agrep("pain", condition, value=T)) %>%
  group_by(revdrug,costmatch, condition) %>%
  summarise(good=sum(s_love,s_wond,s_save,s_great,s_happy),
            bad=sum(s_hate, s_angry, s_illegal, s_nightmare, s_burns, s_waste, s_irrit,
                    s_ineff),
            count=sum(n),
            s_rate=sum(s_rating)) 
save(reviewdata, file="fully_joined_review_data.r")
rev_and_cost<-reviewdata %>%
  left_join(cost, by=c("costmatch" = "drug")) %>%
  ungroup() %>%
  select(-costmatch, -generic)
rev_and_cost<-rev_and_cost[is.na(rev_and_cost$condition)==F,]

save(rev_and_cost, file="joined_review_and_cost_files.r")

#rm(revcount)
#rm(reviewdata)
#rm(smalluse,use, res_big, res_big_clean, res, pain_rev, paincost, nam, matchfile, drev, cost)
##########
#Explore file
#table(rev_and_cost$condition)
randc<-
rev_and_cost %>%
  group_by(revdrug, supply, cost) %>%
  summarise(good=sum(good),
         bad=sum(bad),
         count=sum(count),
         s_rate=sum(s_rate)) %>%
        mutate( drug=revdrug) %>%
  ungroup() %>%
    select(drug,  good, bad, count, s_rate, supply, cost) %>%
  mutate(rating=s_rate/count)
#It appears some drugs created two lines and need to be combine

randc<-randc %>% group_by(drug, good, bad, rating, count) %>%
  summarise(supply=sum(supply), cost=sum(cost)) %>%
  ungroup()


reg<-lm(cost ~ good+bad+rating+good*rating, data=randc[randc$good>0,])
summary(reg)
##########
#
library(ggplot)
drugs<-randc %>%
  #filter(supply >1000000) %>%
  group_by(drug) %>%
  summarise(good=sum(good),
            bad=sum(bad),
            rate=mean(rating),
            supply=sum(supply),
            cost=sum(cost),
            percost=sum(cost)/sum(supply)) #%>%

drugs %>%filter(percost>200) %>% arrange(desc(percost))
 
drugs %>%
  #filter(percost<150) %>%
  ggplot( aes(rate, percost)) +
  geom_boxplot(aes(cut_width(rate, 1))) +
  theme_classic()+
  scale_y_continuous(name="Per Pill Cost for Top 125 Prescribed Drugs\n (Cost<$100 excludes 3 drugs)",#limits=c(0,100),
                      breaks=c(0,25,50,75,100 ),
                      labels=c("$0", "$25", "$50", "$75","$100"))+
  scale_x_discrete(name="Average Drug Review Raings", labels= c("<1.5",
                                                                "1.5-2.5", "2.5-3.5",
                                                                "3.5-4.5", "4.5-5.5",
                                                                "5.5-6.5", "6.5-7.5",
                                                                "7.5-8.5", "8.5-9.5",
                                                                ">9.5"))

drugs %>%
  #filter(percost<150) %>%
  ggplot( aes(rate, percost)) +
  geom_boxplot(aes(cut_width(rate, 1))) +
  theme_classic()+
  scale_y_continuous(name="Per Pill Cost for Top 125 Prescribed Drugs\n (Cost<$100 excludes 3 drugs)",#limits=c(0,100),
                     breaks=c(0,5000,10000,15000,20000 ),
                     labels=c("$0", "$5k", "$10k", "$15k","$20k"))+
  scale_x_discrete(name="Average Drug Review Raings", labels= c("<1.5",
                                                                "1.5-2.5", "2.5-3.5",
                                                                "3.5-4.5", "4.5-5.5",
                                                                "5.5-6.5", "6.5-7.5",
                                                                "7.5-8.5", "8.5-9.5",
                                                                ">9.5"))+
  ggtitle("Per Pill Cost of 1,424 Medicare Covered Drugs by User Ratings")
  
drugs %>%
  filter(percost>1000)

###########
#Looking at words
wordrate<-randc %>%
  group_by(drug) %>%
  summarise(good=sum(good),
            bad=sum(bad),
            rate=mean(rating),
            supply=sum(supply),
            cost=sum(cost),
            percost=sum(cost)/sum(supply),
            count=sum(count),
            goodrate=good/count,
            badrate = bad/count) 

goodd<- wordrate%>%
  arrange(desc(goodrate)) %>%
  filter(count>2) %>%
  slice(1:10) %>%
  mutate(scaledmoney=cost/sum(cost))
save(goodd, file="Good Comments.r")

badd<- wordrate%>%
  arrange(desc(badrate)) %>%
  filter(count>2) %>%
  slice(1:10)%>%
  mutate(scaledmoney=cost/sum(cost))
save(badd, file="Bad Comments.r")

ggplot(data=goodd, aes(x=factor(drug, levels=goodd$drug), y=goodrate))+
  geom_bar(stat="identity", fill= "darkgreen") +
  geom_line(data=goodd, aes(x=factor(drug, levels=drug), 
                           y=scaledmoney, group=1), stat="identity")+
  coord_flip() +
  theme_classic() +
  scale_x_discrete(name = "Drugs with Positive Comments")+
  scale_y_continuous(name="Percent of Comments with Postive Phrases", limits=c(0,1),
                     breaks=seq(0,1,0.2),
                     labels=c("0%", "20%", "40%", "60%", "80%", "100%")) +
  annotate("text", x=9.75, y=0.9, label="$337MM")+
  ggtitle("Percent of Positive Phrases for Drugs with More Than 2 Reviews")

ggplot(data=badd, aes(x=factor(drug, levels=badd$drug), y=badrate))+
  geom_bar(stat="identity", fill= "darkred")  +
  geom_line(data=badd, aes(x=factor(drug, levels=badd$drug), 
                           y=badd$scaledmoney, group=1), stat="identity")+
  coord_flip() +
  theme_classic() +
  scale_x_discrete(name = "Drugs with Positive Comments")+
  scale_y_continuous(name="Percent of Comments with Negative Phrases", limits=c(0,1),
                     breaks=seq(0,1,0.2),
                     labels=c("0%", "20%", "40%", "60%", "80%", "100%"))+
    annotate("text", x=7, y=0.5, label="$244MM")+
  annotate("text", x=6, y=0.5, label="$159MM")+
  annotate("text", x=8, y=0.5, label="$163MM")+
  ggtitle("Percent of Negative Phrases for Drugs with More Than 2 Reviews")



