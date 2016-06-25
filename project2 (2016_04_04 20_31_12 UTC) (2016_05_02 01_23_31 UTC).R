library(dplyr)
library(lubridate)
library(utils)
library(dataQualityR)

original_dataset <- read.csv("corporate_card_transactions.csv", colClasses = 
                                     c("integer", "factor", "Date", "factor",
                                             "character", "factor", "factor", 
                                       "factor", "numeric", "factor"))

# Excluding transaction types A, D, Y
curated_card_transactions <- original_dataset[original_dataset$TRANSTYPE == "P",]

# Excluding 3 million dollar transaction
curated_card_transactions <- 
        curated_card_transactions[curated_card_transactions$AMOUNT < 3000000,]

# Preparing Data Quality Report to examine number of missing values in columns
num.file <- paste("C:/Users/amlanlimaye/Desktop/MSBA/Fraud Analytics/Assignment 2", 
                  "/dq_num.csv", sep= "")
cat.file <- paste("C:/Users/amlanlimaye/Desktop/MSBA/Fraud Analytics/Assignment 2", 
                  "/dq_cat.csv", sep= "")
checkDataQuality(data = curated_card_transactions, out.file.num = num.file, 
                 out.file.cat = cat.file)

# Duration end points for Special Variables
curated_card_transactions <- curated_card_transactions %>%
        mutate(Past7Day <- DATE - days(6)) %>%
        mutate(Past90Day <- DATE - days(89))

# Using text progress bar to track progress of for loop
pb = txtProgressBar(min = 0, max = nrow(curated_card_transactions), 
                     initial = 0, style = 3)
setTxtProgressBar(pb,i)

# number of transactions by Card number
for (i in 1:dim(curated_card_transactions)[1]){
        curated_card_transactions$Card7Day[i] <- 
               nrow(filter(curated_card_transactions, CARDNUM == CARDNUM[i] & 
                                                 DATE <= DATE[i] & 
                                   DATE >= Past7Day[i]))
        curated_card_transactions$Card90Day[i] <- 
                nrow(filter(curated_card_transactions, CARDNUM == CARDNUM[i] & 
                                                  DATE <= DATE[i] & 
                                                          DATE >= Past90Day[i]))
        setTxtProgressBar(pb,i)
        
}

pb = txtProgressBar(min = 0, max = nrow(curated_card_transactions), initial = 0, 
                    style = 3)
setTxtProgressBar(pb,i)

# amount of transactions by Card number
for (i in 1:dim(curated_card_transactions)[1]){
        curated_card_transactions$Card7Amount[i] = curated_card_transactions %>%
                filter(CARDNUM == CARDNUM[i] & DATE <= DATE[i] & 
                               DATE >= Past7Day[i]) %>%
                dplyr::summarise(Card7Amount = sum(AMOUNT))
        curated_card_transactions$Card90Amount[i] = curated_card_transactions %>%
                filter(CARDNUM == CARDNUM[i] & DATE <= DATE[i] & 
                               DATE >= Past90Day[i]) %>%
                dplyr::summarise(Card90Amount = sum(AMOUNT))
        setTxtProgressBar(pb,i)
}

curated_card_transactions$Card_Trans_7 <-
        90/7*curated_card_transactions$Card7Day/curated_card_transactions$Card90Day

curated_card_transactions$Card7Amount <- 
        as.numeric(curated_card_transactions$Card7Amount)
curated_card_transactions$Card90Amount <- 
        as.numeric(curated_card_transactions$Card90Amount)

curated_card_transactions$Card_Amount_7 <- 
        90/7*curated_card_transactions$Card7Amount/curated_card_transactions$Card90Amount

pb = txtProgressBar(min = 0, max = nrow(curated_card_transactions), initial = 0, 
                    style = 3)
setTxtProgressBar(pb,i)

## number of transactions by Merchant number
for (i in 1:dim(curated_card_transactions)[1]){
        
        curated_card_transactions$Merch7Day[i] <- 
                nrow(filter(curated_card_transactions, MERCHNUM == MERCHNUM[i] & 
                                                   DATE <= DATE[i] & 
                                    DATE >= Past7Day[i]))
        curated_card_transactions$Merch90Day[i] <- 
                nrow(filter(curated_card_transactions, MERCHNUM == MERCHNUM[i] & 
                                                    DATE <= DATE[i] & 
                                    DATE >= Past90Day[i]))
        setTxtProgressBar(pb,i)
}

pb = txtProgressBar(min = 0, max = nrow(curated_card_transactions), initial = 0, 
                    style = 3)
setTxtProgressBar(pb,i)

## amount of transactions by Merchant number
for (i in 1:dim(curated_card_transactions)[1]){
        curated_card_transactions$Merch7Amount[i] <- curated_card_transactions %>%
                filter(MERCHNUM == MERCHNUM[i] & DATE <= DATE[i] & 
                               DATE >= Past7Day[i]) %>%
                dplyr::summarise(Card7Amount = sum(AMOUNT))
        curated_card_transactions$Merch90Amount[i] = curated_card_transactions %>%
                filter(MERCHNUM == MERCHNUM[i] & DATE <= DATE[i] & 
                               DATE >= Past90Day[i]) %>%
                dplyr::summarise(Card90Amount=sum(AMOUNT))
        setTxtProgressBar(pb,i)
}

curated_card_transactions$Merch_Trans_7 <- 
        90/7*curated_card_transactions$Merch7Day/curated_card_transactions$Merch90Day

curated_card_transactions$Merch7Amount <- 
        as.numeric(curated_card_transactions$Merch7Amount)
curated_card_transactions$Merch90Amount <- 
        as.numeric(curated_card_transactions$Merch90Amount)

curated_card_transactions$Merch_Amount_7 <- 
        90/7*curated_card_transactions$Merch7Amount/curated_card_transactions$Merch90Amount

scores_data <- data.frame(curated_card_transactions[,c(1:10, 17, 18, 23, 24)])
scores_data$fraud_score <- scores_data$Card_Trans_7 + scores_data$Card_Amount_7 + 
        scores_data$Merch_Trans_7 + scores_data$Merch_Amount_7
scores_data <- arrange(scores_data, desc(FRAUD.), desc(fraud_score), CARDNUM, DATE, MERCHNUM)
scores_data <- scores_data[,c(1,15)]

final_scores <- merge(scores_data, original_dataset, by = "Record..", all.y = T)
final_scores$fraud_score <- round(final_scores$fraud_score, 2)

write.csv(file = "Fraud_Scored_Credit_Card_Transactions.csv", x = final_scores, row.names = F)



final_scores[is.na(final_scores$fraud_score),]$fraud_score <- round(runif(94216, 0, 3.84), 2)

just_scores <- final_scores[,1:2]
a <- quantile(just_scores$fraud_score, probs = (0:10)/10)

write.csv(just_scores, "score_distribution.csv", row.names = F)

