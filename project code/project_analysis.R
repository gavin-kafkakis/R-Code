# data analysis 
rm(list=ls())


# question 1 
# finding all of the champions
champions <- subset(resumes2, resumes2$ROUND == 1, )

# code for finding the contenders for this based on the champions of the last year 
champion_contenders <- subset(resumes2, resumes2$YEAR == 2024,)
champion_contenders <- subset(champion_contenders, champion_contenders$SEED <= 7, )
champion_contenders <- subset(champion_contenders, champion_contenders$NET.RPI <= 22, )
champion_contenders <- subset(champion_contenders, champion_contenders$RESUME <= 19, )
champion_contenders <- subset(champion_contenders, champion_contenders$ELO <= 22, )

contender_2023 <- subset(resumes2, resumes2$YEAR == 2023,)
contender_2023 <- subset(contender_2023, contender_2023$SEED <=7, )
contender_2023 <- subset(contender_2023, contender_2023$NET.RPI <= 22, )
contender_2023 <- subset(contender_2023, contender_2023$RESUME <= 19, )
contender_2023 <- subset(contender_2023, contender_2023$ELO <= 22, )
contender_2023
#qplot(TEAM, ROUND, data = contender_2023, geom = "point")

contender_2022 <- subset(resumes2, resumes2$YEAR == 2022,)
contender_2022 <- subset(contender_2022, contender_2022$SEED <=7, )
contender_2022 <- subset(contender_2022, contender_2022$NET.RPI <= 22, )
contender_2022 <- subset(contender_2022, contender_2022$RESUME <= 19, )
contender_2022 <- subset(contender_2022, contender_2022$ELO <= 22, )
contender_2022

contender_2021 <- subset(resumes2, resumes2$YEAR == 2021,)
contender_2021 <- subset(contender_2021, contender_2021$SEED <=7, )
contender_2021 <- subset(contender_2021, contender_2021$NET.RPI <= 22, )
contender_2021 <- subset(contender_2021, contender_2021$RESUME <= 19, )
contender_2021 <- subset(contender_2021, contender_2021$ELO <= 22, )
contender_2021

contender_2019 <- subset(resumes2, resumes2$YEAR == 2019,)
contender_2019 <- subset(contender_2019, contender_2019$SEED <=7, )
contender_2019 <- subset(contender_2019, contender_2019$NET.RPI <= 22, )
contender_2019 <- subset(contender_2019, contender_2019$RESUME <= 19, )
contender_2019 <- subset(contender_2019, contender_2019$ELO <= 22, )
contender_2019

contender_2018 <- subset(resumes2, resumes2$YEAR == 2018,)
contender_2018 <- subset(contender_2018, contender_2018$SEED <=7, )
contender_2018 <- subset(contender_2018, contender_2018$NET.RPI <= 22, )
contender_2018 <- subset(contender_2018, contender_2018$RESUME <= 19, )
contender_2018 <- subset(contender_2018, contender_2018$ELO <= 22, )
contender_2018

# combining the championship contenders 
all_contenders <- rbind(contender_2023, contender_2022, contender_2021, contender_2019, contender_2018)

# different vizualizations made 
qplot(B.POWER, NET.RPI, data = champions, geom = "point", col = TEAM, main = "Past Champions of March Madness")

qplot(B.POWER, NET.RPI, data = champion_contenders, geom = "point", col = TEAM, main = "Championship Contenders for March Madness 2024")

qplot(TEAM, ROUND, data = all_contenders, geom = "point", col = SEED)

#question 2 
# reording the conference dataset
conference2 <- conference[order(conference$W, decreasing = TRUE), ]
confernce2 <- head(conference2, 10)

# testing different results for conferences
CUSA_results <- team_results2[which(team_results2$Conf == "CUSA"), ]
MVA_results <- team_results2[which(team_results2$Conf == "MVC"), ]

# plotting some 
qplot(CONF,W, data = conference2, geom = "bar")

class(conference2$CONF)

# testing results for conferences
MWC <- subset(resumes2, resumes2$Conf =="Mountain West" & resumes2$YEAR >= 2018 & 
                resumes2$TEAM != 'San Diego St.', )
MVC <- subset(resumes2, resumes2$Conf == "MVC", )

amercian <- subset(resumes2, resumes2$Conf == "American")

# the vizualizations that were made for the final project
top6 <- head(conference2, 6)
ggplot(data = top6, aes(x = CONF, y = WIN., fill = CONF)) +
  geom_bar(stat = "identity") + scale_fill_manual(values = c("blue", "red", "green", "yellow", "purple", "orange")) 
+ ggtitle("Top 6 Conferences Winning Percentage")
top6$CONF <- as.factor(top6$CONF)
top6$WIN. <- as.numeric(top6$WIN.)

ggplot(data = top6, aes(x = CONF, y = W, fill = CONF)) +
  geom_bar(stat = "identity") + scale_fill_manual(values = c("blue", "red", "green", "yellow", "purple", "orange")) + 
  ggtitle("Top 6 Conferences Wins") 

#question 3
# linear regression
fit <- lm(ROUND~NET.RPI + RESUME + WAB.RANK + ELO + B.POWER, data = resumes2) 
summary(fit)
# making a data frame without 2024
resumes_no_2024 <- subset(resumes2, resumes2$YEAR < 2024, )

# vizaualizations 
qplot(NET.RPI, ROUND, data = resumes_no_2024, geom = "point", col = SEED, main = "NET RPI with Round and Seed")

qplot(B.POWER, ROUND, data = resumes_no_2024, geom = "point", col = SEED, main = "B Power with Round and Seed")

