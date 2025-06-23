rm(list=ls())

install.packages("rvest")
library(rvest)

webpage <- read_html("https://bballnet.com/")
table_node <- html_nodes(webpage, "table")

table_content <- html_table(table_node)[[1]]
head(table_content)

team_results <- read.csv("Team Results.csv")
resumes <- read.csv("Resumes.csv")
conference <- read.csv("Conference Results.csv")

table_content$Prev <- NULL
conference$PAKE <- NULL
conference$PAKE.RANK <- NULL
conference$PASE.RANK <- NULL
conference$PASE <- NULL
conference$TOP2 <- NULL
conference$CHAMP. <- NULL

resumes2 <- merge(resumes, table_content, by.x = 'TEAM',by.y = 'School', all.x = TRUE)
resumes2$Rank <- NULL
resumes2$Record <- NULL 
resumes2$Prev <- NULL
resumes2$R.SCORE <- NULL
resumes2$PLUS.500 <- NULL

na_subset <- subset(resumes2,is.na(resumes2$Conf), )


# na subs 
resumes2$Conf[resumes2$TEAM == 'Albany'] <- 'American East'
resumes2$Conf[resumes2$TEAM == 'Appalachian St.'] <- 'Sun Belt'
resumes2$Conf[resumes2$TEAM == 'Arkansas Pine Bluff'] <- 'SWAC'
resumes2$Conf[resumes2$TEAM == 'Boston University'] <- 'American East'
resumes2$Conf[resumes2$TEAM == 'Cal St. Bakersfield'] <- 'WAC'
resumes2$Conf[resumes2$TEAM == '	Cal St. Northridge'] <- 'Big West' 
resumes2$Conf[resumes2$TEAM == 'College of Charleston'] <- 'CAA'
resumes2$Conf[resumes2$TEAM == 'Connecticut' & resumes2$YEAR <= 2020] <- 'American'
resumes2$Conf[resumes2$TEAM == 'Connecticut' & resumes2$YEAR > 2020] <- 'Big East'
resumes2$Conf[resumes2$TEAM == 'Detroit'] <- 'Horizon'
resumes2$Conf[resumes2$TEAM == 'East Tennessee St.' & resumes2$YEAR <= 2010] <- 'ASUN'
resumes2$Conf[resumes2$TEAM == 'East Tennessee St.' & resumes2$YEAR > 2010] <- 'SoCon'
resumes2$Conf[resumes2$TEAM == 'Eastern Kentucky'] <- 'OVC'
resumes2$Conf[resumes2$TEAM == 'Eastern Washington'] <- 'Big Sky'
resumes2$Conf[resumes2$TEAM == 'Fairleigh Dickinson'] <- 'NEC'
resumes2$Conf[resumes2$TEAM == 'Florida Atlantic' & resumes2$YEAR == 2024] <- 'American'
resumes2$Conf[resumes2$TEAM == 'Florida Atlantic' & resumes2$YEAR == 2023] <- 'CUSA'
resumes2$Conf[resumes2$TEAM == 'Florida Gulf Coast'] <- 'ASUN'
resumes2$Conf[resumes2$TEAM == 'Gardner Webb'] <- 'Big South'
resumes2$Conf[resumes2$TEAM == 'Grambling St.'] <- 'SWAC'
resumes2$Conf[resumes2$TEAM == 'Hartford'] <- 'American East'
resumes2$Conf[resumes2$TEAM == 'Lamar'] <- 'Southland'
resumes2$Conf[resumes2$TEAM == 'LIU Brooklyn'] <- 'NEC'
resumes2$Conf[resumes2$TEAM == 'Louisiana Lafayette'] <- 'Sun Belt'
resumes2$Conf[resumes2$TEAM == 'Loyola MD'] <- 'WCC'
resumes2$Conf[resumes2$TEAM == 'McNeese St.'] <- 'Southland'
resumes2$Conf[resumes2$TEAM == 'Miami FL'] <- 'ACC'
resumes2$Conf[resumes2$TEAM == 'Middle Tennessee' & resumes2$YEAR == 2012] <- 'Sun Belt'
resumes2$Conf[resumes2$TEAM == 'Middle Tennessee' & resumes2$YEAR > 2012] <- 'CUSA'
resumes2$Conf[resumes2$TEAM == 'Mississippi'] <- 'SEC'
resumes2$Conf[resumes2$TEAM == 'Mississippi Valley St.'] <- 'SWAC'
resumes2$Conf[resumes2$TEAM == 'North Carolina A&T'] <- 'MEAC'
resumes2$Conf[resumes2$TEAM == 'North Carolina Central'] <- 'MEAC'
resumes2$Conf[resumes2$TEAM == 'North Carolina St.'] <- 'ACC'
resumes2$Conf[resumes2$TEAM == 'Northern Colorado'] <- 'Big Sky'
resumes2$Conf[resumes2$TEAM == 'Northern Iowa'] <- 'MVC'
resumes2$Conf[resumes2$TEAM == 'Northern Kentucky'] <- 'Horizon'
resumes2$Conf[resumes2$TEAM == 'Prairie View A&M'] <- 'SWAC'
resumes2$Conf[resumes2$TEAM == 'Saint Mary\'s'] <- 'WCC'
resumes2$Conf[resumes2$TEAM == 'Sam Houston St.'] <- 'Southland'
resumes2$Conf[resumes2$TEAM == 'South Florida'] <- 'Big East'
resumes2$Conf[resumes2$TEAM == 'Southeast Missouri St.'] <- 'OVC'
resumes2$Conf[resumes2$TEAM == 'Southern'] <- 'SWAC'
resumes2$Conf[resumes2$TEAM == 'Southern Miss'] <- 'CUSA'
resumes2$Conf[resumes2$TEAM == 'St.John\'s'] <- 'Big East'
resumes2$Conf[resumes2$TEAM == 'Stephen F. Austin'] <- 'Southland'
resumes2$Conf[resumes2$TEAM == 'Texas A&M Corpus Chris'] <- 'Southland'
resumes2$Conf[resumes2$TEAM == 'UNC Wilmington'] <- 'CAA'
resumes2$Conf[resumes2$TEAM == 'USC'] <- 'Pac 12'
resumes2$Conf[resumes2$TEAM == 'Western Kentucky' & resumes2$YEAR <= 2023] <- 'Sun Belt'
resumes2$Conf[resumes2$TEAM == 'Western Kentucky' & resumes2$YEAR == 2024] <- 'CUSA'
resumes2$Conf[resumes2$TEAM == 'Western Michigan'] <- 'MAC'
resumes2$Conf[resumes2$TEAM == 'Cal St. Northridge'] <- 'Big West'
unique(resumes2$Conf)
unique(resumes2$TEAM)


resumes_16 <- subset(resumes, resumes$ROUND <= 16, )
resumes_16 <- subset(resumes_16, resumes_16$ROUND > 0, )
average_rpi <- mean(resumes_16$NET.RPI)
average_rpi

average_wab <- mean(resumes_16$WAB.RANK)
average_wab


qplot(Q1.PLUS.Q2.W, data = resumes_16, geom = "bar")

qplot(ROUND, NET.RPI, data = resumes_16, geom = "bar", fun.y = "mean")


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

all_contenders <- rbind(contender_2023, contender_2022, contender_2021, contender_2019, contender_2018)
qplot(TEAM, ROUND, data = all_contenders, geom = "point", col = TEAM) + scale_x_discrete(labels = NULL)

qplot(CONF, W, data = conference, geom = "point")


conference2 <- conference[order(conference$W, decreasing = TRUE), ]
confernce2 <- head(conference2, 10)

CUSA_results <- team_results2[which(team_results2$Conf == "CUSA"), ]
MVA_results <- team_results2[which(team_results2$Conf == "MVC"), ]

qplot(CONF,W, data = conference2, geom = "bar")

class(conference2$CONF)

MWC <- subset(resumes2, resumes2$Conf =="Mountain West" & resumes2$YEAR >= 2018 & 
                resumes2$TEAM != 'San Diego St.', )
MVC <- subset(resumes2, resumes2$Conf == "MVC", )

amercian <- subset(resumes2, resumes2$Conf == "American")

top6 <- head(conference2, 6)
ggplot(data = top6, aes(x = CONF, y = WIN., fill = CONF)) +
  geom_bar(stat = "identity") + scale_fill_manual(values = c("blue", "red", "green", "yellow", "purple", "orange")) 
                                                  + ggtitle("Top 6 Conferences Winning Percentage")
top6$CONF <- as.factor(top6$CONF)
top6$WIN. <- as.numeric(top6$WIN.)

ggplot(data = top6, aes(x = CONF, y = W, fill = CONF)) +
  geom_bar(stat = "identity") + scale_fill_manual(values = c("blue", "red", "green", "yellow", "purple", "orange")) + 
                                                    ggtitle("Top 6 Conferences Wins") 

round64 <- subset(resumes2, resumes2$ROUND == 64,)
round32 <- subset(resumes2, resumes2$ROUND <= 32, )
mean(round64$NET.RPI)
mean(round32$NET.RPI)

mean(round64$RESUME)
mean(round32$RESUME)


fit <- lm(ROUND~NET.RPI + RESUME + WAB.RANK + ELO + B.POWER, data = resumes2) 
summary(fit)
resumes_no_2024 <- subset(resumes2, resumes2$YEAR < 2024, )

qplot(NET.RPI, ROUND, data = resumes_no_2024, geom = "point", col = SEED, main = "NET RPI with Round and Seed")

qplot(B.POWER, ROUND, data = resumes_no_2024, geom = "point", col = SEED, main = "B Power with Round and Seed")

qplot(B.POWER, NET.RPI, data = champions, geom = "point", col = TEAM, main = "Past Champions of March Madness")

qplot(B.POWER, NET.RPI, data = champion_contenders, geom = "point", col = TEAM, main = "Championship Contenders for March Madness 2024")


qplot(TEAM, ROUND, data = all_contenders, geom = "point", col = SEED)
