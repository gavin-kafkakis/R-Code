rm(list=ls())

install.packages("rvest")
library(rvest)

# web scrapping 
webpage <- read_html("https://bballnet.com/")
table_node <- html_nodes(webpage, "table")

table_content <- html_table(table_node)[[1]]
head(table_content)

#reading in the new csv files 
resumes <- read.csv("Resumes.csv")
conference <- read.csv("Conference Results.csv")

# getting rid of unneccasary columns 
table_content$Prev <- NULL
conference$PAKE <- NULL
conference$PAKE.RANK <- NULL
conference$PASE.RANK <- NULL
conference$PASE <- NULL
conference$TOP2 <- NULL
conference$CHAMP. <- NULL

# merging the datasets and then getting rid of more columns 
resumes2 <- merge(resumes, table_content, by.x = 'TEAM',by.y = 'School', all.x = TRUE)
resumes2$Rank <- NULL
resumes2$Record <- NULL 
resumes2$Prev <- NULL
resumes2$R.SCORE <- NULL

# subseting the reusme dataset to find which teams are without a conference 
na_subset <- subset(resumes2,is.na(resumes2$Conf), )

# going through and giving teams a conference 
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

# checking to make sure that there are no more na values 
unique(resumes2$Conf)
unique(resumes2$TEAM)
