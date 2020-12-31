#itunesr gerekli
#devtools::install_github("amrrs/itunesr")
require("itunesr")
#devtools::install_github("cran/rPython")

obiletID<-596039443
require(rvest)
require(xtable)
require(httr)
require(data.table)
require(googlesheets)
require(googlesheets4)
require(reshape)
require(plyr)
require(dplyr)
require(googleAnalyticsR)
obilet_tr<-data.frame(matrix(ncol = 7, nrow = 0))
obilet_us<-data.frame(matrix(ncol = 7, nrow = 0))
obilet_de<-data.frame(matrix(ncol = 7, nrow = 0))
colnames<-c("Title","Author_URL","Author_Name","App_Version","Rating","Review","Date")
colnames(obilet_tr)<-colnames
colnames(obilet_us)<-colnames
colnames(obilet_de)<-colnames

#Turkey
for(i in 1:10) {
  print(i)
  obilet_reviews_dummy_tr <- getReviews(obiletID,'tr',i)
  obilet_reviews_dummy_tr$Date<-as.Date(as.character(as.POSIXct(obilet_reviews_dummy_tr$Date)))
  obilet_reviews_dummy_tr$App_Version <-as.character(obilet_reviews_dummy_tr$App_Version)
  obilet_tr<-rbind.fill(obilet_tr,obilet_reviews_dummy_tr)
}
obilet_tr$Country<-"Turkey"
obilet_reviews_tr<-obilet_tr
obilet_reviews_tr$Date<-as.Date(obilet_reviews_tr$Date, origin="1970-01-01")

#USA
for(i in 1:1) {
  obilet_reviews_dummy_us <- getReviews(obiletID,'us',i)
  obilet_reviews_dummy_us$Date<-as.Date(as.character(as.POSIXct(obilet_reviews_dummy_us$Date)))
  obilet_reviews_dummy_us$App_Version <-as.character(obilet_reviews_dummy_us$App_Version)
  obilet_us<-rbind.fill(obilet_us,obilet_reviews_dummy_us)
}
obilet_us$Country<-"USA"
obilet_reviews_us<-obilet_us


obilet_reviews_us$Date<-as.Date(obilet_reviews_us$Date, origin="1970-01-01")

#Germany
for(i in 1:1) {
  obilet_reviews_dummy_de <- getReviews(obiletID,'de',i)
  obilet_reviews_dummy_de$Date<-as.Date(as.character(as.POSIXct(obilet_reviews_dummy_de$Date)))
  obilet_reviews_dummy_de$App_Version <-as.character(obilet_reviews_dummy_de$App_Version)
  obilet_de<-rbind.fill(obilet_de,obilet_reviews_dummy_de)
}
obilet_de$Country<-"Germany"
obilet_reviews_de<-obilet_de
obilet_reviews_de$Date<-as.Date(obilet_reviews_de$Date, origin="1970-01-01")

#Total
obilet_reviews<-rbind.fill(obilet_reviews_tr,obilet_reviews_us,obilet_reviews_de)
require(googlesheets4)
# sheet_write(obilet_reviews,
#             ss="https://docs.google.com/spreadsheets/d/1zNyDFlo9wJdtjGV9SzRp8z7TOctUyxV4gsi8tqAICnI/edit#gid=0",
#             sheet="ios_reviews2")

message <- obilet_reviews[c("Date","Rating","Review")]
message$Platform <- "iOS"
# devtools::install_github('hrbrmstr/slackr')
require(slackr)
require(lubridate)

slackr_setup(channel = "#ios-review-itunesr", username= "ios_review",
                       incoming_webhook_url = "https://hooks.slack.com/services/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",
                       bot_user_oauth_token = "xoxb-2893681624-1588164479511-KVdNdxJLjjiqTd7Mn5bERs8e")
require(rPython)
python.load("/Users/yusufhancer/Desktop/androidScrap.py")
require(purrr)
android <- read.csv2("/Users/yusufhancer/Desktop/android.csv",header=F)
colnames <- c("Date","Rating","Review")
colnames(android) <- colnames
android$Platform <- "Android"
android$Date <- gsub(" Ocak ","-1-",android$Date)
android$Date <- gsub(" Şubat ","-2-",android$Date)
android$Date <- gsub(" Mart ","-3-",android$Date)
android$Date <- gsub(" Nisan ","-4-",android$Date)
android$Date <- gsub(" Mayıs ","-5-",android$Date)
android$Date <- gsub(" Haziran ","-6-",android$Date)
android$Date <- gsub(" Temmuz ","-7-",android$Date)
android$Date <- gsub(" Ağustos ","-8-",android$Date)
android$Date <- gsub(" Eylül ","-9-",android$Date)
android$Date <- gsub(" Ekim ","-10-",android$Date)
android$Date <- gsub(" Kasım ","-11-",android$Date)
android$Date <- gsub(" Aralık ","-12-",android$Date)

android$Date <- as.Date(android$Date, format="%d-%m-%y")


message <- rbind.fill(message,android)
# slackr_setup(config_file = "/Users/yusufhancer/Desktop/iOS-review.txt")
message$Rating <- ifelse(message[,"Rating"]=="1" | message[,"Rating"]=="1 yıldız ","★☆☆☆☆",
                     ifelse(message[,"Rating"]=="2" | message[,"Rating"]=="2 yıldız ","★★☆☆☆",
                        ifelse(message[,"Rating"]=="3" | message[,"Rating"]=="3 yıldız ","★★★☆☆",
                           ifelse(message[,"Rating"]=="4" | message[,"Rating"]=="4 yıldız ", "★★★★☆","★★★★★"))))

messagefromyesterday <- filter(message, Date > Sys.Date()-1)
if((Sys.Date()-1) %in% unique(messagefromyesterday$Date)){
  textSlackr(paste0("Reviews from ", Sys.Date()-1, " ", weekdays.Date(Sys.Date()-1),":"))
  for(i in 1:length(messagefromyesterday)){
      textSlackr(paste0(messagefromyesterday[i,"Platform"],"
Rating: ",messagefromyesterday[i,"Rating"],"
Message: ", messagefromyesterday[i,"Review"], "
Date: ", messagefromyesterday[i,"Date"]))
    }
}else{
   textSlackr(paste0("There is no review from ", Sys.Date()-1, " ", weekdays.Date(Sys.Date()-1)))
}

messagefromlastweek <- filter(messsage, Date > Sys.Date()-7)
if(weekdays.Date(Sys.Date()) = "Thursday" & unique(messagefromlastweek$Date){
  textSlackr(paste0("Reviews from last week:"))
  for(i in 1:length(messagefromlastweek)){
    textSlackr(paste0(messagefromlastweek[i,"Platform"],"
Rating: ",messagefromlastweek[i,"Rating"],"
Message: ", messagefromlastweek[i,"Review"], "
Date: ", messagefromlastweek[i,"Date"]))
  }
}else{
  textSlackr(paste0("There is no review from last week"))
}
# install.packages("miniUI")
# install.packages("shiny")
# install.packages("shinyFiles")

# f <- system.file("/Users/yusufhancer/Desktop/AppStore_Ratings.R")
# cmd <- cron_rscript(f)
# # cron_add(command = cmd, frequency = 'minutely', id = 'minuteTest', description = 'My process 1')
# cron_rm(id="deneme")

# cron_add(command = cmd, frequency = 'daily', at= "9:30" ,days_of_week = c(1, 2, 3, 4, 5), id = 'TriggerfromComp', description = 'My process 1')

