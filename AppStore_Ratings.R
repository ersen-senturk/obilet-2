#itunesr gerekli
#devtools::install_github("amrrs/itunesr")
require("itunesr")
obiletID<-596039443 #https://apps.apple.com/tr/app/obilet-otob%C3%BCs-ve-u%C3%A7ak-bileti/id596039443?l=tr
#override the getReviews function in the package as the function neglects the first entry
getReviews <- function(app_id,country,page_num){
  json_url <- paste0('https://itunes.apple.com/',
                     country,
                     '/rss/customerreviews/page=',
                     page_num,
                     '/id=',
                     app_id,
                     '/sortby=mostrecent/',
                     'json')
  xml_url <- paste0('https://itunes.apple.com/',
                    country,
                    '/rss/customerreviews/page=',
                    page_num,
                    '/id=',
                    app_id,
                    '/sortby=mostrecent/',
                    'xml')
  js <- jsonlite::fromJSON(json_url)
  reviews <- cbind(Title = js$feed$entry$title$label,
                   Author_URL = js$feed$entry$author$uri,
                   Author_Name = js$feed$entry$author$name,
                   App_Version = js$feed$entry$`im:version`$label,
                   Rating = js$feed$entry$`im:rating`$label,
                   Review = js$feed$entry$content$label)
  names(reviews) <- c('Title','Author_URL','Author_Name','App_Version','Rating','Review')
  #reading xml for date
  xml_n <- xml2::read_xml(xml_url)
  entries <- xml2::xml_children(xml_n)[xml2::xml_name(xml2::xml_children(xml_n))=='entry']
  date <- xml2::xml_text(
    xml2::xml_children(entries))[xml2::xml_name(xml2::xml_children(entries))=='updated']
  reviews$Date <- as.POSIXct(
    lubridate::with_tz(
      strptime(date,format='%Y-%m-%dT%H:%M:%S',tz='America/Los_Angeles'),
      tzone='Europe/London'))
  reviews$Title <- as.character(reviews$Title)
  reviews$Review <- as.character(reviews$Review)
  rownames(reviews) <- NULL
  return(reviews)
}

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
obilet_reviews_dummy_us <- getReviews(obiletID,'us',1)
obilet_reviews_dummy_us$Date<-as.Date(as.character(as.POSIXct(obilet_reviews_dummy_us$Date)))
obilet_reviews_dummy_us$App_Version <-as.character(obilet_reviews_dummy_us$App_Version)
obilet_us<-rbind.fill(obilet_us,obilet_reviews_dummy_us)
obilet_us$Country<-"USA"
obilet_reviews_us<-obilet_us
obilet_reviews_us$Date<-as.Date(obilet_reviews_us$Date, origin="1970-01-01")

#Germany
obilet_reviews_dummy_de <- getReviews(obiletID,'de',1)
obilet_reviews_dummy_de$Date<-as.Date(as.character(as.POSIXct(obilet_reviews_dummy_de$Date)))
obilet_reviews_dummy_de$App_Version <-as.character(obilet_reviews_dummy_de$App_Version)
obilet_de<-rbind.fill(obilet_de,obilet_reviews_dummy_de)
obilet_de$Country<-"Germany"
obilet_reviews_de<-obilet_de
obilet_reviews_de$Date<-as.Date(obilet_reviews_de$Date, origin="1970-01-01")

#Total
obilet_reviews<-rbind.fill(obilet_reviews_tr,obilet_reviews_us,obilet_reviews_de)
#require(googlesheets4)
# sheet_write(obilet_reviews,
#             ss="https://docs.google.com/spreadsheets/d/1zNyDFlo9wJdtjGV9SzRp8z7TOctUyxV4gsi8tqAICnI/edit#gid=0",
#             sheet="ios_reviews2")

message <- obilet_reviews[c("Date","Rating","Review")]
message$Platform <- "iOS"

require(lubridate)
library(tidyverse)
aaa <- readLines("/Users/yusufhancer/Desktop/android.txt") %>% str_split(";&;")
android <- matrix(unlist(aaa),ncol=3,byrow=T)
android <- data.frame(android)
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

# eğer daha fazla comment alınmak isterse scrolldown yapılmalıydı python scriptte
# yapıldığı takdirde burda bir değişikliğe ihtiyaç yok.
# eğer ileride başka bir link içinde bu yapı kullanılacaksa bu kısımda güncellenmeli
android[seq(nrow(android)/2+1,nrow(android),by=1),1] <- as.Date(android[seq(nrow(android)/2+1,nrow(android),by=1),1], format="%d-%m-%Y")
android[seq(1,nrow(android)/2,by=1),1] <- as.Date(android[seq(1,nrow(android)/2,by=1),1], format='%B %d, %Y')
date <- android[,1]
date <- as.numeric(date)
date <- as.Date(as.POSIXct(date*24*60*60, origin = "1970-01-01", tz="UTC"))
android[,1] <- date

message <- rbind.fill(message,android)
message$Rating <- ifelse(message[,"Rating"]=="1","★☆☆☆☆",
                     ifelse(message[,"Rating"]=="2","★★☆☆☆",
                        ifelse(message[,"Rating"]=="3","★★★☆☆",
                           ifelse(message[,"Rating"]=="4", "★★★★☆","★★★★★"))))
message <- message[order(message$Date,decreasing = T),]

# manual olarak çalıştırmadan değiştirdiğim kısım
n<-3
messagefromyesterday <- filter(message, Date > Sys.Date()-n)
title = paste0("Reviews from ", Sys.Date()-n+1, " ", weekdays.Date(Sys.Date()-n+1)," to today:")
noreview = paste0("There is no review from ", Sys.Date()-n+1, " ", weekdays.Date(Sys.Date()-n+1)," to today.")
#iOS App
# devtools::install_github('hrbrmstr/slackr')
require(slackr)
messagefromyesterdayiOS <- filter(messagefromyesterday, Platform == "iOS")
slackr_setup(channel = "#app-reviews",
             username= "ios_review",
             incoming_webhook_url = "https://hooks.slack.com/services/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",
             bot_user_oauth_token = 'xoxb-2893681624-1588164479511-KVdNdxJLjjiqTd7Mn5bERs8e')
if(nrow(messagefromyesterdayiOS) != 0){
  textSlackr(title)
  for(i in 1:nrow(messagefromyesterdayiOS)){
    textSlackr(paste0(messagefromyesterdayiOS[i,"Platform"],"
Date: ", messagefromyesterdayiOS[i,"Date"],"
Rating: ",messagefromyesterdayiOS[i,"Rating"],"
Message: ", messagefromyesterdayiOS[i,"Review"]))
  }
}else{
  textSlackr(noreview)
}

#Android App
messagefromyesterdayAndroid <- filter(messagefromyesterday, Platform == "Android")
slackr_setup(channel = "#app-reviews", username= "android_review",
             incoming_webhook_url = "https://hooks.slack.com/services/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",
             bot_user_oauth_token = "xoxb-2893681624-1614885992069-coa5DW04FnLq4A5HtGBEvZtm")
if(nrow(messagefromyesterdayAndroid) != 0){
  textSlackr(title)
  for(i in 1:nrow(messagefromyesterdayAndroid)){
    textSlackr(paste0(messagefromyesterdayAndroid[i,"Platform"],"
Date: ", messagefromyesterdayAndroid[i,"Date"],"
Rating: ",messagefromyesterdayAndroid[i,"Rating"],"
Message: ", messagefromyesterdayAndroid[i,"Review"]))
  }
}else{
  textSlackr(noreview)
}

