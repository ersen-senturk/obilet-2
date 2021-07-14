# install.packages("rvest")
# install.packages("xtable")
# install.packages("httr")
# install.packages("data.table")
require(rvest)
require(xtable)
require(httr)
require(data.table)
##Start of Rating Part
##1. Hafta
#authcode<-“YWsu_3zmBymykPxSwitQwpq6ij0”
##2. Hafta
#authcode<-“mRMqrbR_fCCvFKgi5Ew4tEwmpgA”
##3. Hafta
#authcode<-“9DTKDZa5CjXuFp9ronR8-KL1iYI”
##4. Hafta
#authcode<-“C44VPKNSgm3PU7ebCRtd4axDsDo”
##4. Hafta
##authcode<-“lQ0eX14x3RPETiBT6bMPLWJL2cE”
#authcode<-“lQ0eX14x3RPETiBT6bMPLWJL2cE”
#6uIpNbPlRoAOayxq7eCNX2Jn-RU
#596039443
#tarih parametrelerinin belirlenmesi (ağustos için örnek) api'de start_date=2020-07-31, end_date=2020_08_31; script'te base_date=2020-07-30 yapılır

authcode<-"YWsu_3zmBymykPxSwitQwpq6ij0"
ios_app_ratings <- GET("https://api.apptweak.com/ios/applications/596039443/ratings.json?country=tr&start_date=2021-5-31&end_date=2021-6-30", add_headers("X-Apptweak-Key"=authcode))
ratings<-content(ios_app_ratings)
obilet_ios_ratings_cumulative<-data.frame(matrix(ncol = 6, nrow = 0))
obilet_ios_ratings_daily<-data.frame(matrix(ncol = 6, nrow = 0))
colnames<-c("Date","OneStar","TwoStars","ThreeStars","FourStars","FiveStars")
colnames(obilet_ios_ratings_cumulative)<-colnames
colnames(obilet_ios_ratings_daily)<-colnames
obilet_ios_ratings_cumulative$Date<-as.Date(obilet_ios_ratings_cumulative$Date, origin="1970-01-01")
obilet_ios_ratings_daily$Date<-as.Date(obilet_ios_ratings_daily$Date, origin="1970-01-01")

base_date<-"2021-5-30"
dolu_max<-1
days_covered <- length(ratings[["content"]][["ratings"]])
for(i in 1:days_covered) {
  print(i)
  obilet_ios_ratings_cumulative[i, "Date"]<-as.Date(base_date) + i
  if(length(ratings[["content"]][["ratings"]][[i]])==0) {
    current_date_value = ratings[["content"]][["ratings"]][[dolu_max]]
  } else {
    current_date_value = ratings[["content"]][["ratings"]][[i]]
    dolu_max<-i
  } 
  if(i == 1) {
    obilet_ios_ratings_cumulative[i, "OneStar"]<- current_date_value[["1"]]
    obilet_ios_ratings_cumulative[i, "TwoStars"]<-current_date_value[["2"]]
    obilet_ios_ratings_cumulative[i, "ThreeStars"]<-current_date_value[["3"]]
    obilet_ios_ratings_cumulative[i, "FourStars"]<-current_date_value[["4"]]
    obilet_ios_ratings_cumulative[i, "FiveStars"]<-current_date_value[["5"]]
  } 
  # 1. günde bir hata olma ihtimali yok mu?
  else{
    if(current_date_value[["1"]] - obilet_ios_ratings_cumulative[[i-1,"OneStar"]] < 0) {
      obilet_ios_ratings_cumulative[i, "OneStar"] <- obilet_ios_ratings_cumulative[[i-1,"OneStar"]]
    } else {
      obilet_ios_ratings_cumulative[i, "OneStar"] <- current_date_value[["1"]]
    }
    if(current_date_value[["2"]] - obilet_ios_ratings_cumulative[[i-1,"TwoStars"]] < 0) {
      obilet_ios_ratings_cumulative[i, "TwoStars"] <- obilet_ios_ratings_cumulative[[i-1,"TwoStars"]]
    } else {
      obilet_ios_ratings_cumulative[i, "TwoStars"] <- current_date_value[["2"]]
    }
    if(current_date_value[["3"]] - obilet_ios_ratings_cumulative[[i-1,"ThreeStars"]] < 0) {
      obilet_ios_ratings_cumulative[i, "ThreeStars"] <- obilet_ios_ratings_cumulative[[i-1,"ThreeStars"]]
    } else {
      obilet_ios_ratings_cumulative[i, "ThreeStars"] <- current_date_value[["3"]]
    }
    if(current_date_value[["4"]] - obilet_ios_ratings_cumulative[[i-1,"FourStars"]] < 0) {
      obilet_ios_ratings_cumulative[i, "FourStars"] <- obilet_ios_ratings_cumulative[[i-1,"FourStars"]]
    } else {
      obilet_ios_ratings_cumulative[i, "FourStars"] <- current_date_value[["4"]]
    }
    if(current_date_value[["5"]] - obilet_ios_ratings_cumulative[[i-1,"FiveStars"]] < 0) {
      obilet_ios_ratings_cumulative[i, "FiveStars"] <- obilet_ios_ratings_cumulative[[i-1,"FiveStars"]]
    } else {
      obilet_ios_ratings_cumulative[i, "FiveStars"] <- current_date_value[["5"]]
    }
  }
}

for(i in 1:(days_covered-1)) {
  print(i)
  obilet_ios_ratings_daily[i, "Date"]<-obilet_ios_ratings_cumulative[i+1, "Date"]
  obilet_ios_ratings_daily[i, "OneStar"]<-obilet_ios_ratings_cumulative[i+1, "OneStar"]-obilet_ios_ratings_cumulative[i, "OneStar"]
  obilet_ios_ratings_daily[i, "TwoStars"]<-obilet_ios_ratings_cumulative[i+1, "TwoStars"]-obilet_ios_ratings_cumulative[i, "TwoStars"]
  obilet_ios_ratings_daily[i, "ThreeStars"]<-obilet_ios_ratings_cumulative[i+1, "ThreeStars"]-obilet_ios_ratings_cumulative[i, "ThreeStars"]
  obilet_ios_ratings_daily[i, "FourStars"]<-obilet_ios_ratings_cumulative[i+1, "FourStars"]-obilet_ios_ratings_cumulative[i, "FourStars"]
  obilet_ios_ratings_daily[i, "FiveStars"]<-obilet_ios_ratings_cumulative[i+1, "FiveStars"]-obilet_ios_ratings_cumulative[i, "FiveStars"]
}

##Ratings Sinem Update - END
require(googlesheets4)
sheet_write(obilet_ios_ratings_cumulative,
            ss="https://docs.google.com/spreadsheets/d/1zNyDFlo9wJdtjGV9SzRp8z7TOctUyxV4gsi8tqAICnI/edit#gid=0",
            sheet="ios_cumulative_Haziran2021")
sheet_write(obilet_ios_ratings_daily,
            ss="https://docs.google.com/spreadsheets/d/1zNyDFlo9wJdtjGV9SzRp8z7TOctUyxV4gsi8tqAICnI/edit#gid=0",
            sheet="ios_daily-ios_cumulative_Haziran2021")
