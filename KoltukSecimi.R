require(lubridate)
require(ggplot2)
require("ggpubr")
koltuk <- read.csv("/Users/yusufhancer/Desktop/koltuk.csv")
koltuk$Ulasim_Yolu <- NULL
koltuk <- koltuk[37:136,]

for(i in seq(1,length(koltuk$Hafta)/4,by=1)){
  koltuk[seq(4*(i-1)+1,4*i,by=1),"Hafta"] <- as.Date(as.numeric(as.Date("2019-01-01") %m+% months(i-1)),origin="1970-01-01")
}
koltuk$Hafta <- as.Date(as.numeric(koltuk$Hafta),origin="1970-01-01")
summaryOfRates <-function(set){
  a<-ggplot(data = set, aes(x = Hafta, y = Sefer_Secimi/Sefer_Listeleme, group=Kanal)) +
    geom_line(aes(color=Kanal)) +
    labs(x = "Date",
         y = "Sefer Secimi Oranı")
  b<-ggplot(data = set, aes(x = Hafta, y = Koltuk_Secimi/Sefer_Secimi, group=Kanal)) +
    geom_line(aes(color=Kanal)) +
    labs(x = "Date",
         y = "Koltuk Secimi Oranı")
  c<-ggplot(data = set, aes(x = Hafta, y = Odeme_Sayfasi_Goruntuleme/Koltuk_Secimi, group=Kanal)) +
    geom_line(aes(color=Kanal)) +
    labs(x = "Date",
         y = "Odeme Sayfası Goruntuleme Oranı")
  
  ggarrange(a, b, c + rremove("x.text"), 
            labels = c("Sefer Secimi Oranı", "Koltuk Secimi Oranı", "Odeme Sayfası Goruntuleme Oranı"),
            ncol = 1, nrow = 3)
}
summaryOfNum <-function(set){
  a<-ggplot(data = set, aes(x = Hafta, y = Sefer_Listeleme, group=Kanal)) +
    geom_line(aes(color=Kanal)) +
    labs(x = "Date",
         y = "Sefer Listeleme")
  b<-ggplot(data = set, aes(x = Hafta, y = Sefer_Secimi, group=Kanal)) +
    geom_line(aes(color=Kanal)) +
    labs(x = "Date",
         y = "Sefer Secimi")
  c<-ggplot(data = set, aes(x = Hafta, y = Koltuk_Secimi , group=Kanal)) +
    geom_line(aes(color=Kanal)) +
    labs(x = "Date",
         y = "Koltuk Secimi")
  d<-ggplot(data = set, aes(x = Hafta, y = Odeme_Sayfasi_Goruntuleme , group=Kanal)) +
    geom_line(aes(color=Kanal)) +
    labs(x = "Date",
         y = "Odeme Sayfasi Goruntuleme")
  
  ggarrange(a, b, c,d + rremove("x.text"), 
            labels = c("Sefer Listeleme","Sefer Secimi", "Koltuk Secimi", "Odeme Sayfası Goruntuleme"),
            ncol = 2, nrow = 2)
}
look <- koltuk[koltuk$Hafta >= as.Date("2020-02-01"),]
summaryOfRates(koltuk)
summaryOfRates(look)
summaryOfNum(koltuk)
summaryOfNum(look)
