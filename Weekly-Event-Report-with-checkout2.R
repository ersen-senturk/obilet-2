require(googlesheets)
require(googlesheets4)
require(reshape)
require(plyr)
require(dplyr)
require(googleAnalyticsR)


start<-"2021-2-15"
end<-"2021-2-21"
week<-as.Date("15/2/2021", format="%d/%m/%Y")
ga_id<-63089351
ga_id_android<-73607140
ga_id_ios<-72226277


#1. Web Genel (Otobüs - Uçak)
my_filter1<-dim_filter(dimension = "eventCategory",operator = "REGEXP",expressions = "funnel")
my_filter2<-dim_filter(dimension = "eventCategory",operator = "REGEXP",expressions =  "Payment Success")
my_filter3<-dim_filter(dimension = "eventCategory",operator = "REGEXP",expressions =  "MasterPass")
my_filter_clause<-filter_clause_ga4(list(my_filter1, my_filter2, my_filter3),operator = "OR")

data1<-google_analytics(ga_id,
                        date_range = c(start,end),
                        metrics = c("ga:uniqueEvents", "ga:totalEvents"),
                        dimension=c("eventCategory","eventAction","eventLabel","deviceCategory"), anti_sample = T,
                        dim_filters = my_filter_clause)

unique(data1$eventCategory) #kontrol kodu

##1.1. Web otobüs
data_web_otobus<-subset(data1,(data1$eventAction%in%c("viewJourneys","clickJourney","addToCart","checkout-1","checkout-2","purchase")&data1$deviceCategory!="tablet")|
                          (data1$eventCategory=="Payment Success - Bus"&data1$eventLabel%in%c("PurchaseSuccess","Registered User Detected","Loggedin User Detected")&data1$deviceCategory!="tablet")|
                          (data1$eventCategory=="MasterPass"&data1$eventLabel=="Successfull payment MasterPass"&data1$deviceCategory!="tablet"))

data_web_otobus$eventLabel[data_web_otobus$eventLabel == "(not set)"] <- data_web_otobus$eventAction[data_web_otobus$eventLabel == "(not set)"]
data_web_otobus_unique<-cast(data_web_otobus, deviceCategory~eventLabel, value = "uniqueEvents")
data_web_otobus_total<-cast(data_web_otobus, deviceCategory~eventLabel, value="totalEvents")

### Purchase Success = Sigorta, purchase = Otobüs Bilet, Successful payment Masterpass = Masterpass
data_web_otobus_total_gerekliler<-data_web_otobus_total[c("deviceCategory","purchase", "PurchaseSuccess","Successfull payment MasterPass")] 
data_web_otobus_unique_gerekliler<-data_web_otobus_unique[c("deviceCategory", "viewJourneys", "clickJourney", "addToCart", "checkout-1", "checkout-2", "purchase", "PurchaseSuccess","Loggedin User Detected","Registered User Detected", "Successfull payment MasterPass")]
data_web_otobus_hepsi<-merge(data_web_otobus_unique_gerekliler, data_web_otobus_total_gerekliler, by="deviceCategory", all.x = T)

data_web_otobus_hepsi$Ulasim_Yolu<-"Otobus"
data_web_otobus_hepsi$Hafta<-as.Date(week, "%d/%m/%Y")
data_web_otobus_hepsi$Anonim_Odeme<-data_web_otobus_hepsi$purchase.x-(ifelse(is.na(data_web_otobus_hepsi$`Loggedin User Detected`),0,data_web_otobus_hepsi$`Loggedin User Detected`)+
                                                                        ifelse(is.na(data_web_otobus_hepsi$`Registered User Detected`),0,data_web_otobus_hepsi$`Registered User Detected`))

data_web_otobus_final<-data_web_otobus_hepsi[c("Ulasim_Yolu", "Hafta","deviceCategory", "viewJourneys", "clickJourney", "addToCart", "checkout-1","checkout-2", "purchase.x", "PurchaseSuccess.x","Loggedin User Detected","Registered User Detected","Anonim_Odeme", "Successfull payment MasterPass.x","purchase.y","PurchaseSuccess.y","Successfull payment MasterPass.y")]
names(data_web_otobus_final)<-c("Ulasim_Yolu", "Hafta", "Kanal", "Sefer_Listeleme", "Sefer_Secimi","Koltuk_Secimi", "Odeme_Sayfasi_Goruntuleme","Odeme_Islemi_Istegi", "Odeme", "Sigorta", "Uye_Odeme", "Uye_Anonim_Odeme","Anonim_Odeme", "MasterPass_Odeme", "Total_Odeme", "Total_Sigorta","Total_MasterPass_Odeme")

##1.2 Web Ucak w/o ödeme sayfası, ödeme ve sigorta
data_web_flight_odemesiz<-subset(data1,data1$eventAction%in%c("listDomesticOneWayFlights","listDomesticTwoWayFlights",
                                                              "listInternationalFlights","selectDomesticOneWayFlight"
                                                              ,"selectDomesticOutboundFlight",
                                                              "selectDomesticReturnFlight","selectInternationalFlight")&data1$deviceCategory!="tablet")

###1.2.1. DomesticTY
data_web_domesticTY<-subset(data_web_flight_odemesiz, data_web_flight_odemesiz$eventAction%in%c("listDomesticOneWayFlights", "selectDomesticOneWayFlight"))
data_web_domesticTY_final<-cast(data_web_domesticTY, deviceCategory~eventAction, value="uniqueEvents")

data_web_domesticTY_final$Ulasim_Yolu<-"Ucak_Domestic_Tek_Yon"
data_web_domesticTY_final$Hafta<-as.Date(week, "%d/%m/%Y")

data_web_domesticTY_final<-data_web_domesticTY_final[c("Ulasim_Yolu", "Hafta", "deviceCategory", "listDomesticOneWayFlights", "selectDomesticOneWayFlight")]
names(data_web_domesticTY_final)<-c("Ulasim_Yolu", "Hafta", "Kanal", "Sefer_Listeleme", "Sefer_Secimi")

###1.2.2. DomesticCY
data_web_domesticCY<-subset(data_web_flight_odemesiz, data_web_flight_odemesiz$eventAction%in%c("listDomesticTwoWayFlights", "selectDomesticOutboundFlight","selectDomesticReturnFlight"))
data_web_domesticCY_final<-cast(data_web_domesticCY, deviceCategory~eventAction, value = "uniqueEvents")

data_web_domesticCY_final$Ulasim_Yolu<-"Ucak_Domestic_Gidis_Donus"
data_web_domesticCY_final$Hafta<-as.Date(week, "%d/%m/%Y")

data_web_domesticCY_final<-data_web_domesticCY_final[c("Ulasim_Yolu", "Hafta", "deviceCategory", "listDomesticTwoWayFlights", "selectDomesticOutboundFlight","selectDomesticReturnFlight")]
names(data_web_domesticCY_final)<-c("Ulasim_Yolu", "Hafta", "Kanal", "Sefer_Listeleme", "Gidis_Sefer_Secimi", "Donus_Sefer_Secimi")

###1.2.3. International
data_web_international<-subset(data_web_flight_odemesiz, data_web_flight_odemesiz$eventAction%in%c("listInternationalFlights", "selectInternationalFlight"))
data_web_international_final<-cast(data_web_international, deviceCategory~eventAction, value="uniqueEvents")

data_web_international_final$Ulasim_Yolu<-"Ucak_International"
data_web_international_final$Hafta<-as.Date(week, "%d/%m/%Y")

data_web_international_final<-data_web_international_final[c("Ulasim_Yolu", "Hafta", "deviceCategory", "listInternationalFlights", "selectInternationalFlight" )]
names(data_web_international_final)<-c("Ulasim_Yolu", "Hafta", "Kanal", "Sefer_Listeleme", "Sefer_Secimi")

###1.2.4. Web Ucak w/o ödeme sayfası ve ödeme final
data_web_ucak_odemesiz<-rbind.fill(data_web_domesticTY_final, data_web_domesticCY_final, data_web_international_final)

data_web_ucak_odemesiz$Kanal<-gsub("mobile", "1. Mobile_Web", data_web_ucak_odemesiz$Kanal)
data_web_ucak_odemesiz$Kanal<-gsub("desktop", "2. Desktop", data_web_ucak_odemesiz$Kanal)


#2. Android Genel (Otobüs - Uçak) İleride Insurance Payment Success-Bus altına gelecek ve
#Payment Success-Bus altından üye mi değil mi bilgisine bakılacak

my_filter1_android<-dim_filter(dimension = "eventCategory",operator = "REGEXP",expressions = "funnel")
my_filter2_android<-dim_filter(dimension = "eventCategory",operator = "REGEXP",expressions =  "Payment Success")
my_filter3_android<-dim_filter(dimension = "eventCategory",operator = "REGEXP",expressions =  "MasterPass")
my_filter_clause_android<-filter_clause_ga4(list(my_filter1_android, my_filter2_android, my_filter3_android),operator = "OR")

data1_android<-google_analytics(ga_id_android,
                                date_range = c(start,end),
                                metrics = c("ga:uniqueEvents", "ga:totalEvents"),
                                dimension=c("eventCategory","eventAction","eventLabel"), anti_sample = T,
                                dim_filters = my_filter_clause_android)

unique(data1_android$eventCategory)

##2.1. Android otobüs
data_android_otobus<-subset(data1_android,(data1_android$eventAction%in%c("viewJourneys","clickJourney","addToCart","checkout-1","checkout-2","purchase"))|
                              (data1_android$eventCategory=="Payment Success - Bus"&data1_android$eventLabel%in%c("PurchaseSuccess","Registered User Detected","Loggedin User Detected"))|
                              (data1_android$eventCategory=="MasterPass"&data1_android$eventLabel=="Successful payment MasterPass"))

data_android_otobus$eventLabel[data_android_otobus$eventLabel == "(not set)"] <- data_android_otobus$eventAction[data_android_otobus$eventLabel == "(not set)"]
data_android_otobus$Ulasim_Yolu<-"Otobus"
data_android_otobus$Hafta<-as.Date(week, "%d/%m/%Y")
data_android_otobus$Kanal<-"Android-New"

data_android_otobus_unique<-cast(data_android_otobus, Ulasim_Yolu+Hafta+Kanal~eventLabel, value = "uniqueEvents")
data_android_otobus_total<-cast(data_android_otobus, Ulasim_Yolu+Hafta+Kanal~eventLabel, value="totalEvents")

data_android_otobus_total_gerekliler<-data_android_otobus_total[c( "Ulasim_Yolu","Hafta","Kanal", "purchase", "PurchaseSuccess","Successful payment MasterPass")] 
data_android_otobus_unique_gerekliler<-data_android_otobus_unique[c("Ulasim_Yolu","Hafta","Kanal", "viewJourneys", "clickJourney", "addToCart", "checkout-1", "checkout-2","purchase", "PurchaseSuccess","Loggedin User Detected","Registered User Detected", "Successful payment MasterPass")]
data_android_otobus_hepsi<-merge(data_android_otobus_unique_gerekliler, data_android_otobus_total_gerekliler, by=c("Ulasim_Yolu","Hafta", "Kanal"), all.x = T)

data_android_otobus_hepsi$Anonim_Odeme<-data_android_otobus_hepsi$purchase.x-(ifelse(is.na(data_android_otobus_hepsi$`Loggedin User Detected`),0,data_android_otobus_hepsi$`Loggedin User Detected`)+
                                                                                ifelse(is.na(data_android_otobus_hepsi$`Registered User Detected`),0,data_android_otobus_hepsi$`Registered User Detected`))

data_android_otobus_final<-data_android_otobus_hepsi[c("Ulasim_Yolu", "Hafta", "Kanal" ,"viewJourneys", "clickJourney", "addToCart", "checkout-1", "checkout-2", "purchase.x", "PurchaseSuccess.x", "Loggedin User Detected","Registered User Detected","Anonim_Odeme", "Successful payment MasterPass.x", "purchase.y", "PurchaseSuccess.y","Successful payment MasterPass.y")]
names(data_android_otobus_final)<-c("Ulasim_Yolu", "Hafta", "Kanal", "Sefer_Listeleme", "Sefer_Secimi","Koltuk_Secimi", "Odeme_Sayfasi_Goruntuleme","Odeme_Islemi_Istegi" ,"Odeme", "Sigorta", "Uye_Odeme", "Uye_Anonim_Odeme","Anonim_Odeme", "MasterPass_Odeme", "Total_Odeme", "Total_Sigorta","Total_MasterPass_Odeme")


##2.2 Android Ucak w/o ödeme sayfası, ödeme ve sigorta
data_android_flight_odemesiz<-subset(data1_android,data1_android$eventAction%in%c("listDomesticOneWayFlights","listDomesticTwoWayFlights",
                                                                                  "listInternationalFlights","selectDomesticOneWayFlight"
                                                                                  ,"selectDomesticOutboundFlight",
                                                                                  "selectDomesticReturnFlight","selectInternationalFlight"))


###2.2.1. Android DomesticTY
data_android_domesticTY<-subset(data_android_flight_odemesiz, data_android_flight_odemesiz$eventAction%in%c("listDomesticOneWayFlights", "selectDomesticOneWayFlight"))

data_android_domesticTY$Ulasim_Yolu<-"Ucak_Domestic_Tek_Yon"
data_android_domesticTY$Hafta<-as.Date(week, "%d/%m/%Y")
data_android_domesticTY$Kanal<-"3. Android-New"

data_android_domesticTY_final<-cast(data_android_domesticTY, Ulasim_Yolu+Hafta+Kanal~eventAction, value="uniqueEvents")

data_android_domesticTY_final<-data_android_domesticTY_final[c("Ulasim_Yolu", "Hafta", "Kanal", "listDomesticOneWayFlights", "selectDomesticOneWayFlight")]
names(data_android_domesticTY_final)<-c("Ulasim_Yolu", "Hafta", "Kanal", "Sefer_Listeleme", "Sefer_Secimi")

###2.2.2. Android DomesticCY
data_android_domesticCY<-subset(data_android_flight_odemesiz, data_android_flight_odemesiz$eventAction%in%c("listDomesticTwoWayFlights", "selectDomesticOutboundFlight","selectDomesticReturnFlight"))

data_android_domesticCY$Ulasim_Yolu<-"Ucak_Domestic_Gidis_Donus"
data_android_domesticCY$Hafta<-as.Date(week, "%d/%m/%Y")
data_android_domesticCY$Kanal<-"3. Android-New"

data_android_domesticCY_final<-cast(data_android_domesticCY, Ulasim_Yolu+Hafta+Kanal~eventAction, value="uniqueEvents")

data_android_domesticCY_final<-data_android_domesticCY_final[c("Ulasim_Yolu", "Hafta", "Kanal", "listDomesticTwoWayFlights", "selectDomesticOutboundFlight","selectDomesticReturnFlight")]
names(data_android_domesticCY_final)<-c("Ulasim_Yolu", "Hafta", "Kanal", "Sefer_Listeleme", "Gidis_Sefer_Secimi", "Donus_Sefer_Secimi")

###2.2.3. Android International
data_android_international<-subset(data_android_flight_odemesiz, data_android_flight_odemesiz$eventAction%in%c("listInternationalFlights", "selectInternationalFlight"))

data_android_international$Ulasim_Yolu<-"Ucak_International"
data_android_international$Hafta<-as.Date(week, "%d/%m/%Y")
data_android_international$Kanal<-"3. Android-New"

data_android_international_final<-cast(data_android_international, Ulasim_Yolu+Hafta+Kanal~eventAction, value="uniqueEvents")

data_android_international_final<-data_android_international_final[c("Ulasim_Yolu", "Hafta", "Kanal", "listInternationalFlights", "selectInternationalFlight" )]
names(data_android_international_final)<-c("Ulasim_Yolu", "Hafta", "Kanal", "Sefer_Listeleme", "Sefer_Secimi")

###2.2.4. Ucak Android w/o ödeme sayfası, ödeme ve sigorta final
data_android_ucak_odemesiz<-rbind.fill(data_android_domesticTY_final, data_android_domesticCY_final, data_android_international_final)

#3. Ios Genel (Otobüs - Uçak)
my_filter1_ios<-dim_filter(dimension = "eventCategory",operator = "REGEXP",expressions = "funnel")
my_filter2_ios<-dim_filter(dimension = "eventCategory",operator = "REGEXP",expressions =  "Payment Success")
my_filter3_ios<-dim_filter(dimension = "eventCategory",operator = "REGEXP",expressions =  "MasterPass")
my_filter_clause_ios<-filter_clause_ga4(list(my_filter1_ios, my_filter2_ios, my_filter3_ios),operator = "OR")

data1_ios<-google_analytics(ga_id_ios,
                            date_range = c(start,end),
                            metrics = c("ga:uniqueEvents", "ga:totalEvents"),
                            dimension=c("eventCategory","eventAction","eventLabel"), anti_sample = T,
                            dim_filters = my_filter_clause_ios)

unique(data1_ios$eventCategory)


##3.1. Ios otobüs
data_ios_otobus<-subset(data1_ios,(data1_ios$eventAction%in%c("viewJourneys","clickJourney","addToCart","checkout-1","checkout-2","purchase"))|
                          (data1_ios$eventCategory=="Payment Success - Bus"&data1_ios$eventLabel%in%c("PurchaseSuccess","Registered User Detected","Loggedin User Detected"))|
                          (data1_ios$eventCategory=="MasterPass"&data1_ios$eventLabel=="Successfully payment MasterPass"))

data_ios_otobus$eventLabel[data_ios_otobus$eventLabel == "(not set)"] <- data_ios_otobus$eventAction[data_ios_otobus$eventLabel == "(not set)"]
data_ios_otobus$Ulasim_Yolu<-"Otobus"
data_ios_otobus$Hafta<-as.Date(week, "%d/%m/%Y")
data_ios_otobus$Kanal<-"Ios-New"

data_ios_otobus_unique<-cast(data_ios_otobus, Ulasim_Yolu+Hafta+Kanal~eventLabel, value = "uniqueEvents")
data_ios_otobus_total<-cast(data_ios_otobus, Ulasim_Yolu+Hafta+Kanal~eventLabel, value="totalEvents")

data_ios_otobus_total_gerekliler<-data_ios_otobus_total[c( "Ulasim_Yolu","Hafta","Kanal", "purchase", "PurchaseSuccess","Successfully payment MasterPass")] 
data_ios_otobus_unique_gerekliler<-data_ios_otobus_unique[c("Ulasim_Yolu","Hafta","Kanal", "viewJourneys", "clickJourney", "addToCart", "checkout-1","checkout-2", "purchase", "PurchaseSuccess","Loggedin User Detected","Registered User Detected", "Successfully payment MasterPass")]
data_ios_otobus_hepsi<-merge(data_ios_otobus_unique_gerekliler, data_ios_otobus_total_gerekliler, by=c("Ulasim_Yolu","Hafta", "Kanal"), all.x = T)

data_ios_otobus_hepsi$Anonim_Odeme<-data_ios_otobus_hepsi$purchase.x-(ifelse(is.na(data_ios_otobus_hepsi$`Loggedin User Detected`),0,data_ios_otobus_hepsi$`Loggedin User Detected`)+
                                                                        ifelse(is.na(data_ios_otobus_hepsi$`Registered User Detected`),0,data_ios_otobus_hepsi$`Registered User Detected`))

data_ios_otobus_final<-data_ios_otobus_hepsi[c("Ulasim_Yolu", "Hafta", "Kanal" ,"viewJourneys", "clickJourney", "addToCart", "checkout-1","checkout-2", "purchase.x", "PurchaseSuccess.x", "Loggedin User Detected","Registered User Detected","Anonim_Odeme", "Successfully payment MasterPass.x", "purchase.y", "PurchaseSuccess.y","Successfully payment MasterPass.y")]
names(data_ios_otobus_final)<-c("Ulasim_Yolu", "Hafta", "Kanal", "Sefer_Listeleme", "Sefer_Secimi","Koltuk_Secimi", "Odeme_Sayfasi_Goruntuleme","Odeme_Islemi_Istegi" ,"Odeme", "Sigorta", "Uye_Odeme", "Uye_Anonim_Odeme","Anonim_Odeme", "MasterPass_Odeme", "Total_Odeme", "Total_Sigorta","Total_MasterPass_Odeme")

##3.2 Ios Ucak w/o ödeme sayfası, ödeme ve sigorta
data_ios_flight_odemesiz<-subset(data1_ios,data1_ios$eventAction%in%c("listDomesticOneWayFlights","listDomesticTwoWayFlights",
                                                                      "listInternationalFlights","selectDomesticOneWayFlight"
                                                                      ,"selectDomesticOutboundFlight",
                                                                      "selectDomesticReturnFlight","selectInternationalFlight"))

###3.2.1. Ios DomesticTY
data_ios_domesticTY<-subset(data_ios_flight_odemesiz, data_ios_flight_odemesiz$eventAction%in%c("listDomesticOneWayFlights", "selectDomesticOneWayFlight"))

data_ios_domesticTY$Ulasim_Yolu<-"Ucak_Domestic_Tek_Yon"
data_ios_domesticTY$Hafta<-as.Date(week, "%d/%m/%Y")
data_ios_domesticTY$Kanal<-"4. Ios-New"

data_ios_domesticTY_final<-cast(data_ios_domesticTY, Ulasim_Yolu+Hafta+Kanal~eventAction, value="uniqueEvents")

data_ios_domesticTY_final<-data_ios_domesticTY_final[c("Ulasim_Yolu", "Hafta", "Kanal", "listDomesticOneWayFlights", "selectDomesticOneWayFlight")]
names(data_ios_domesticTY_final)<-c("Ulasim_Yolu", "Hafta", "Kanal", "Sefer_Listeleme", "Sefer_Secimi")

###3.2.2. Ios DomesticCY
data_ios_domesticCY<-subset(data_ios_flight_odemesiz, data_ios_flight_odemesiz$eventAction%in%c("listDomesticTwoWayFlights", "selectDomesticOutboundFlight","selectDomesticReturnFlight"))

data_ios_domesticCY$Ulasim_Yolu<-"Ucak_Domestic_Gidis_Donus"
data_ios_domesticCY$Hafta<-as.Date(week, "%d/%m/%Y")
data_ios_domesticCY$Kanal<-"4. Ios-New"

data_ios_domesticCY_final<-cast(data_ios_domesticCY, Ulasim_Yolu+Hafta+Kanal~eventAction, value="uniqueEvents")

data_ios_domesticCY_final<-data_ios_domesticCY_final[c("Ulasim_Yolu", "Hafta", "Kanal", "listDomesticTwoWayFlights", "selectDomesticOutboundFlight","selectDomesticReturnFlight")]
names(data_ios_domesticCY_final)<-c("Ulasim_Yolu", "Hafta", "Kanal", "Sefer_Listeleme", "Gidis_Sefer_Secimi", "Donus_Sefer_Secimi")

###3.2.3. Ios International
data_ios_international<-subset(data_ios_flight_odemesiz, data_ios_flight_odemesiz$eventAction%in%c("listInternationalFlights", "selectInternationalFlight"))

data_ios_international$Ulasim_Yolu<-"Ucak_International"
data_ios_international$Hafta<-as.Date(week, "%d/%m/%Y")
data_ios_international$Kanal<-"4. Ios-New"

data_ios_international_final<-cast(data_ios_international, Ulasim_Yolu+Hafta+Kanal~eventAction, value="uniqueEvents")

data_ios_international_final<-data_ios_international_final[c("Ulasim_Yolu", "Hafta", "Kanal", "listInternationalFlights", "selectInternationalFlight" )]
names(data_ios_international_final)<-c("Ulasim_Yolu", "Hafta", "Kanal", "Sefer_Listeleme", "Sefer_Secimi")

###3.2.4. Ucak w/o ödeme sayfası, ödeme ve sigorta final (web, android, ios)
data_android_ucak_odemesiz<-rbind.fill(data_android_domesticTY_final, data_android_domesticCY_final, data_android_international_final)
data_ios_ucak_odemesiz<-rbind.fill(data_ios_domesticTY_final, data_ios_domesticCY_final, data_ios_international_final)
data_ucak_odemesiz<-rbind.fill(data_web_ucak_odemesiz, data_android_ucak_odemesiz, data_ios_ucak_odemesiz)

#4. Segmented Flight Web
my_filter1_web_sgm<-dim_filter(dimension = "eventCategory",operator = "REGEXP",expressions = "funnel")
my_filter2_web_sgm<-dim_filter(dimension = "eventCategory",operator = "REGEXP",expressions =  "Payment Success")
my_filter3_web_sgm<-dim_filter(dimension = "eventCategory",operator = "REGEXP",expressions =  "MasterPass")
my_filter_clause_web_sgm<-filter_clause_ga4(list(my_filter1_web_sgm, my_filter2_web_sgm, my_filter3_web_sgm),operator = "OR")

sg1<-segment_ga4("DomesticTY_web",segment_id = "gaid::EKno2FLAR7K9UsHWvt3pdA")
sg2<-segment_ga4("DomesticCY_web",segment_id = "gaid::pqoFI_1lTVW1_mEV62-0yw")
sg3<-segment_ga4("International_web", segment_id="gaid::8rXo3fWLQpWrLV06d6VAwA")

##4.1. Segmented Flight Web (DomesticTY)
data1_web_sgm1<-google_analytics(ga_id,
                                 date_range = c(start,end),
                                 metrics = c("ga:uniqueEvents", "ga:totalEvents"),
                                 dimension=c("eventCategory","eventAction","eventLabel","deviceCategory"),
                                 anti_sample = T,
                                 segments=sg1,
                                 dim_filters = my_filter_clause_web_sgm)
unique(data1_web_sgm1$eventCategory)
data1_web_sgm1_gerekliler<-subset(data1_web_sgm1, (data1_web_sgm1$eventAction%in%c("flightCheckout-1","flightCheckout-2","flightPurchase")&data1_web_sgm1$deviceCategory!="tablet")|
                                    (data1_web_sgm1$eventCategory=="Payment Success - Flight"&data1_web_sgm1$eventLabel%in%c("PurchaseSuccess","Loggedin User Detected","Registered User Detected")&data1_web_sgm1$deviceCategory!="tablet")|
                                    (data1_web_sgm1$eventCategory=="MasterPass"&data1_web_sgm1$eventLabel=="Successfull payment MasterPass"&data1_web_sgm1$deviceCategory!="tablet"))
data1_web_sgm1_gerekliler$Ulasim_Yolu<-"Ucak_Domestic_Tek_Yon"

##4.2. Segmented Flight Web (DomesticCY)
data1_web_sgm2<-google_analytics(ga_id,
                                 date_range = c(start,end),
                                 metrics = c("ga:uniqueEvents", "ga:totalEvents"),
                                 dimension=c("eventCategory","eventAction","eventLabel","deviceCategory"),
                                 anti_sample = T,
                                 segments=sg2,
                                 dim_filters = my_filter_clause_web_sgm)

data1_web_sgm2_gerekliler<-subset(data1_web_sgm2, (data1_web_sgm2$eventAction%in%c("flightCheckout-1","flightCheckout-2","flightPurchase")&data1_web_sgm2$deviceCategory!="tablet")|
                                    (data1_web_sgm2$eventCategory=="Payment Success - Flight"&data1_web_sgm2$eventLabel%in%c("PurchaseSuccess","Loggedin User Detected","Registered User Detected")&data1_web_sgm2$deviceCategory!="tablet")|
                                    (data1_web_sgm2$eventCategory=="MasterPass"&data1_web_sgm2$eventLabel=="Successfull payment MasterPass"&data1_web_sgm2$deviceCategory!="tablet"))
data1_web_sgm2_gerekliler$Ulasim_Yolu<-"Ucak_Domestic_Gidis_Donus"

##4.3. Segmented Flight Web (International)
data1_web_sgm3<-google_analytics(ga_id,
                                 date_range = c(start,end),
                                 metrics = c("ga:uniqueEvents", "ga:totalEvents"),
                                 dimension=c("eventCategory","eventAction","eventLabel","deviceCategory"),
                                 anti_sample = T,
                                 segments=sg3,
                                 dim_filters = my_filter_clause_web_sgm)

data1_web_sgm3_gerekliler<-subset(data1_web_sgm3, (data1_web_sgm3$eventAction%in%c("flightCheckout-1","flightCheckout-2","flightPurchase")&data1_web_sgm3$deviceCategory!="tablet")|
                                    (data1_web_sgm3$eventCategory=="Payment Success - Flight"&data1_web_sgm3$eventLabel%in%c("PurchaseSuccess","Loggedin User Detected","Registered User Detected")&data1_web_sgm3$deviceCategory!="tablet")|
                                    (data1_web_sgm3$eventCategory=="MasterPass"&data1_web_sgm3$eventLabel=="Successfull payment MasterPass"&data1_web_sgm3$deviceCategory!="tablet"))
data1_web_sgm3_gerekliler$Ulasim_Yolu<-"Ucak_International"

##4.4. Segmented Flight Web Hepsi
data_web_ucak_odemeli<-rbind.fill(data1_web_sgm1_gerekliler, data1_web_sgm2_gerekliler, data1_web_sgm3_gerekliler)

data_web_ucak_odemeli$Hafta<-as.Date(week, "%d/%m/%Y")

data_web_ucak_odemeli$eventLabel[data_web_ucak_odemeli$eventLabel == "(not set)"] <- data_web_ucak_odemeli$eventAction[data_web_ucak_odemeli$eventLabel == "(not set)"]
data_web_ucak_odemeli_unique<-cast(data_web_ucak_odemeli, Ulasim_Yolu+Hafta+deviceCategory~eventLabel, value = "uniqueEvents")
data_web_ucak_odemeli_total<-cast(data_web_ucak_odemeli, Ulasim_Yolu+Hafta+deviceCategory~eventLabel, value = "totalEvents")

if(!("flightPurchase" %in% unique(data_web_ucak_odemeli$eventLabel))){
  data_web_ucak_odemeli_unique[,"flightPurchase"] <- 0
  data_web_ucak_odemeli_total[,"flightPurchase"] <- 0
}
if(!("Successfull payment MasterPass" %in% unique(data_web_ucak_odemeli$eventLabel))){
  data_web_ucak_odemeli_unique[,"Successfull payment MasterPass"] <- 0
  data_web_ucak_odemeli_total[,"Successfull payment MasterPass"] <- 0
}
if(!("PurchaseSuccess" %in% unique(data_web_ucak_odemeli$eventLabel))){
  data_web_ucak_odemeli_unique[,"PurchaseSuccess"] <- 0
  data_web_ucak_odemeli_total[,"PurchaseSuccess"] <- 0
}

data_web_ucak_odemeli_total_gerekliler<-data_web_ucak_odemeli_total[c("Ulasim_Yolu", "Hafta","deviceCategory", "flightPurchase", "PurchaseSuccess","Successfull payment MasterPass")]
data_web_ucak_odemeli_unique_gerekliler<-data_web_ucak_odemeli_unique[c("Ulasim_Yolu","Hafta", "deviceCategory", "flightCheckout-1","flightCheckout-2", "flightPurchase", "PurchaseSuccess","Loggedin User Detected","Registered User Detected","Successfull payment MasterPass" )]

data_web_ucak_odemeli_final<-merge(data_web_ucak_odemeli_unique_gerekliler, data_web_ucak_odemeli_total_gerekliler, 
                                   by=c("Ulasim_Yolu","Hafta","deviceCategory"), all.x = T)

names(data_web_ucak_odemeli_final)<-c("Ulasim_Yolu","Hafta", "Kanal", "Odeme_Sayfasi_Goruntuleme", "Odeme_Islemi_Istegi","Odeme","Sigorta","Uye_Odeme","Uye_Anonim_Odeme","MasterPass_Odeme","Total_Odeme","Total_Sigorta","Total_MasterPass_Odeme")
data_web_ucak_odemeli_final$Anonim_Odeme<-data_web_ucak_odemeli_final$Odeme-(ifelse(is.na(data_web_ucak_odemeli_final$Uye_Odeme),0,data_web_ucak_odemeli_final$Uye_Odeme)+
                                                                               ifelse(is.na(data_web_ucak_odemeli_final$Uye_Anonim_Odeme),0,data_web_ucak_odemeli_final$Uye_Anonim_Odeme))
data_web_ucak_odemeli_final<-data_web_ucak_odemeli_final[c("Ulasim_Yolu","Hafta", "Kanal", "Odeme_Sayfasi_Goruntuleme", "Odeme_Islemi_Istegi", "Odeme","Sigorta","Uye_Odeme","Uye_Anonim_Odeme", "Anonim_Odeme", "MasterPass_Odeme","Total_Odeme","Total_Sigorta","Total_MasterPass_Odeme")]

data_web_ucak_odemeli_final$Kanal<-gsub("mobile", "1. Mobile_Web", data_web_ucak_odemeli_final$Kanal)
data_web_ucak_odemeli_final$Kanal<-gsub("desktop", "2. Desktop", data_web_ucak_odemeli_final$Kanal)

#5. Segmented Flight Android
my_filter1_android_sgm<-dim_filter(dimension = "eventCategory",operator = "REGEXP",expressions = "funnel")
my_filter2_android_sgm<-dim_filter(dimension = "eventCategory",operator = "REGEXP",expressions = "Payment Success")
my_filter3_android_sgm<-dim_filter(dimension = "eventCategory",operator = "REGEXP",expressions = "MasterPass")
my_filter_clause_android_sgm<-filter_clause_ga4(list(my_filter1_android_sgm, my_filter2_android_sgm, my_filter3_android_sgm),operator = "OR")

sg1<-segment_ga4("DomesticTY_android",segment_id = "gaid::CDfhug6eSKabl-vYKH6vGQ")
sg2<-segment_ga4("DomesticCY_android",segment_id = "gaid::rLZezMWaRqmsFuEnHrHkOw")
sg3<-segment_ga4("International_android", segment_id="gaid::ThzWix_8Ti-EtymZ4X5Cgw")

##5.1. Segmented Flight Android (DomesticTY)
data1_android_sgm1<-google_analytics(ga_id_android,
                                     date_range = c(start,end),
                                     metrics = c("ga:uniqueEvents", "ga:totalEvents"),
                                     dimension=c("eventCategory","eventAction","eventLabel"),
                                     anti_sample = T,
                                     segments=sg1,
                                     dim_filters = my_filter_clause_android_sgm)
unique(data1_android_sgm1$eventCategory)
data1_android_sgm1_gerekliler<-subset(data1_android_sgm1, (data1_android_sgm1$eventAction%in%c("flightCheckout-1","flightCheckout-2","flightPurchase"))|
                                        (data1_android_sgm1$eventCategory=="Payment Success - Flight"&data1_android_sgm1$eventLabel%in%c("PurchaseSuccess","Loggedin User Detected","Registered User Detected"))|
                                        (data1_android_sgm1$eventCategory=="MasterPass"&data1_android_sgm1$eventLabel=="Successful payment MasterPass"))
data1_android_sgm1_gerekliler$Ulasim_Yolu<-"Ucak_Domestic_Tek_Yon"

##5.2. Segmented Flight Android (DomesticCY)
data1_android_sgm2<-google_analytics(ga_id_android,
                                     date_range = c(start,end),
                                     metrics = c("ga:uniqueEvents", "ga:totalEvents"),
                                     dimension=c("eventCategory","eventAction","eventLabel"),
                                     anti_sample = T,
                                     segments=sg2,
                                     dim_filters = my_filter_clause_android_sgm)

data1_android_sgm2_gerekliler<-subset(data1_android_sgm2, (data1_android_sgm2$eventAction%in%c("flightCheckout-1","flightCheckout-2","flightPurchase"))|
                                        (data1_android_sgm2$eventCategory=="Payment Success - Flight"&data1_android_sgm2$eventLabel%in%c("PurchaseSuccess","Loggedin User Detected","Registered User Detected"))|
                                        (data1_android_sgm2$eventCategory=="MasterPass"&data1_android_sgm2$eventLabel=="Successful payment MasterPass"))
data1_android_sgm2_gerekliler$Ulasim_Yolu<-"Ucak_Domestic_Gidis_Donus"

##5.3. Segmented Flight Android (International)
data1_android_sgm3<-google_analytics(ga_id_android,
                                     date_range = c(start,end),
                                     metrics = c("ga:uniqueEvents", "ga:totalEvents"),
                                     dimension=c("eventCategory","eventAction","eventLabel"),
                                     anti_sample = T,
                                     segments=sg3,
                                     dim_filters = my_filter_clause_android_sgm)

data1_android_sgm3_gerekliler<-subset(data1_android_sgm3, (data1_android_sgm3$eventAction%in%c("flightCheckout-1","flightCheckout-2","flightPurchase"))|
                                        (data1_android_sgm3$eventCategory=="Payment Success - Flight"&data1_android_sgm3$eventLabel%in%c("PurchaseSuccess","Loggedin User Detected","Registered User Detected"))|
                                        (data1_android_sgm3$eventCategory=="MasterPass"&data1_android_sgm3$eventLabel=="Successful payment MasterPass"))
data1_android_sgm3_gerekliler$Ulasim_Yolu<-"Ucak_International"
##5.4. Segmented Flight Android Hepsi
data_android_ucak_odemeli<-rbind.fill(data1_android_sgm1_gerekliler, data1_android_sgm2_gerekliler, data1_android_sgm3_gerekliler)

data_android_ucak_odemeli$Hafta<-as.Date(week, "%d/%m/%Y")
data_android_ucak_odemeli$Kanal<-"3. Android-New"

data_android_ucak_odemeli$eventLabel[data_android_ucak_odemeli$eventLabel == "(not set)"] <- data_android_ucak_odemeli$eventAction[data_android_ucak_odemeli$eventLabel == "(not set)"]
data_android_ucak_odemeli_unique<-cast(data_android_ucak_odemeli, Ulasim_Yolu+Hafta+Kanal~eventLabel, value = "uniqueEvents")
data_android_ucak_odemeli_total<-cast(data_android_ucak_odemeli, Ulasim_Yolu+Hafta+Kanal~eventLabel, value = "totalEvents")

if(!("flightPurchase" %in% unique(data_android_ucak_odemeli$eventLabel))){
  data_android_ucak_odemeli_unique[,"flightPurchase"] <- 0
  data_android_ucak_odemeli_total[,"flightPurchase"] <- 0
}
if(!("Successful payment MasterPass" %in% unique(data_android_ucak_odemeli$eventLabel))){
  data_android_ucak_odemeli_unique[,"Successful payment MasterPass"] <- 0
  data_android_ucak_odemeli_total[,"Successful payment MasterPass"] <- 0
}
if(!("PurchaseSuccess" %in% unique(data_android_ucak_odemeli$eventLabel))){
  data_android_ucak_odemeli_unique[,"PurchaseSuccess"] <- 0
  data_android_ucak_odemeli_total[,"PurchaseSuccess"] <- 0
}

data_android_ucak_odemeli_total_gerekliler<-data_android_ucak_odemeli_total[c("Ulasim_Yolu", "Hafta","Kanal", "flightPurchase", "PurchaseSuccess","Successful payment MasterPass")]
data_android_ucak_odemeli_unique_gerekliler<-data_android_ucak_odemeli_unique[c("Ulasim_Yolu","Hafta","Kanal", "flightCheckout-1","flightCheckout-2","flightPurchase", "PurchaseSuccess", "Loggedin User Detected", "Registered User Detected", "Successful payment MasterPass")]

data_android_ucak_odemeli_final<-merge(data_android_ucak_odemeli_unique_gerekliler, data_android_ucak_odemeli_total_gerekliler, 
                                       by=c("Ulasim_Yolu","Hafta","Kanal"), all.x = T)


names(data_android_ucak_odemeli_final)<-c("Ulasim_Yolu","Hafta", "Kanal", "Odeme_Sayfasi_Goruntuleme","Odeme_Islemi_Istegi", "Odeme","Sigorta", "Uye_Odeme","Uye_Anonim_Odeme", "MasterPass_Odeme", "Total_Odeme", "Total_Sigorta","Total_MasterPass_Odeme")
data_android_ucak_odemeli_final$Anonim_Odeme<-data_android_ucak_odemeli_final$Odeme-(ifelse(is.na(data_android_ucak_odemeli_final$Uye_Odeme),0,data_android_ucak_odemeli_final$Uye_Odeme)+
                                                                                       ifelse(is.na(data_android_ucak_odemeli_final$Uye_Anonim_Odeme),0,data_android_ucak_odemeli_final$Uye_Anonim_Odeme))
data_android_ucak_odemeli_final<-data_android_ucak_odemeli_final[c("Ulasim_Yolu","Hafta", "Kanal", "Odeme_Sayfasi_Goruntuleme", "Odeme_Islemi_Istegi","Odeme","Sigorta","Uye_Odeme","Uye_Anonim_Odeme", "Anonim_Odeme", "MasterPass_Odeme","Total_Odeme","Total_Sigorta","Total_MasterPass_Odeme")]

#6. Segmented Flight Ios
my_filter1_ios_sgm<-dim_filter(dimension = "eventCategory",operator = "REGEXP",expressions = "funnel")
my_filter2_ios_sgm<-dim_filter(dimension = "eventCategory",operator = "REGEXP",expressions =  "Payment Success")
my_filter3_ios_sgm<-dim_filter(dimension = "eventCategory",operator = "REGEXP",expressions =  "MasterPass")
#my_filter_clause_ios_sgm<-filter_clause_ga4(list(my_filter1_ios_sgm, my_filter2_ios_sgm, my_filter3_ios_sgm),operator = "OR")
my_filter_clause_ios_sgm_payment<-filter_clause_ga4(list(my_filter1_ios_sgm))
my_filter_clause_ios_sgm_paymentSuccess_masterpass<-filter_clause_ga4(list(my_filter2_ios_sgm, my_filter3_ios_sgm),operator = "OR")


sg1<-segment_ga4("DomesticTY_ios",segment_id = "gaid::Zy7Y3j7FRd-VD9NdCSe9vA")
sg2<-segment_ga4("DomesticCY_ios",segment_id = "gaid::Tuc6WiHATFqqEfjXhNVENw")
sg3<-segment_ga4("International_ios", segment_id="gaid::iwcvzMkvRnyRoRBVVR4mvg")

##6.1. Segmented Flight Ios (DomesticTY)
data1_ios_sgm1_payment<-google_analytics(ga_id_ios,
                                         date_range = c(start,end),
                                         metrics = c("ga:uniqueEvents", "ga:totalEvents"),
                                         dimension=c("eventCategory","eventAction"),
                                         anti_sample = T,
                                         segments=sg1,
                                         dim_filters = my_filter_clause_ios_sgm_payment) #uçak ödeme ve checkout

data1_ios_sgm1_paymentSuccess_masterpass<-google_analytics(ga_id_ios,
                                                           date_range = c(start,end),
                                                           metrics = c("ga:uniqueEvents", "ga:totalEvents"),
                                                           dimension=c("eventCategory","eventAction","eventLabel"),
                                                           anti_sample = T,
                                                           segments=sg1,
                                                           dim_filters = my_filter_clause_ios_sgm_paymentSuccess_masterpass) #uçak paymentsuccess ve masterpass

data1_ios_sgm1<-rbind.fill(data1_ios_sgm1_paymentSuccess_masterpass, data1_ios_sgm1_payment)
data1_ios_sgm1_gerekliler<-subset(data1_ios_sgm1, (data1_ios_sgm1$eventAction%in%c("flightCheckout-1","flightCheckout-2","flightPurchase"))|
                                    (data1_ios_sgm1$eventCategory=="Payment Success - Flight"&data1_ios_sgm1$eventLabel%in%c("PurchaseSuccess","Loggedin User Detected","Registered User Detected"))|
                                    (data1_ios_sgm1$eventCategory=="MasterPass"&data1_ios_sgm1$eventLabel=="Successfully payment MasterPass"))
data1_ios_sgm1_gerekliler$Ulasim_Yolu<-"Ucak_Domestic_Tek_Yon"

##6.2. Segmented Flight Ios (DomesticCY)
data1_ios_sgm2_payment<-google_analytics(ga_id_ios,
                                         date_range = c(start,end),
                                         metrics = c("ga:uniqueEvents", "ga:totalEvents"),
                                         dimension=c("eventCategory","eventAction"),
                                         anti_sample = T,
                                         segments=sg2,
                                         dim_filters = my_filter_clause_ios_sgm_payment) #uçak ödeme ve checkout

data1_ios_sgm2_paymentSuccess_masterpass<-google_analytics(ga_id_ios,
                                                           date_range = c(start,end),
                                                           metrics = c("ga:uniqueEvents", "ga:totalEvents"),
                                                           dimension=c("eventCategory","eventAction","eventLabel"),
                                                           anti_sample = T,
                                                           segments=sg2,
                                                           dim_filters = my_filter_clause_ios_sgm_paymentSuccess_masterpass) #uçak paymentsuccess ve masterpass

data1_ios_sgm2<-rbind.fill(data1_ios_sgm2_paymentSuccess_masterpass, data1_ios_sgm2_payment)

data1_ios_sgm2_gerekliler<-subset(data1_ios_sgm2, (data1_ios_sgm2$eventAction%in%c("flightCheckout-1","flightCheckout-2","flightPurchase"))|
                                    (data1_ios_sgm2$eventCategory=="Payment Success - Flight"&data1_ios_sgm2$eventLabel%in%c("PurchaseSuccess","Loggedin User Detected","Registered User Detected"))|
                                    (data1_ios_sgm2$eventCategory=="MasterPass"&data1_ios_sgm2$eventLabel=="Successfully payment MasterPass"))
data1_ios_sgm2_gerekliler$Ulasim_Yolu<-"Ucak_Domestic_Gidis_Donus"
##6.3. Segmented Flight Ios (International)
data1_ios_sgm3_payment<-google_analytics(ga_id_ios,
                                         date_range = c(start,end),
                                         metrics = c("ga:uniqueEvents", "ga:totalEvents"),
                                         dimension=c("eventCategory","eventAction"),
                                         anti_sample = T,
                                         segments=sg3,
                                         dim_filters = my_filter_clause_ios_sgm_payment) #uçak ödeme ve checkout

data1_ios_sgm3_paymentSuccess_masterpass<-google_analytics(ga_id_ios,
                                                           date_range = c(start,end),
                                                           metrics = c("ga:uniqueEvents", "ga:totalEvents"),
                                                           dimension=c("eventCategory","eventAction","eventLabel"),
                                                           anti_sample = T,
                                                           segments=sg3,
                                                           dim_filters = my_filter_clause_ios_sgm_paymentSuccess_masterpass) #uçak paymentsuccess ve masterpass


data1_ios_sgm3<-rbind.fill(data1_ios_sgm3_paymentSuccess_masterpass, data1_ios_sgm3_payment)

data1_ios_sgm3_gerekliler<-subset(data1_ios_sgm3, (data1_ios_sgm3$eventAction%in%c("flightCheckout-1","flightCheckout-2","flightPurchase"))|
                                    (data1_ios_sgm3$eventCategory=="Payment Success - Flight"&data1_ios_sgm3$eventLabel%in%c("PurchaseSuccess","Loggedin User Detected","Registered User Detected"))|
                                    (data1_ios_sgm3$eventCategory=="MasterPass"&data1_ios_sgm3$eventLabel=="Successfully payment MasterPass"))
data1_ios_sgm3_gerekliler$Ulasim_Yolu<-"Ucak_International"
##6.4. Segmented Flight Ios Hepsi
data_ios_ucak_odemeli<-rbind.fill(data1_ios_sgm1_gerekliler, data1_ios_sgm2_gerekliler, data1_ios_sgm3_gerekliler)

data_ios_ucak_odemeli$Hafta<-as.Date(week, "%d/%m/%Y")
data_ios_ucak_odemeli$Kanal<-"4. Ios-New"

#data_ios_ucak_odemeli$eventLabel[data_ios_ucak_odemeli$eventLabel == "(not set)"] <- data_ios_ucak_odemeli$eventAction[data_ios_ucak_odemeli$eventLabel == "(not set)"]
#üstteki kod na'lar için çalışmıyor

data_ios_ucak_odemeli$eventLabel[is.na(data_ios_ucak_odemeli$eventLabel)] <- data_ios_ucak_odemeli$eventAction[is.na(data_ios_ucak_odemeli$eventLabel)]
data_ios_ucak_odemeli_unique<-cast(data_ios_ucak_odemeli, Ulasim_Yolu+Hafta+Kanal~eventLabel, value = "uniqueEvents")
data_ios_ucak_odemeli_total<-cast(data_ios_ucak_odemeli, Ulasim_Yolu+Hafta+Kanal~eventLabel, value = "totalEvents")

if(!("flightPurchase" %in% unique(data_ios_ucak_odemeli$eventLabel))){
  data_ios_ucak_odemeli_unique[,"flightPurchase"] <- 0
  data_ios_ucak_odemeli_total[,"flightPurchase"] <- 0
}
if(!("Successfully payment MasterPass" %in% unique(data_ios_ucak_odemeli$eventLabel))){
  data_ios_ucak_odemeli_unique[,"Successfully payment MasterPass"] <- 0
  data_ios_ucak_odemeli_total[,"Successfully payment MasterPass"] <- 0
}
if(!("PurchaseSuccess" %in% unique(data_ios_ucak_odemeli$eventLabel))){
  data_ios_ucak_odemeli_unique[,"PurchaseSuccess"] <- 0
  data_ios_ucak_odemeli_total[,"PurchaseSuccess"] <- 0
}

data_ios_ucak_odemeli_total_gerekliler<-data_ios_ucak_odemeli_total[c("Ulasim_Yolu", "Hafta","Kanal", "flightPurchase", "PurchaseSuccess","Successfully payment MasterPass")]
data_ios_ucak_odemeli_unique_gerekliler<-data_ios_ucak_odemeli_unique[c("Ulasim_Yolu","Hafta","Kanal", "flightCheckout-1","flightCheckout-2", "flightPurchase", "PurchaseSuccess", "Loggedin User Detected", "Registered User Detected", "Successfully payment MasterPass")]

data_ios_ucak_odemeli_final<-merge(data_ios_ucak_odemeli_unique_gerekliler, data_ios_ucak_odemeli_total_gerekliler, 
                                   by=c("Ulasim_Yolu","Hafta","Kanal"), all.x = T)


names(data_ios_ucak_odemeli_final)<-c("Ulasim_Yolu","Hafta", "Kanal", "Odeme_Sayfasi_Goruntuleme","Odeme_Islemi_Istegi", "Odeme","Sigorta", "Uye_Odeme","Uye_Anonim_Odeme", "MasterPass_Odeme", "Total_Odeme", "Total_Sigorta","Total_MasterPass_Odeme")
data_ios_ucak_odemeli_final$Anonim_Odeme<-data_ios_ucak_odemeli_final$Odeme-(ifelse(is.na(data_ios_ucak_odemeli_final$Uye_Odeme),0,data_ios_ucak_odemeli_final$Uye_Odeme)+
                                                                               ifelse(is.na(data_ios_ucak_odemeli_final$Uye_Anonim_Odeme),0,data_ios_ucak_odemeli_final$Uye_Anonim_Odeme))
data_ios_ucak_odemeli_final<-data_ios_ucak_odemeli_final[c("Ulasim_Yolu","Hafta", "Kanal", "Odeme_Sayfasi_Goruntuleme", "Odeme_Islemi_Istegi", "Odeme","Sigorta","Uye_Odeme","Uye_Anonim_Odeme", "Anonim_Odeme", "MasterPass_Odeme","Total_Odeme","Total_Sigorta","Total_MasterPass_Odeme")]

#####Otobus Final
#data_android_otobus_final+data_ios_otobus_final+data_web_otobus_final=data_otobus_final
data_otobus_final<-rbind.fill(data_android_otobus_final, data_ios_otobus_final, data_web_otobus_final)
data_otobus_final$Kanal<-gsub("mobile", "1. Mobile_Web", data_otobus_final$Kanal)
data_otobus_final$Kanal<-gsub("desktop", "2. Desktop", data_otobus_final$Kanal)
data_otobus_final$Kanal<-gsub("Android-New", "3. Android-New", data_otobus_final$Kanal)
data_otobus_final$Kanal<-gsub("Ios-New", "4. Ios-New", data_otobus_final$Kanal)

data_ucak_odemeli<-rbind.fill(data_android_ucak_odemeli_final, data_ios_ucak_odemeli_final, data_web_ucak_odemeli_final)
data_ucak_final<-merge(data_ucak_odemesiz, data_ucak_odemeli, 
                       by=c("Ulasim_Yolu","Hafta","Kanal"), all.x=T)
####Session Sayılarını Çekme

#1.Web
data_session_web<-google_analytics(ga_id,
                                   date_range = c(start,end),
                                   metrics = c("ga:sessions"),
                                   dimension=c("deviceCategory"), anti_sample = T)

data_session_web<-subset(data_session_web, data_session_web$deviceCategory!="tablet")
data_session_web$Hafta<-as.Date(week, "%d/%m/%Y")
data_session_web<-data_session_web[c("Hafta", "deviceCategory", "sessions")]
names(data_session_web)<-c("Hafta","Kanal", "Session_Sayisi")
data_session_web$Kanal<-gsub("mobile", "1. Mobile_Web", data_session_web$Kanal)
data_session_web$Kanal<-gsub("desktop", "2. Desktop", data_session_web$Kanal)
#2. Android
data_session_android<-google_analytics(ga_id_android,
                                       date_range = c(start,end),
                                       metrics = c("ga:sessions"),
                                       anti_sample = T)

data_session_android$Hafta<-as.Date(week, "%d/%m/%Y")
data_session_android$Kanal<-"3. Android-New"
data_session_android<-data_session_android[c("Hafta", "Kanal", "sessions")]
names(data_session_android)<-c("Hafta","Kanal", "Session_Sayisi")

#3. iOS
data_session_ios<-google_analytics(ga_id_ios,
                                   date_range = c(start,end),
                                   metrics = c("ga:sessions"),
                                   anti_sample = T)

data_session_ios$Hafta<-as.Date(week, "%d/%m/%Y")
data_session_ios$Kanal<-"4. Ios-New"
data_session_ios<-data_session_ios[c("Hafta", "Kanal", "sessions")]
names(data_session_ios)<-c("Hafta","Kanal", "Session_Sayisi")

#Session Final
data_session<-rbind(data_session_web, data_session_android, data_session_ios)
data_session<-data_session%>%arrange(Kanal)

####Final
#data_otobus_final+data_ucak_final=data_final
data_final<-rbind.fill(data_otobus_final, data_ucak_final)
data_final<-merge(data_final, data_session, by=c("Hafta","Kanal"), all.x=T) #Session sayılarının cihaza göre son tabloya join edilmesi
data_final$Ulasim_Yolu<-gsub( "Otobus","1. Otobus", data_final$Ulasim_Yolu)
data_final$Ulasim_Yolu<-gsub( "Ucak_Domestic_Tek_Yon","2. Ucak_Domestic_Tek_Yon", data_final$Ulasim_Yolu)
data_final$Ulasim_Yolu<-gsub( "Ucak_Domestic_Gidis_Donus","3. Ucak_Domestic_Gidis_Donus", data_final$Ulasim_Yolu)
data_final$Ulasim_Yolu<-gsub( "Ucak_International","4. Ucak_International", data_final$Ulasim_Yolu)
data_final<-data_final[c("Ulasim_Yolu","Hafta","Kanal","Sefer_Listeleme","Sefer_Secimi","Koltuk_Secimi",
                         "Gidis_Sefer_Secimi", "Donus_Sefer_Secimi","Odeme_Sayfasi_Goruntuleme","Odeme_Islemi_Istegi",
                         "Odeme","Sigorta","Uye_Odeme","Uye_Anonim_Odeme","Anonim_Odeme","MasterPass_Odeme","Total_Odeme","Total_Sigorta","Total_MasterPass_Odeme","Session_Sayisi")]
data_final[is.na(data_final$Odeme_Sayfasi_Goruntuleme),"Odeme_Sayfasi_Goruntuleme"]=0
data_final[is.na(data_final$Odeme_Sayfasi_Goruntuleme),"Odeme_Islemi_Istegi"]=0
data_final[is.na(data_final$Odeme),"Odeme"]=0
data_final[is.na(data_final$Sigorta),"Sigorta"]=0
data_final[is.na(data_final$Uye_Odeme),"Uye_Odeme"]=0
data_final[is.na(data_final$Uye_Anonim_Odeme),"Uye_Anonim_Odeme"]=0
data_final[is.na(data_final$Anonim_Odeme),"Anonim_Odeme"]=0
#data_final$Uye_Odeme[data_final$Kanal%in%c("3. Android-New","4. Ios-New")]="-"
#data_final$Uye_Anonim_Odeme[data_final$Kanal%in%c("3. Android-New","4. Ios-New")]="-"
#data_final$Anonim_Odeme[data_final$Kanal%in%c("3. Android-New","4. Ios-New")]="-"
data_final$MasterPass_Odeme[data_final$Ulasim_Yolu%in%c("2. Ucak_Domestic_Tek_Yon","3. Ucak_Domestic_Gidis_Donus", "4. Ucak_International")]="-"
data_final[is.na(data_final$MasterPass_Odeme),"MasterPass_Odeme"]=0
data_final$Total_MasterPass_Odeme[data_final$Ulasim_Yolu%in%c("2. Ucak_Domestic_Tek_Yon","3. Ucak_Domestic_Gidis_Donus", "4. Ucak_International")]="-"
data_final[is.na(data_final$Total_MasterPass_Odeme),"MasterPass_Odeme"]=0
data_final[is.na(data_final$Total_Odeme),"Total_Odeme"]=0
data_final[is.na(data_final$Total_Sigorta),"Total_Sigorta"]=0
data_final[is.na(data_final)]="-"
data_final<-data_final%>%arrange(Ulasim_Yolu,Kanal)

### data_final[is.na(data_final)]="-"  -> NA değerlerin bulunduğu columnları character yapıyor.

class(data_final$Sefer_Secimi)
sheet_write(data_final,
            ss="https://docs.google.com/spreadsheets/d/1hlkt06QJRKKyjD5q89Fu1UC_hXdBefIMj-27k2z2vno/edit#gid=68852758",
            sheet="15-Şubat-2021-checkout")
# Aylık: https://docs.google.com/spreadsheets/d/1zNyDFlo9wJdtjGV9SzRp8z7TOctUyxV4gsi8tqAICnI/edit#gid=248854895 
# Haftalık: https://docs.google.com/spreadsheets/d/1hlkt06QJRKKyjD5q89Fu1UC_hXdBefIMj-27k2z2vno/edit#gid=68852758

