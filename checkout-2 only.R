require(googlesheets)
require(googlesheets4)
require(reshape)
require(plyr)
require(dplyr)
require(googleAnalyticsR)
require(data.table)

start<-"2020-1-1"
end<-"2020-1-31"
month<-as.Date("1/1/2020", format="%d/%m/%Y")
ga_id<-63089351
ga_id_android<-73607140
ga_id_ios<-72226277

my_filter1<-dim_filter(dimension = "eventCategory",operator = "EXACT",expressions = "funnel")
my_filter2<-dim_filter(dimension = "eventAction",operator = "EXACT",expressions =  "checkout-2")
my_filter_clause<-filter_clause_ga4(list(my_filter1, my_filter2),operator = "AND")
data1<-google_analytics(ga_id,
                        date_range = c(start,end),
                        metrics = "ga:uniqueEvents",
                        dimension=c("eventCategory","eventAction","deviceCategory"), anti_sample = T,
                        dim_filters = my_filter_clause)
data2<-subset(data1,(data1$deviceCategory!="tablet"))
data2$eventCategory <- NULL
data2 <- data2[c("deviceCategory","eventAction","uniqueEvents")]

my_filter1_android<-dim_filter(dimension = "eventCategory",operator = "EXACT",expressions = "Funnel")
my_filter2_android<-dim_filter(dimension = "eventAction",operator = "EXACT",expressions =  "checkout-2")
my_filter_clause_android<-filter_clause_ga4(list(my_filter1_android, my_filter2_android),operator = "AND")
data1_android<-google_analytics(ga_id_android,
                               date_range = c(start,end),
                               metrics = "ga:uniqueEvents",
                               dimension=c("eventCategory","eventAction"), anti_sample = T,
                               dim_filters = my_filter_clause_android)
data1_android$eventCategory <- NULL
data1_android$deviceCategory <- "3. android"
data2_android <- data1_android[c("deviceCategory","eventAction","uniqueEvents")]

my_filter1_ios<-dim_filter(dimension = "eventCategory",operator = "EXACT",expressions = "Funnel")
my_filter2_ios<-dim_filter(dimension = "eventAction",operator = "EXACT",expressions =  "checkout-2")
my_filter_clause_ios<-filter_clause_ga4(list(my_filter1_ios, my_filter2_ios),operator = "AND")

data1_ios<-google_analytics(ga_id_ios,
                                date_range = c(start,end),
                                metrics = "ga:uniqueEvents",
                                dimension=c("eventCategory","eventAction"), anti_sample = T,
                                dim_filters = my_filter_clause_ios)
data1_ios$eventCategory <- NULL
data1_ios$deviceCategory <- "4. ios"
data2_ios <- data1_ios[c("deviceCategory","eventAction","uniqueEvents")]

data_otobus <- rbind(data2,data2_android,data2_ios)

data_otobus <- as.data.table(data_otobus)
data_otobus<- melt(data_otobus, id=c("deviceCategory","eventAction"))
data_otobus$variable <- NULL
data_otobus$type <- "1. bus"
colnames(data_otobus) <- c("deviceCategory","eventAction","uniqueEvents","type")

my_filter1_web_sgm<-dim_filter(dimension = "eventCategory",operator = "EXACT",expressions = "funnel")
my_filter2_web_sgm<-dim_filter(dimension = "eventAction",operator = "EXACT",expressions =  "flightCheckout-2")
my_filter_clause_web_sgm<-filter_clause_ga4(list(my_filter1_web_sgm, my_filter2_web_sgm),operator = "AND")

sg1<-segment_ga4("DomesticTY_web",segment_id = "gaid::EKno2FLAR7K9UsHWvt3pdA")
sg2<-segment_ga4("DomesticCY_web",segment_id = "gaid::pqoFI_1lTVW1_mEV62-0yw")
sg3<-segment_ga4("International_web", segment_id="gaid::8rXo3fWLQpWrLV06d6VAwA")

data1_web_sgm1<-google_analytics(ga_id,
                                 date_range = c(start,end),
                                 metrics = "ga:uniqueEvents",
                                 dimension=c("eventCategory","eventAction","deviceCategory"),
                                 anti_sample = T,
                                 segments=sg1,
                                 dim_filters = my_filter_clause_web_sgm)
data1_web_sgm1$eventCategory <- NULL
data1_web_sgm1$segment <- NULL
data1_web_sgm1$type <- "2. DomesticTY"

data1_web_sgm2<-google_analytics(ga_id,
                                 date_range = c(start,end),
                                 metrics = "ga:uniqueEvents",
                                 dimension=c("eventCategory","eventAction","deviceCategory"),
                                 anti_sample = T,
                                 segments=sg2,
                                 dim_filters = my_filter_clause_web_sgm)
data1_web_sgm2$eventCategory <- NULL
data1_web_sgm2$segment <- NULL
data1_web_sgm2$type <- "3. DomesticCY"

data1_web_sgm3<-google_analytics(ga_id,
                                 date_range = c(start,end),
                                 metrics = "ga:uniqueEvents",
                                 dimension=c("eventCategory","eventAction","deviceCategory"),
                                 anti_sample = T,
                                 segments=sg3,
                                 dim_filters = my_filter_clause_web_sgm)
data1_web_sgm3$eventCategory <- NULL
data1_web_sgm3$segment <- NULL
data1_web_sgm3$type <- "4. International"

data_web_flight <- rbind(data1_web_sgm1,data1_web_sgm2,data1_web_sgm3)
data_web_flight <- data_web_flight[c("deviceCategory","eventAction","uniqueEvents","type")]

my_filter1_android_sgm<-dim_filter(dimension = "eventCategory",operator = "EXACT",expressions = "Funnel")
my_filter2_android_sgm<-dim_filter(dimension = "eventAction",operator = "EXACT",expressions = "flightCheckout-2")
my_filter_clause_android_sgm<-filter_clause_ga4(list(my_filter1_android_sgm, my_filter2_android_sgm),operator = "AND")

sg1<-segment_ga4("DomesticTY_android",segment_id = "gaid::CDfhug6eSKabl-vYKH6vGQ")
sg2<-segment_ga4("DomesticCY_android",segment_id = "gaid::rLZezMWaRqmsFuEnHrHkOw")
sg3<-segment_ga4("International_android", segment_id="gaid::ThzWix_8Ti-EtymZ4X5Cgw")

data1_android_sgm1<-google_analytics(ga_id_android,
                                     date_range = c(start,end),
                                     metrics = "ga:uniqueEvents",
                                     dimension=c("eventCategory","eventAction"),
                                     anti_sample = T,
                                     segments=sg1,
                                     dim_filters = my_filter_clause_android_sgm)

data1_android_sgm1$eventCategory <- NULL
data1_android_sgm1$segment <- NULL
data1_android_sgm1$deviceCategory <- "3. android"
data1_android_sgm1 <- data1_android_sgm1[c("deviceCategory","eventAction","uniqueEvents")]
data1_android_sgm1$type <- "2. DomesticTY"
data1_android_sgm1 <- as.data.table(data1_android_sgm1)

data1_android_sgm2<-google_analytics(ga_id_android,
                                     date_range = c(start,end),
                                     metrics = "ga:uniqueEvents",
                                     dimension=c("eventCategory","eventAction"),
                                     anti_sample = T,
                                     segments=sg2,
                                     dim_filters = my_filter_clause_android_sgm)

data1_android_sgm2$eventCategory <- NULL
data1_android_sgm2$segment <- NULL
data1_android_sgm2$deviceCategory <- "3. android"
data1_android_sgm2 <- data1_android_sgm2[c("deviceCategory","eventAction","uniqueEvents")]
data1_android_sgm2$type <- "3. DomesticCY"
data1_android_sgm2 <- as.data.table(data1_android_sgm2)

data1_android_sgm3<-google_analytics(ga_id_android,
                                     date_range = c(start,end),
                                     metrics = "ga:uniqueEvents",
                                     dimension=c("eventCategory","eventAction"),
                                     anti_sample = T,
                                     segments=sg3,
                                     dim_filters = my_filter_clause_android_sgm)

data1_android_sgm3$eventCategory <- NULL
data1_android_sgm3$segment <- NULL
data1_android_sgm3$deviceCategory <- "3. android"
data1_android_sgm3 <- data1_android_sgm3[c("deviceCategory","eventAction","uniqueEvents")]
data1_android_sgm3$type <- "4. International"
data1_android_sgm3 <- as.data.table(data1_android_sgm3)

data_android_flight <- rbind(data1_android_sgm1,data1_android_sgm2,data1_android_sgm3)

my_filter1_ios_sgm<-dim_filter(dimension = "eventCategory",operator = "REGEXP",expressions = "Funnel")
my_filter2_ios_sgm<-dim_filter(dimension = "eventAction",operator = "REGEXP",expressions =  "flightCheckout-2")
my_filter_clause_ios<-filter_clause_ga4(list(my_filter1_ios_sgm,my_filter2_ios_sgm),operator="AND")

sg1<-segment_ga4("DomesticTY_ios",segment_id = "gaid::Zy7Y3j7FRd-VD9NdCSe9vA")
sg2<-segment_ga4("DomesticCY_ios",segment_id = "gaid::Tuc6WiHATFqqEfjXhNVENw")
sg3<-segment_ga4("International_ios", segment_id="gaid::iwcvzMkvRnyRoRBVVR4mvg")

data1_ios_sgm1<-google_analytics(ga_id_ios,
                                         date_range = c(start,end),
                                         metrics = "ga:uniqueEvents",
                                         dimension=c("eventCategory","eventAction"),
                                         anti_sample = T,
                                         segments=sg1,
                                         dim_filters = my_filter_clause_ios)

data1_ios_sgm1$eventCategory <- NULL
data1_ios_sgm1$segment <- NULL
data1_ios_sgm1$deviceCategory <- "4. ios"
data1_ios_sgm1 <- data1_ios_sgm1[c("deviceCategory","eventAction","uniqueEvents")]
data1_ios_sgm1$type <- "2. DomesticTY"
data1_ios_sgm1 <- as.data.table(data1_ios_sgm1)

data1_ios_sgm2<-google_analytics(ga_id_ios,
                                 date_range = c(start,end),
                                 metrics = "ga:uniqueEvents",
                                 dimension=c("eventCategory","eventAction"),
                                 anti_sample = T,
                                 segments=sg2,
                                 dim_filters = my_filter_clause_ios)

data1_ios_sgm2$eventCategory <- NULL
data1_ios_sgm2$segment <- NULL
data1_ios_sgm2$deviceCategory <- "4. ios"
data1_ios_sgm2 <- data1_ios_sgm2[c("deviceCategory","eventAction","uniqueEvents")]
data1_ios_sgm2$type <- "3. DomesticCY"
data1_ios_sgm2 <- as.data.table(data1_ios_sgm2)

data1_ios_sgm3<-google_analytics(ga_id_ios,
                                 date_range = c(start,end),
                                 metrics = "ga:uniqueEvents",
                                 dimension=c("eventCategory","eventAction"),
                                 anti_sample = T,
                                 segments=sg3,
                                 dim_filters = my_filter_clause_ios)

data1_ios_sgm3$eventCategory <- NULL
data1_ios_sgm3$segment <- NULL
data1_ios_sgm3$deviceCategory <- "4. ios"
data1_ios_sgm3 <- data1_ios_sgm3[c("deviceCategory","eventAction","uniqueEvents")]
data1_ios_sgm3$type <- "4. International"
data1_ios_sgm3 <- as.data.table(data1_ios_sgm3)

data_ios_flight <- rbind(data1_ios_sgm1,data1_ios_sgm2,data1_ios_sgm3)
data_final <- rbind(data_otobus,data_web_flight,data_android_flight,data_ios_flight)
data_final[data_final$deviceCategory == "mobile","deviceCategory"] = "1. mobile"
data_final[data_final$deviceCategory == "desktop","deviceCategory"] = "2. desktop"
data_final<-data_final%>%arrange(type,deviceCategory)
data_final$eventAction <- NULL
data_final$month <- as.Date(month, "%d/%m/%Y")

sheet_write(data_final,
            ss="https://docs.google.com/spreadsheets/d/1hjBMQWOkFyiBWvG7uqViaTeN97l2oTZrtyOMNpYQRSg/edit#gid=0",
            sheet="Ocak-2020")

