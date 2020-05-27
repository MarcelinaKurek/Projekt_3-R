## wczytanie danych
library(readr)
library(data.table)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(lubridate)


january <- data.table(read_csv("201901-citibike-tripdata.csv.zip"))
february <- data.table(read_csv("201902-citibike-tripdata.csv.zip"))
march <- data.table(read_csv("201903-citibike-tripdata.csv.zip"))
april <- data.table(read_csv("201904-citibike-tripdata.csv.zip"))
may <- data.table(read_csv("201905-citibike-tripdata.csv.zip"))
june <- data.table(read_csv("201906-citibike-tripdata.csv.zip"))
july <- data.table(read_csv("201907-citibike-tripdata.csv.zip"))
august <- data.table(read_csv("201908-citibike-tripdata.csv.zip"))
september <- data.table(read_csv("201909-citibike-tripdata.csv.zip"))
october <- data.table(read_csv("201910-citibike-tripdata.csv.zip"))
november <- data.table(read_csv("201911-citibike-tripdata.csv.zip"))
december <- data.table(read_csv("201912-citibike-tripdata.csv.zip"))

## mniejsze dane do pracy, żeby szybciej działało potem usuniemy

january <- january[sample(nrow(january), 10000), ]
february <- february[sample(nrow(february), 10000), ]
march <- march[sample(nrow(march), 10000), ]
april <- april[sample(nrow(april), 10000), ]
may <- may[sample(nrow(may), 10000), ]
june <- june[sample(nrow(june), 10000), ]
july <- july[sample(nrow(july), 10000), ]
august <- august[sample(nrow(august), 10000), ]
september <- september[sample(nrow(september), 10000), ]
october <- october[sample(nrow(october), 10000), ]
november <- november[sample(nrow(november), 10000), ]
december <- december[sample(nrow(december), 10000), ]

## Dodanie kolumny z nazwą miesiąca
january <- january[, month := "january"]
february <- february[, month := "february"]
march<- march[, month := "march"]
april<- april[, month := "april"]
may<- may[, month := "may"]
june <- june[, month := "june"]
july<- july[, month := "july"]
august <- august[, month := "august"]
september<- september[, month := "september"]
october <- october[, month := "october"]
november <- november[, month := "november"]
december <- december[, month := "december"]

## lista z bazami danych
months <- list(january, february, march, april, may, june, july, august, september, october, november, december)
## Dodanie kolumny z dniem tygodnia
for (i in 1:12) {
  months[[i]] <- months[[i]][, Weekday:= weekdays(starttime)]
}
## Wszystkie miesiące
whole_data <- data.table(january)
for(i in 2:12) {
  whole_data <- rbind(whole_data, months[[i]], use.names = FALSE)
}
wszystkie_wypozyczenia <- dim(whole_data)[1]
## ---- ile wszystkich rowerów ----
bikes <- whole_data[, "bikeid"]
bikes <- unique(bikes, by = "bikeid")
ile_rowerów <- dim(bikes)[1]
## ---- subskrybenci do zwykłych ----
by_usertype <- whole_data[,.N, b=usertype]
fwrite(by_usertype, "sub_vs_customers.csv")
## ---- baza stacji ----
raw_stations <- data.table(january[, c("end station id", "end station name", "end station latitude", "end station longitude")])
raw_stations <- rbind(raw_stations, january[, c("start station id", "start station name", 
                                        "start station latitude", "start station longitude")], 
                  use.names=FALSE)
for(i in 2:12) {
  raw_stations <- rbind(raw_stations, months[[i]][,c("end station id", "end station name", 
                                             "end station latitude", "end station longitude")], 
                    use.names = FALSE)
  raw_stations <- rbind(raw_stations, months[[i]][,c("start station id", "start station name", 
                                             "start station latitude", "start station longitude")],
                    use.names = FALSE)
}
oldnames <- c("end station id", "end station name", 
              "end station latitude", "end station longitude")
newnames <- c("id", "name", 
              "latitude", "longitude")
setnames(raw_stations,old = oldnames, new = newnames, skip_absent = TRUE)

stations <- copy(raw_stations)
stations <- unique(stations, by = "id")
stations <- stations[!is.na(stations$id) & id != "NULL",]
ile_stacji <- dim(stations)[1]

general_info <- data.frame(wypozyczenia = wszystkie_wypozyczenia, rowery = ile_rowerów, stacje = ile_stacji )
fwrite(general_info, "general.txt")
## ---- najbardziej oblegane stacje ogólnie ----

busy_stations <- copy(raw_stations)
busy_stations <- busy_stations[!is.na(id) & id != "NULL"]
busy_stations <- busy_stations[, .N, by="id"]
busy_stations <- stations[busy_stations, on="id"]
busy_stations <- busy_stations[order(N, decreasing = TRUE)]

fwrite(busy_stations, file="busy_stations.csv")

## ---- najbardziej oblegane stacje patrząc po end station od pon do piatku ----
# rano 7 - 10 ----
days <- c("poniedziałek", "wtorek", "środa", "czwartek", "piątek")
end_stations_indices <- data.table(june[Weekday %in% days & hour(starttime) >= 7 & hour(starttime) <= 10,"end station id"])
for(i in 7:9) {
  end_stations_indices <- rbind(end_stations_indices, months[[i]][Weekday %in% days & hour(starttime) >= 7 & hour(starttime) <= 10,"end station id"], use.names = FALSE)
}

end_stations_indices_grouped <- end_stations_indices[, .N ,by="end station id"]
end_stations_indices_drop_na <- end_stations_indices_grouped[!is.na(end_stations_indices_grouped$`end station id`) 
                                                             & end_stations_indices_grouped$`end station id` != "NULL",]
end_stations_indices_sorted_byN <- end_stations_indices_drop_na[order(N, decreasing = TRUE)] 
setkey(stations, id)
setkey(end_stations_indices_sorted_byN, `end station id`)
end_stations <- merge.data.table(end_stations_indices_sorted_byN,stations,
                                 by.x = key(end_stations_indices_sorted_byN) , by.y = key(stations),all.x = TRUE)

fwrite(end_stations, file="end_stations_morning.csv")

#wieczorem 16 - 19 ----
end_stations_indices <- data.table(june[Weekday %in% days & hour(starttime) >= 16 & hour(starttime) <= 19,"end station id"])
for(i in 7:9) {
  end_stations_indices <- rbind(end_stations_indices, months[[i]][Weekday %in% days & hour(starttime) >= 16 & hour(starttime) <= 19,"end station id"], use.names = FALSE)
}

end_stations_indices_grouped <- end_stations_indices[, .N ,by="end station id"]
end_stations_indices_drop_na <- end_stations_indices_grouped[!is.na(end_stations_indices_grouped$`end station id`) 
                                                             & end_stations_indices_grouped$`end station id` != "NULL",]
end_stations_indices_sorted_byN <- end_stations_indices_drop_na[order(N, decreasing = TRUE)] 
setkey(stations, id)
setkey(end_stations_indices_sorted_byN, `end station id`)
end_stations <- merge.data.table(end_stations_indices_sorted_byN,stations,
                                 by.x = key(end_stations_indices_sorted_byN) , by.y = key(stations),all.x = TRUE)

fwrite(end_stations, file="end_stations_evening.csv")


## ---- najczęściej wybierana trasa ----

routes <- data.table(january)
routes <- routes[, .N , by = list(`start station id`, `end station id`)]
routes <- routes[, month:= 1]
for(i in 2:12) {
  temporary <- months[[i]][, .N , by = list(`start station id`, `end station id`)]
  temporary$`start station id` <- as.double(temporary$`start station id`)
  temporary$`end station id` <- as.double(temporary$`end station id`) 
  temporary <- temporary[, month := i]
  routes <- rbind(routes, temporary, use.names=FALSE)
}
stations$id <- as.double(stations$id)

# szerokość i dł. geog. stacji początkowej
routes <- merge.data.table(routes, stations, by.x = "start station id", by.y = "id", all.x = TRUE, all.y = FALSE)
oldnames <- c("latitude", "longitude")
newnames <- c("start latitude", "start longitude")
setnames(routes, old = oldnames, new=newnames, skip_absent = TRUE)

# szerokość i dł. geog. stacji końcowej
routes <- merge.data.table(routes, stations, by.x = "end station id", by.y = "id", all.x = TRUE, all.y = FALSE)
newnames <- c("end latitude", "end longitude")
setnames(routes, old = oldnames, new=newnames, skip_absent = TRUE)

fwrite(routes[, c("N" ,"start latitude", "start longitude", "end latitude", "end longitude", "month")], "routes_to_plot.csv")

## ---- obserwacje ruchu na podstawie godzin( w których godzinach są krótkie trasy a w których długie) ----
hours_observations <- data.table(june[,c("starttime", "tripduration")])
for (i in 7:9) {
  hours_observations <- rbind(months[[i]][,c("starttime", "tripduration")])
}
hours_observations <- cbind(hours_observations, hour = hour(hours_observations$starttime), `duration in minutes` = hours_observations$tripduration / 60)
#pogrupowane po godzinach
hours_grouped <- hours_observations[, .N , by="hour"]
fwrite(hours_grouped, "hours_grouped.csv")

# czas trwania podróży przedziały  0 - 15 min, 15 min - 30 min, 30 min - 45 min, ponad 45 min (prawdopodonie przejażdżka)
duration_0_15min <- hours_observations[`duration in minutes` > 0 & `duration in minutes` <= 15, 'hour']
duration_15_30min <- hours_observations[`duration in minutes` > 15 & `duration in minutes` <= 30, 'hour']
duration_30_45min <- hours_observations[`duration in minutes` > 30 & `duration in minutes` <= 45, 'hour']
duration_over_45min <- hours_observations[`duration in minutes` > 45, 'hour']

# grupujemy każdą z tabel żeby zobaczyć w jakich godzinach najczęściej występuje dany czas podróży

# very short
duration_0_15min_grouped <- duration_0_15min[, .N, by = 'hour']
duration_0_15min_grouped <- duration_0_15min_grouped[order(hour, decreasing = FALSE)]
#short
duration_15_30min_grouped <- duration_15_30min[, .N, by = 'hour']
duration_15_30min_grouped <- duration_15_30min_grouped[order(hour, decreasing = FALSE)]
#average
duration_30_45min_grouped <- duration_30_45min[, .N, by = 'hour']
duration_30_45min_grouped <- duration_30_45min_grouped[order(hour, decreasing = FALSE)]
#long
duration_over_45min_grouped <- duration_over_45min[, .N, by = 'hour']
duration_over_45min_grouped <- duration_over_45min_grouped[order(hour, decreasing = FALSE)]

tripduration_over_hours <- data.table(hour = c(1:24), `0-15min` = duration_0_15min_grouped, `15-30min` = duration_15_30min_grouped,
                                      `30-45min` = duration_30_45min_grouped, `over 45min`=duration_over_45min_grouped)
fwrite(tripduration_over_hours, file = "tripduration_over_hours.csv")

## ---- Sprawdzenie czy ludzie jeżdżą w parach ----
group_trips <-data.table(whole_data[month %in% c("june", "july", "august"),])
group_trips <- group_trips[order(starttime),list(`starttime`,`start station id`, `end station id`)]
result_tab <- data.table(NULL)
len <- dim(group_trips)[1]
howmany <- 2
for (i in 1:(len-1)) {
  iter <- 1
  for (j in (i+1):len) {
    if (units.difftime(ymd_hms(group_trips$starttime[j]) - ymd_hms(group_trips$starttime[i])) == "secs") {
      minutes <- 0
    }
    else {
      minutes <- as.numeric(ymd_hms(group_trips$starttime[j]) - ymd_hms(group_trips$starttime[i]))
    }
    
    if (minutes < 4) {
      if (group_trips$`start station id`[i] == group_trips$`start station id`[j] & group_trips$`end station id`[i] == group_trips$`end station id`[j]){
        if (iter == 1) {
          row <- cbind(group_trips[i], howmany)
          result_tab <- rbind(result_tab, row)
        }
        else {
          result_tab[dim(result_tab)[1]] <- result_tab[dim(result_tab)[1]] + 1
        }
        iter <- iter + 1
      }
    }
    else if(minutes >= 4){
        break;
      }
    }
  }



## ---- Najdłużssze wypożyczenia ----
# funkcja do wyboru najdłuższych wypożyczeń z poszczególnych miesięcy
najdluzsze_podroze <- function(miesiac, ile = 6, min_czas_trwania = 0){
  x <- miesiac[tripduration >= min_czas_trwania,][order(tripduration, decreasing = TRUE)]
  ifelse(nrow(x) > ile, head(x, ile), x)
}

## struktura użytkowników
##wykres słupkowy
wszystko_razem <-  function(x,y) merge.data.table(x,y,all=TRUE) 
wszystko <- Reduce(wszystko_razem, months)
wypozyczenia <- wszystko[`birth year` > 1950, .(`birth year`, gender)]

dane_do_wykresu <- wypozyczenia[, .("rok urodzenia" =`birth year`, `liczba osób` = .N), by = list(`birth year`, gender)]
fwrite(dane_do_wykresu, "Wiek i płeć użytkownikóW.csv")


ggplot(data=dane_do_wykresu, aes(x=`rok urodzenia`, y=`liczba osób`, fill = as.factor(gender))) +
  geom_bar(stat="identity")+
  geom_text(aes(y=0, label=10), vjust=1.6, 
            color="white", size=3.5)+
  scale_fill_brewer(palette = "Dark2", name = "Płeć", labels = c("Brak danych", "Mężczyzna", "Kobieta") )+
  ggtitle("Chrarakterystyka użytkowników")

  
  
  
  ### --porówanie ruchu w poszczególnych miesiącach-- 
ruch <- lapply(months,  FUN = nrow)
which.max(ruch)



# kod generujący wykres ruchu w ciągu roku
ruch <- lapply(months,  FUN = nrow)
ruch <- unlist(ruch)
names(ruch) = c("STY", "LUT", "MAR", "KWI", "MAJ", "CZE", "LIP", "SIE", "WRZ", "PAZ", "LIS", "GRU")
palette(brewer.pal(11, "Spectral"))
barplot(ruch, las = 1, col = c(5, 6, 7 ,5, 4, 3, 2, 1, 11, 10, 9, 8), ylim = c(0, 50000), at = NULL)
ticks<-c(0, 10000,20000,30000,40000, 50000)
axis(2,at=ticks,labels=c("0", "10.000", "20.000", "30.000", "40.000", "50.000"), las = 1)
axis(1, at = seq(0.6, 14, length.out = 12), labels = names(ruch))
title("Liczba wypożyczonych rowerów w ciągu roku")



## --Średni czas wypożyczenia roweru w poszczególnych miesiącach--

czas_wypozyczenia <- wszystko[, .(month, tripduration, srednia = mean(tripduration)/60 ), by = month]
czas_wypozyczenia <- distinct(czas_wypozyczenia[, .("miesiąc" = month, "srednia długość podróży [min]" = srednia)])
fwrite(czas_wypozyczenia, file = "czas_wypozyczenia_rowerow.csv")

paleta <- palette(brewer.pal(12, "Paired"))

#wykres
ggplot(data=czas_wypozyczenia, aes(x= reorder(`miesiąc`, c(1:12)), y=`srednia długość podróży [min]`, fill = `miesiąc`)) +
  geom_bar(stat="identity")+
  scale_fill_manual(values = paleta)+
  scale_x_discrete(labels=c("STY", "LUT", "MAR", "KWI", "MAJ", "CZE", "LIP", "SIE", "WRZ", "PAZ", "LIS", "GRU"))+
  xlab("")+
  ggtitle("Średnia długość podróży w poszczególnych miesiącach")+
  theme(legend.position = "none")


## ----porówanie dnia powszedniego z weekendem dla miesięc czerwiec -wrzesien----


czerwiec_wrzesien <- c("june", "july", "august", "september")
Weekend_vs_zwykly <- wszystko[month %in% czerwiec_wrzesien, .("data" = as.Date(starttime), month)]
Weekend_vs_zwykly

# 1 czerwca - sobota
soboty <- seq.Date(as.Date("2019-06-01"), to = as.Date("2019-09-30"), by = 7 )
niedziele <- seq.Date(as.Date("2019-06-02"), to = as.Date("2019-09-30"), by = 7 )

n1 <- as.Date("2019-06-02")
Weekend_vs_zwykly <- Weekend_vs_zwykly[, .(data, month, "weekend" = (data %in% soboty | data %in% niedziele))]
Weekend_vs_zwykly <- Weekend_vs_zwykly[, .(data, month, weekend, "tydzien" = ceiling((data - n1)/7))][order(tydzien)]
Weekend_vs_zwykly <- Weekend_vs_zwykly[, .(data, month, "weekend" = as.character(weekend), tydzien, "liczba_tras"= .N) , by = c("tydzien", "weekend")]
Weekend_vs_zwykly[, c(1:2)] <- NULL
Weekend_vs_zwykly <- distinct(Weekend_vs_zwykly)
Weekend_vs_zwykly <- Weekend_vs_zwykly[, .(month, weekend, tydzien, liczba_tras, dni = .N), by = "liczba_tras"]
Weekend_vs_zwykly <- Weekend_vs_zwykly[, .(month, weekend, tydzien = n1+tydzien*7, "srednia" = liczba_tras/dni)]
Weekend_vs_zwykly <- distinct(Weekend_vs_zwykly)
fwrite(Weekend_vs_zwykly, file = "Weekend_vs_zwykly.csv")

#wykres
ggplot(Weekend_vs_zwykly, aes(x=tydzien ,y=srednia, group=weekend, colour=weekend)) +
  geom_point()+
  geom_line(size = 1)+
  ggtitle("Średnia ilość wypożyczeń w ciągu dnia w kolejnych tygodniach")+
  scale_colour_brewer(palette = "Set1", name = "", labels = c("Dni powszednie", "Weekendy") )+
  ylab("Ilość wypożyczeń [szt]")+
  xlab("")




