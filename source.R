## wczytanie danych
library(readr)
library(data.table)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(lubridate)
library(dplyr)


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
june <- june[sample(nrow(june), 100000), ]
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
                                       "start station latitude", "start station longitude")],use.names=FALSE)
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
end_stations_indices_sorted_byN <- end_stations_indices_drop_na[order(by = N, decreasing = TRUE)] 
setkey(stations, id)
setkey(end_stations_indices_sorted_byN, `end station id`)
end_stations_indices_sorted_byN$`end station id` <- as.character(end_stations_indices_sorted_byN$`end station id`)
end_stations <- merge(end_stations_indices_sorted_byN,stations, by.x = "end station id", by.y = "id",all.x = TRUE)

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
end_stations_indices_sorted_byN$`end station id` <- as.character(end_stations_indices_sorted_byN$`end station id`)
end_stations <- merge(end_stations_indices_sorted_byN,stations, by.x = "end station id", by.y = "id",all.x = TRUE)

fwrite(end_stations, file="end_stations_evening.csv")
## ---- gdzie jeżdzą turyści ----
tourists <- june[usertype=="Customer", c("start station latitude", "start station longitude", 
                                         "end station latitude", "end station longitude")]
for ( i in 7:9) {
  tourists <- rbind(tourists, months[[i]][usertype=="Customer",
                                          c("start station latitude", "start station longitude", 
                                            "end station latitude", "end station longitude")], use.names = FALSE)
}
tourists_places <- tourists[,.N, by = c("end station latitude", "end station longitude")]
tourists_places <- tourists_places[order(by = N, decreasing = TRUE)]
fwrite(tourists_places, "tourists_places.csv")
## ---- TRASY osoby 7 - 18 lat - uczniowie pon - pt ----
students_months <- april
students_months <- rbind(students_months, may, september, use.names = FALSE)
students_routes <- students_months[(2019 - `birth year`) <= 18 & (2019 - `birth year`) >= 7 &
                         Weekday%in% c("poniedziałek", "wtorek", "środa", "czwartek", "piątek"),
                       c("start station latitude", "start station longitude", "end station latitude", "end station longitude")]
students_routes_grouped <- students_routes[, .N, by = list(`start station longitude`, `start station latitude`, 
                                                           `end station longitude`, `end station latitude`)]
students_routes_grouped <- students_routes_grouped[(order(by = N, decreasing = TRUE))]
fwrite(students_routes_grouped, "student_routes.csv")
## ---- najczęściej wybierana trasa  ogólnie ----
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
routes <- routes[, .N, by = list("start latitude", "start longitude", "end latitude", "end longitude")]
routes <- routes[order(by = N, decreasing = TRUE),]
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
veryshort_count <- duration_0_15min_grouped[, sum(N)]
#short
duration_15_30min_grouped <- duration_15_30min[, .N, by = 'hour']
duration_15_30min_grouped <- duration_15_30min_grouped[order(hour, decreasing = FALSE)]
short_count <- duration_15_30min_grouped[, sum(N)]
#average
duration_30_45min_grouped <- duration_30_45min[, .N, by = 'hour']
duration_30_45min_grouped <- duration_30_45min_grouped[order(hour, decreasing = FALSE)]
average_count <- duration_30_45min_grouped[, sum(N)]
#long
duration_over_45min_grouped <- duration_over_45min[, .N, by = 'hour']
duration_over_45min_grouped <- duration_over_45min_grouped[order(hour, decreasing = FALSE)]
long_count <- duration_over_45min_grouped[, sum(N)]

typy_podrozy <- list("veryshort", "short", "average", "long")
typy_podrozy_zliczone <- data.table(`typ podrozy` = typy_podrozy, N = list(veryshort_count, short_count, average_count, long_count))
fwrite(typy_podrozy_zliczone, "ile_podrozy_okreslonej_dl.csv")
tripduration_over_hours <- data.table(hour = c(1:24), `0-15min` = duration_0_15min_grouped, `15-30min` = duration_15_30min_grouped,
                                      `30-45min` = duration_30_45min_grouped, `over 45min`=duration_over_45min_grouped)
fwrite(tripduration_over_hours, file = "tripduration_over_hours.csv")

## ---- Sprawdzenie czy ludzie jeżdżą w parach ----
group_trips <-june
group_trips <- rbind(group_trips, july, august, use.names = FALSE)
group_trips <- group_trips[,list(`starttime`,`start station id`, `end station id`)]
group_trips$`start station id` <- as.character(group_trips$`start station id`)
group_trips$`end station id` <- as.character(group_trips$`end station id`)
group_trips <- group_trips[, start_end := paste(`start station id`, `end station id`, sep=" ")]
result_tab <- data.table(NULL)

count_groups <- function(x) {
  result_tab <- data.table(NULL)
  len <- length(x)
  howmany <- 2
  for (i in 1:(len - 1)) {
    iter <- 1
    for (j in (i + 1):len) {
      if (units.difftime(ymd_hms(x[j]$starttime) - ymd_hms(x[i]$starttime)) == "secs") {
        minutes <- 0
      }
      else if (units.difftime(ymd_hms(x[j]$starttime) - ymd_hms(x[i]$starttime)) == "mins") {
        minutes <- as.numeric(ymd_hms(x[j]$starttime) - ymd_hms(x[i]$starttime))
      }
      else {
        break;
      }
      if (minutes < 4) {
          if (iter == 1) {
            row <- cbind(x[i], howmany)
            result_tab <- rbind(result_tab, row, use.names = FALSE)
            print("pierwszy raz")
            print(row)
            
          }
          else {
            result_tab$howmany[dim(result_tab)[1]] <- result_tab$howmany[dim(result_tab)[1]] + 1
            print(result_tab)
            print("drugi")
          }
          iter <- iter + 1
        }
      else if(minutes >= 4){
        break;
      }
    }
  }
  return(result_tab)
}
group_trips <- group_trips %>% group_by(start_end) #%>% group_map(data = .x, .f = count_groups())
by_ids <- split(group_trips, group_trips$start_end)
by_ids_copy <- copy(by_ids)
result <- lapply(by_ids_copy, count_groups)
fwrite(result_tab, "group_trips.csv")

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


  ### --porówanie ruchu w poszczególnych miesiącach-- 
ruch <- lapply(months,  FUN = nrow)
which.max(ruch)



## --Średni czas wypożyczenia roweru w poszczególnych miesiącach--

czas_wypozyczenia <- wszystko[, .(month, tripduration, srednia = mean(tripduration)/60 ), by = month]
czas_wypozyczenia <- distinct(czas_wypozyczenia[, .("miesiąc" = month, "srednia długość podróży [min]" = srednia)])
fwrite(czas_wypozyczenia, file = "czas_wypozyczenia_rowerow.csv")


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





