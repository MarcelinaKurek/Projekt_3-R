## wczytanie danych
library(readr)
library(data.table)
library(dplyr)
library(ggplot2)
library(RColorBrewer)

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

## ---- najbardziej oblegane stacje ogólnie ----

busy_stations <- copy(raw_stations)
busy_stations <- busy_stations[!is.na(id) & id != "NULL"]
busy_stations <- busy_stations[, .N, by="id"]
busy_stations <- stations[busy_stations, on="id"]
busy_stations <- busy_stations[order(N, decreasing = TRUE)]

fwrite(busy_stations, file="busy_stations.csv")

## ---- najbardziej oblegane stacje patrząc po end station ----

end_stations_indices <- data.table(january[,"end station id"])
for(i in 2:12) {
  end_stations_indices <- rbind(end_stations_indices, months[[i]]$`end station id`, use.names = FALSE)
}

end_stations_indices_grouped <- end_stations_indices[, .N ,by="end station id"]
end_stations_indices_drop_na <- end_stations_indices_grouped[!is.na(end_stations_indices_grouped$`end station id`) 
                                                             & end_stations_indices_grouped$`end station id` != "NULL",]
end_stations_indices_sorted_byN <- end_stations_indices_drop_na[order(N, decreasing = TRUE)] 
setkey(stations, id)
setkey(end_stations_indices_sorted_byN, `end station id`)
end_stations <- merge.data.table(end_stations_indices_sorted_byN,stations,
                                 by.x = key(end_stations_indices_sorted_byN) , by.y = key(stations),all.x = TRUE)

fwrite(end_stations, file="end_stations.csv")


## ---- najczęściej wybierana trasa ----

routes <- data.table(january)
routes <- routes[, .N , by = list(`start station id`, `end station id`)]
for(i in 2:12) {
  temporary <- months[[i]][, .N , by = list(`start station id`, `end station id`)]
  temporary$`start station id` <- as.double(temporary$`start station id`)
  temporary$`end station id` <- as.double(temporary$`end station id`)
  routes <- merge.data.table(routes, temporary, all = TRUE)
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

routes <- routes[order(N, decreasing = TRUE)]
routes_to_plot <- routes[1:100 ,c("start latitude", "start longitude", "id")]
routes_to_plot <- rbind(routes_to_plot, routes[1:100 ,c("end latitude", "end longitude", "id")], use.names = FALSE)
fwrite(routes_to_plot, "routes_to_plot.csv")

## ---- obserwacje ruchu na podstawie godzin( w których godzinach są krótkie trasy a w których długie) ----
hours_observations <- data.table(january[,c("starttime", "tripduration")])
for (i in 2:12) {
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
duration_0_15min_grouped <- duration_0_15min_grouped[order(N, decreasing = TRUE)]
#short
duration_15_30min_grouped <- duration_15_30min[, .N, by = 'hour']
duration_15_30min_grouped <- duration_15_30min_grouped[order(N, decreasing = TRUE)]
#average
duration_30_45min_grouped <- duration_30_45min[, .N, by = 'hour']
duration_30_45min_grouped <- duration_30_45min_grouped[order(N, decreasing = TRUE)]
#long
duration_over_45min_grouped <- duration_over_45min[, .N, by = 'hour']
duration_over_45min_grouped <- duration_over_45min_grouped[order(N, decreasing = TRUE)]

tripduration_over_hours <- data.table(`0-15min` = duration_0_15min_grouped[1:4], `15-30min` = duration_15_30min_grouped[1:4],
                                      `30-45min` = duration_30_45min_grouped[1:4], `over 45min`=duration_over_45min_grouped[1:4])
fwrite(tripduration_over_hours, file = "tripduration_over_hours.csv")

## ---- Najdłużssze wypożyczenia ----
# funkcja do wyboru najdłuższych wypożyczeń z poszczególnych miesięcy
najdluzsze_podroze <- function(miesiac, ile = 6, min_czas_trwania = 0){
  x <- miesiac[tripduration >= min_czas_trwania,][order(tripduration, decreasing = TRUE)]
  ifelse(nrow(x) > ile, head(x, ile), x)
}

## wybór 1% najdłuższych wypożyczeń spośród wszystkich miesięcy
wszystko_razem <-  function(x,y) merge.data.table(x,y,all=TRUE) 
najdluzsze_wypozyczenia <- Reduce(wszystko_razem, months)[order(tripduration, decreasing = TRUE)]
najdluzszych_1procent <- head(najdluzsze_wypozyczenia, 4050)
fwrite(najdluzszych_1procent, "najdluzszych_1procent.csv")


#kod generujący wykres przedziału wiekowego oraz płci "najlepszych" użytkowników
dane_do_wykresu <- najdluzszych_1procent[, .("rok urodzenia" =`birth year`, `liczba osób` = .N), by = list(`birth year`, gender)]
dane_do_wykresu <- dane_do_wykresu[`liczba osób` < 1000, ] #jeden rok zaciemnia wykres





### --porówanie ruchu w poszczególnych miesiącach-- 
ruch <- lapply(months,  FUN = nrow)
which.max(ruch)


# kod generujący wykres ruchu w ciągu roku
ruch <- unlist(ruch)
names(ruch) = c("JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL", "AUG", "SEP", "OCT", "NOV", "DEC")
palette(brewer.pal(11, "Spectral"))
barplot(ruch, las = 1, col = c(5, 6, 7 ,5, 4, 3, 2, 1, 11, 10, 9, 8), ylim = c(0, 50000), at = NULL)
ticks<-c(0, 10000,20000,30000,40000, 50000)
axis(2,at=ticks,labels=c("0", "10.000", "20.000", "30.000", "40.000", "50.000"), las = 1)
axis(1, at = seq(0.6, 14, length.out = 12), labels = names(ruch))
title("Liczba wypożyczonych rowerów w ciągu roku")



