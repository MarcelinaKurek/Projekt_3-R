## wczytanie danych
library(readr)
library(data.table)
library(dplyr)
library(ggplot2)

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
