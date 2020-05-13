## wczytanie danych
library(readr)
library(data.table)

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

## lista z bazami danych
months <- list(january, february, march, april, may, june, july, august, september, october, november, december)

## baza stacji
stations <- data.table(january[, c("end station id", "end station name", "end station latitude", "end station longitude")])
stations <- rbind(stations, january[, c("start station id", "start station name", 
                                        "start station latitude", "start station longitude")], 
                                        use.names=FALSE)
for(i in 2:12) {
  stations <- rbind(stations, months[[i]][,c("end station id", "end station name", 
                                                         "end station latitude", "end station longitude")], 
                                                          use.names = FALSE)
  stations <- rbind(stations, months[[i]][,c("start station id", "start station name", 
                                                         "start station latitude", "start station longitude")],
                                                          use.names = FALSE)
}
oldnames <- c("end station id", "end station name", 
              "end station latitude", "end station longitude")
newnames <- c("id", "name", 
              "latitude", "longitude")
setnames(stations,old = oldnames, new = newnames, skip_absent = TRUE)

stations <- unique(stations)

## ---- najbardziej oblegane stacje patrząc po end station ----
end_stations_indices <- data.table(january[,"end station id"])
for(i in 2:12) {
  end_stations_indices <- rbind(end_stations_indices, months[[i]]$`end station id`, use.names = FALSE)
}

end_stations_indices_grouped <- end_stations_indices[, .N ,by="end station id"]
end_stations_indices_drop_na <- end_stations_indices_grouped[!is.na(end_stations_indices_grouped$`end station id`),]
end_stations_indices_sorted_byN <- end_stations_indices_drop_na[order(N, decreasing = TRUE)] 


