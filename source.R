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

#Komentarz