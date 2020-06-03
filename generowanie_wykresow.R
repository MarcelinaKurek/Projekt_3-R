## generowanie wykresów

## Wczytanie mapy ----
register_google(key = "AIzaSyD5TKwSQOohTjM8V1p2taClynMVSwp6Z1Y", write = TRUE)
nyc <- c(lon = -74.0059, lat = 40.74)
nyc_map <- get_map(nyc, zoom = 12, scale = 4)
map <- ggmap(nyc_map, extent = "device")

## Zagęszczenie ruchu ----
## Ogólnie ----
busyStations <- read_csv("przeliczone_dane/busy_stations.csv")

p1 <- map + geom_point(data= busyStations, aes(x=longitude, y=latitude, color=N), alpha=0.4, size = 1) + 
  scale_size() +
  scale_color_gradient(low = "pink", high = "purple") +
  ggtitle("Zagęszczenie ruchu na stacjach") + 
  theme(plot.title = element_text(face = "bold", size = 14) ,legend.title = element_text("Liczba wypożyczonych rowerów na stacji")) +
  coord_equal(ratio=1)
ggsave("plots/zageszczenie_ruchu_ogolnie.png", plot = p1, width = 10, height = 10, units = "cm")
## Rano ----
morning <- read_csv("przeliczone_dane/end_stations_morning.csv")
p2 <- map + geom_point(data = morning, aes(x=longitude, y=latitude, color=N),alpha=0.4, size =2) + 
  scale_size(range=c(.1, 10)) +
  scale_color_gradient(low="orange", high="blue") +
  ggtitle("Stacje docelowe rano (7 - 10, pon - pt)") +
  theme(legend.title = element_text("Liczba zwrotów rowerów na stacji")) + 
  coord_equal(ratio=1) 

ggsave("plots/zageszczenie_ruchu_rano.png", plot = p2, width = 10, height = 10, units = "cm")

## Wieczór ----

evening <- read_csv("przeliczone_dane/end_stations_evening.csv")
p3 <- map + geom_point(data = evening, aes(x=longitude, y=latitude, color=N),alpha=0.4, size = 1) + 
  scale_size(range=c(.1, 10)) +
  scale_color_gradient(low="orange", high="blue") +
  ggtitle("Stacje docelowe wieczorem (16 - 19, pon - pt)") +
  theme(legend.title = element_text("Liczba zwrotów rowerów na stacji"), plot.title = element_text(face = "bold", size = 12, margin = margin(30,0,5,0))) +
  coord_equal(ratio=1)

ggsave("plots/zageszczenie_ruchu_wieczor.png", plot = p3, width = 7, height = 7, units = "cm")

## Turysci ----

Tourists <- data.table(read.csv("przeliczone_dane/polygon_tourists_map.csv"))

setnames(Tourists, old = c("end.station.latitude", "end.station.longitude"), new = c("latitude", "longitude"))

map + stat_density2d(aes(x = longitude, y = latitude, fill = ..level..), alpha = 0.7,
                     size = 2, bins = 4, data = Tourists ,geom = "polygon") +
  scale_fill_viridis_c(direction = -1) 