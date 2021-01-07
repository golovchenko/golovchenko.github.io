### downloading coordinates ussing ggpmap
library(ggpmap)
coordinates <- mutate_geocode(bordercities, extended)

### rows without coordinate
coordinates_missing<- coordinates %>% filter(is.na(lat))
coordinates_missing$lat <- NULL
coordinates_missing$lon <- NULL

#counting missing rows
m1 <-nrow(coordinates_missing)# There are 63 rows with missing coordinates

# downloading missing coordinates
coordinates1 <- mutate_geocode(coordinates_missing, extended)

### reapiting the procedure above, since there are still a few missing rows
coordinates_missing<- coordinates1 %>% filter(is.na(lat))
coordinates_missing$lat <- NULL
coordinates_missing$lon <- NULL
#counting missing rows
m2 <-nrow(coordinates_missing)# There are 12 rows with missing coordinates

coordinates2 <- mutate_geocode(coordinates_missing, extended)
### reapiting the procedure above, since there are still a few missing rows
coordinates_missing<- coordinates2 %>% filter(is.na(lat))
coordinates_missing$lat <- NULL
coordinates_missing$lon <- NULL
#counting missing rows
m3 <-nrow(coordinates_missing)# There are 2 rows with missing coordinates


### reapiting the procedure above, since there are still a few missing rows
##deleted the non-standard rayons to avoid error when querring google
coordinates3 <- mutate_geocode(coordinates_missing, extended) ### FAILED DUE TO LIMIT
coordinates_missing<- coordinates3 %>% filter(is.na(lat))
coordinates_missing$lat <- NULL
coordinates_missing$lon <- NULL
#counting missing rows
m4 <-nrow(coordinates_missing)# There are 0 rows with missing coordinates

#### binding the three dataframes with coordiantes
coordinates_full <- rbind(coordinates, coordinates1, coordinates2, coordinates3)
coordinates_full <- coordinates_full %>% filter(!is.na(lat)) ### removing rows with missing coordinates

### I have manually inspected the coordinates for each city and corrected the  relatively 
### few cases where the coordinates where incorect (e.g. the API return a city with the 
### same name but in the wrong region)

#manually corecting coordinates
coordinates_full[267,8] <- 45.977356 #Correcting latitude for Таврийское Красноперекопський район
coordinates_full[267,7] <- 33.750178 #Correcting longitude for Таврийское Красноперекопський район
coordinates_full[146,8] <- 45.711880   #Correcting latitude for Джанкой Джанкойський район
coordinates_full[146,7] <- 34.390810 #Correcting longitude for Джанкой
coordinates_full[147,8] <-  45.696181 #Correcting latitude for Дмитровка Джанкойський район
coordinates_full[147,7] <-  34.356506#Correcting longitude for Дмитровка Джанкойський район
coordinates_full[182,8] <-  45.955583 #Correcting latitude for Красноперекопск Красноперекопський район
coordinates_full[182,7] <-  33.792516#Correcting longitude for Красноперекопск Красноперекопський район
coordinates_full[265,8] <- 46.087569   #Correcting latitude for Суворово Красноперекопський район
coordinates_full[265,7] <- 33.688761   #Correcting longitude for Суворово Красноперекопський район
coordinates_full[358,8] <- 46.37631   #Correcting latitude for Кирилловка Якимівський район // coordinates are from OpenStreetMap because there is a mistake in Google Maps
coordinates_full[358,7] <- 35.36722   #Correcting longitude for Кирилловка Якимівський район // coordinates are from OpenStreetMap because there is a mistake in Google Maps
coordinates_full <- coordinates_full[-85,]  #Deleting "Пионерское Генічеський район" because this city does not exist on the map and there are no users registered on this city on Vkontakte
