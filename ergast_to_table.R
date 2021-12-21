#Script to build F1 table and export to f1dats.rds

library(tidyverse)

#Download Formula 1 database
ergast_url <- "http://ergast.com/downloads/f1db_csv.zip"
download.file(ergast_url, "f1db_csv.zip")
unzipped <- unzip("f1db_csv.zip")
db <- map(unzipped, read_csv)
unlink(unzipped)

#Name Formula 1 tables
db_names_raw <- map(unzipped, str_split, "./")
db_names_select <- map(db_names_raw, function(x) x[[1]][[2]])
db_names_short <- map(db_names_select, str_split, ".csv")
db_names <- map(db_names_short, function(x) x[[1]][[1]]) %>% unlist()
names(db) <- db_names

#Create unified table of F1 data
dat <- db$results %>% select(-number) %>% 
  left_join(db$drivers, by = "driverId") %>% 
  left_join(db$constructors, by = "constructorId") %>% 
  left_join(db$status, by = "statusId") %>% 
  left_join(db$races, by = "raceId") %>% 
  left_join(db$seasons, by = "year") %>% 
  left_join(db$circuits, by = "circuitId") %>% 
  left_join(db$qualifying, by = c("raceId", "driverId", "constructorId")) %>% select(-number.y) %>%  
  left_join(db$pit_stops %>% 
              group_by(raceId, driverId) %>% 
              summarise(stops = max(stop, na.rm = TRUE),
                        stopTotalms = sum(milliseconds),
                        stopLaps = paste(lap, collapse = ","),
                        stopms = paste(milliseconds, collapse = ","),
                        stopAtTime = paste(time, collapse = ","),
                        stopDuration = paste(duration, collapse = ","))
            , by = c("raceId", "driverId")) %>% 
  left_join(db$constructor_results, by = c("raceId", "constructorId")) %>% 
  left_join(db$driver_standings, by = c("raceId", "driverId")) %>% 
  left_join(db$constructor_standings, by = c("raceId", "constructorId")) %>% 
  left_join(db$lap_times %>% pivot_wider(id_cols = c(raceId, driverId), names_from = lap, values_from = milliseconds), by = c("raceId", "driverId")) %>% 
  rename(positionRaceDriver = position.x,
         positionTextRaceDriver = positionText.x,
         positionOrderRaceDriver = positionOrder,
         pointsRaceDriver = points.x,
         timeFinish = time.x,
         msFinish = milliseconds,
         fastestLapRank = rank,
         driverNumber = number.x,
         nationalityDriver = nationality.x, 
         nationalityConstructor = nationality.y,
         urlDriver = url.x, 
         urlConstructor = url.y,
         constructor = name.x,
         statusRaceFinish = status.x,
         nameRace = name.y,
         timeRace = time.y,
         urlRace = url.x.x,
         urlSeason = url.y.y,
         nameCircuit = name,
         urlCircuit = url,
         positionQualifyDriver = position.y,
         pointsRaceConstructor = points.y, 
         statusRaceConstructor = status.y,
         pointsSeasonDriver = points.x.x,
         positionSeasonDriver  = position.x.x,
         positionTextSeasonDriver = positionText.y,
         winsSeasonDriver = wins.x,
         pointsSeasonConstructor = points.y.y,
         positionSeasonConstructor = position.y.y,
         positionTextSeasonConstructor = positionText,
         winsSeasonConstructor = wins.y
         ) %>% 
  group_by(year) %>% mutate(pointsYearTotal = sum(pointsRaceDriver),
                            raceFinal = max(raceId)) %>% ungroup()
  
#Calculate year final results for Constructors
dat_year_constructor <- dat %>% group_by(year) %>% mutate(raceIdMax = max(raceId)) %>% 
  filter(raceId == raceIdMax) %>% 
  distinct(constructorId, pointsSeasonConstructor, positionSeasonConstructor, winsSeasonConstructor) %>% 
  rename(pointsSeasonConstructorFinal = pointsSeasonConstructor, 
         positionSeasonConstructorFinal = positionSeasonConstructor, 
         winsSeasonConstructorFinal = winsSeasonConstructor)

#Calculate year final results for Drivers
dat_year_driver <- dat %>% group_by(year) %>% mutate(raceIdMax = max(raceId)) %>% 
  filter(raceId == raceIdMax) %>% 
  distinct(driverId, pointsSeasonDriver, positionSeasonDriver, winsSeasonDriver) %>% 
  rename(pointsSeasonDriverFinal = pointsSeasonDriver, 
         positionSeasonDriverFinal = positionSeasonDriver, 
         winsSeasonDriverFinal = winsSeasonDriver)

#Add year final results for Constructors and Drivers
dats <- dat %>% 
  left_join(dat_year_constructor, by = c("year", "constructorId")) %>% 
  left_join(dat_year_driver, by = c("year", "driverId"))

write_rds(dats, "f1dats.rds", compress = "gz")
