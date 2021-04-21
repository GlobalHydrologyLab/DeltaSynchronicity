#Finding proportion of years where max month is max
DeltaNames <- unique(DeltasCleaner$Delta)

DeltaMaxMonth <- select(DeltaMaxMin,Delta,MaxMeanNDVImonth,MaxMeanNDSSImonth)
DeltaMaxMonth <- cbind(DeltaMaxMonth, propNDVI = 0, propNDSSI = 0)

for (name in DeltaNames) {
  #NDVI
  propMaxNDVI <- DeltasCleaner %>%
    select(Delta, surface, year, month, ndvi) %>%
    filter(Delta == name & surface == "Land" & !is.na(ndvi)) %>%
    group_by(year) %>%
    summarise(max = month[ndvi == max(ndvi)]) %>% 
    ungroup()

  maxMonthNDVI <- DeltaMaxMonth$MaxMeanNDVImonth[DeltaMaxMonth$Delta == name]  
  propMaxNDVI <- sum(propMaxNDVI$max == maxMonthNDVI)/length(propMaxNDVI$max)
  DeltaMaxMonth$propNDVI[DeltaMaxMonth$Delta == name] <- propMaxNDVI
  
  #NDSSI
  propMaxNDSSI <- DeltasCleaner %>%
    select(Delta, surface, year, month, ndssi) %>%
    filter(Delta == name & surface == "Water" & !is.na(ndssi)) %>%
    group_by(year) %>%
    summarise(max = month[ndssi == max(ndssi)]) %>% 
    ungroup()
  
  maxMonthNDSSI <- DeltaMaxMonth$MaxMeanNDSSImonth[DeltaMaxMonth$Delta == name]  
  propMaxNDSSI <- sum(propMaxNDSSI$max == maxMonthNDSSI)/length(propMaxNDSSI$max)
  DeltaMaxMonth$propNDSSI[DeltaMaxMonth$Delta == name] <- propMaxNDSSI
}
DeltaMaxMonth <- DeltaDatawLocations %>% 
  select(Absolute_Latitude,MaxOffset) %>% 
  merge(.,DeltaMaxMonth,by="Delta")
  
ggplot(DeltaMaxMonth, aes(y = Absolute_Latitude, x = MaxOffset)) + geom_point(aes(colour=propNDVI)) 