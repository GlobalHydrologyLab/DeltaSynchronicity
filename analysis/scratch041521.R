#Finding proportion of years where max month is max
DeltaNames <- unique(DeltasCleaner$Delta)

DeltaMaxMonth <- select(DeltaMaxMin,Delta,MaxMeanNDVImonth,MaxMeanNDSSImonth)
DeltaMaxMonth <- cbind(DeltaMaxMonth, 
                       propNDVI = 0, prop3NDVI = 0,
                       propNDSSI = 0, prop3NDSSI = 0)

for (name in DeltaNames) {
  #NDVI
  MaxNDVI <- DeltasCleaner %>%
    select(Delta, surface, year, month, ndvi) %>%
    filter(Delta == name & surface == "Land" & !is.na(ndvi)) %>%
    group_by(year) %>%
    summarise(max = month[ndvi == max(ndvi)]) %>% 
    ungroup() 
  
  MaxNDVI <- MaxNDVI %>%  #Add plus and minus 1 month
    mutate(.,minus1 = ifelse(max == 1,12,max-1),
           plus1 = ifelse(max == 12,1,max+1))
  
  maxMonthNDVI <- DeltaMaxMonth$MaxMeanNDVImonth[DeltaMaxMonth$Delta == name]
  propMaxNDVI <- sum(MaxNDVI$max == maxMonthNDVI)/length(MaxNDVI$max)
  prop3MaxNDVI <- sum(MaxNDVI$max == maxMonthNDVI | 
                       MaxNDVI$plus1 == maxMonthNDVI |
                       MaxNDVI$minus1 == maxMonthNDVI)/
                     length(MaxNDVI$max)
  DeltaMaxMonth$propNDVI[DeltaMaxMonth$Delta == name] <- propMaxNDVI
  DeltaMaxMonth$prop3NDVI[DeltaMaxMonth$Delta == name] <- prop3MaxNDVI
  
  #NDSSI
  MaxNDSSI <- DeltasCleaner %>%
    select(Delta, surface, year, month, ndssi) %>%
    filter(Delta == name & surface == "Water" & !is.na(ndssi)) %>%
    group_by(year) %>%
    summarise(max = month[ndssi == max(ndssi)]) %>% 
    ungroup()
  
  MaxNDSSI <- MaxNDSSI %>%  #Add plus and minus 1 month
    mutate(.,minus1 = ifelse(max == 1,12,max-1),
           plus1 = ifelse(max == 12,1,max+1))
  
  maxMonthNDSSI <- DeltaMaxMonth$MaxMeanNDSSImonth[DeltaMaxMonth$Delta == name]  
  propMaxNDSSI <- sum(MaxNDSSI$max == maxMonthNDSSI)/length(MaxNDSSI$max)
  prop3MaxNDSSI <- sum(MaxNDSSI$max == maxMonthNDSSI | 
                        MaxNDSSI$plus1 == maxMonthNDSSI |
                        MaxNDSSI$minus1 == maxMonthNDSSI)/
                      length(MaxNDSSI$max)
  DeltaMaxMonth$propNDSSI[DeltaMaxMonth$Delta == name] <- propMaxNDSSI
  DeltaMaxMonth$prop3NDSSI[DeltaMaxMonth$Delta == name] <- prop3MaxNDSSI
}
#Add in latitude and MaxOffset to plot proportions
DeltaMaxMonth <- DeltaDatawLocations %>% 
  select(Absolute_Latitude,MaxOffset) %>% 
  merge(.,DeltaMaxMonth,by="Delta")

#Plot each index with continuous colors
ggplot(DeltaMaxMonth, aes(y = Absolute_Latitude, x = MaxOffset)) + geom_point(aes(color = propNDVI)) 
ggplot(DeltaMaxMonth, aes(y = Absolute_Latitude, x = MaxOffset)) + geom_point(aes(color = prop3NDVI)) 
ggplot(DeltaMaxMonth, aes(y = Absolute_Latitude, x = MaxOffset)) + geom_point(aes(color = propNDSSI)) 
ggplot(DeltaMaxMonth, aes(y = Absolute_Latitude, x = MaxOffset)) + geom_point(aes(color = prop3NDSSI)) 

#Plot each index with color thresholds
ggplot(DeltaMaxMonth, aes(y = Absolute_Latitude, x = MaxOffset)) +
  geom_point(aes(color = ifelse(propNDVI<0.084,'<0.084','>0.084'))) +
  labs(color = 'propNDVI')
ggplot(DeltaMaxMonth, aes(y = Absolute_Latitude, x = MaxOffset)) +
  geom_point(aes(color = ifelse(prop3NDVI<0.25,'<0.25','>0.25'))) +
  labs(color = 'prop3NDVI')
ggplot(DeltaMaxMonth, aes(y = Absolute_Latitude, x = MaxOffset)) + 
  geom_point(aes(color = ifelse(propNDSSI<0.084,'<0.084','>0.084'))) +
  labs(color = 'propNDSSI')
ggplot(DeltaMaxMonth, aes(y = Absolute_Latitude, x = MaxOffset)) + 
  geom_point(aes(color = ifelse(prop3NDSSI<0.25,'<0.25','>0.25'))) +
  labs(color = 'prop3NDSSI')

#Calculate ASI from Feng et al 2019, eq 1-3

#First get monthly mean NDVI and NDSSI for each delta
DeltaMeansNDVI <- DeltaMeans %>% 
  filter(surface == "Land") %>% 
  select(Delta,month,MeanNDVI)

DeltaMeansNDSSI <- DeltaMeans %>% 
  filter(surface == "Water") %>% 
  select(Delta,month,MeanNDSSI)

DeltaMeansASI <- left_join(DeltaMeansNDVI,DeltaMeansNDSSI) %>% 
  mutate(mNDVI = MeanNDVI + 1, mNDSSI = MeanNDSSI + 1) %>% #Transform both indices so non-negative
  select(Delta,month,mNDVI,mNDSSI)
  
#Now calculate Jansen-Shannon distance of observations

DeltaMeansASI <- DeltaMeansASI %>% #Requires more than one group/ungroup by Delta to get order correct
  group_by(Delta) %>% 
  mutate(sumNDVI = sum(mNDVI), sumNDSSI = sum(mNDSSI)) %>% #Sums of variables for pmf
  ungroup() %>% 
  mutate(pNDVI = mNDVI/sumNDVI, pNDSSI = mNDSSI/sumNDSSI, #pmfs, mean pmf, information
         pM = 0.5 * (pNDVI + pNDSSI), 
         iNDVI = pNDVI * log2(pNDVI/pM), iNDSSI = pNDSSI * log2(pNDSSI/pM)) %>% 
  group_by(Delta) %>%
  mutate(Dndvi = sum(iNDVI), Dndssi = sum(iNDSSI)) %>%  #Calculate entropies
  ungroup()

JS <- DeltaMeansASI %>% 
  filter(.,DeltaMeansASI$month == 1) %>% 
  mutate(JSobs = sqrt(0.5*Dndvi + 0.5*Dndssi)) %>% 
  select(Delta,JSobs)

#Now min JS. First some functions
roll <- function( x , n ){ #function to help offset
  if( n == 0 )
    return( x )
  c( tail(x,n) , head(x,-n) )
}

calcMinJS <- function(x,y){ #Calculate JSmin from two vectors NEED TO VECTORIZE
  JSall <- data.frame(offset = 0:11,JSw = 0)
  for (w in JSall$offset){
    temp <- data.frame(v1 = x, v2 = roll(y,w)) 
    JSall$JSw[w+1] <- temp %>% 
      mutate(pv1 = v1/sum(v1), pv2 = v2/sum(v2),
             M = 0.5*(pv1+pv2),
             dv1 = sum(pv1*log2(pv1/M)), dv2 = sum(pv2*log2(pv2/M)),
             JStemp = sqrt(0.5*dv1 + 0.5*dv2)) %>% 
      pull(JStemp[1])
  }
  min(JSall$JSw)
}

#Now add JSmin to JS
JS$JSmin <- DeltaMeansASI %>% 
  group_by(Delta) %>% 
  mutate(JSmin = calcMinJS(mNDVI,mNDSSI)) %>% 
  ungroup() %>% 
  filter(month == 1) %>% 
  pull(JSmin)

#Calculate ASI and add latitude  
DeltaASI <- JS %>% 
  mutate(ASI = sqrt(JSobs - JSmin)) %>% 
  merge(.,DeltaDatawLocations,by="Delta") %>% 
  select(Delta,JSobs,JSmin,ASI,Absolute_Latitude,Lat,Lon) %>% 
  rename(AbsLat = Absolute_Latitude)

#Plot scatter of JSobs and ASI and map
ASIplot <- ggplot(DeltaASI, aes(y = AbsLat, x = ASI)) + geom_point()
JSobsplot <- ggplot(DeltaASI, aes(y = AbsLat, x = JSobs)) + geom_point()

world <- ggplot() +
  borders("world", colour = "gray85", fill = "gray80") +
  theme_map() 
DeltaASIrangeMap <- world + 
  geom_point(aes(x = Lon, y = Lat, color = ASI),
             data = DeltaASI,
             size = 5) + scale_color_gradient( high = "red", low  = "yellow") + 
  ggtitle("global Async")

DeltaASIrangeMap

#Plot max/min month for each delta
MaxNDVIMonth <- ggplot(DeltaDatawLocations, aes(y = Lat, x = MaxMeanNDVImonth)) + 
  geom_point() +
  scale_x_discrete(limits = c(1:12), breaks = c(1:12))
MinNDVIMonth <- ggplot(DeltaDatawLocations, aes(y = Lat, x = MinMeanNDVImonth)) + 
  geom_point() +
  scale_x_discrete(limits = c(1:12), breaks = c(1:12))
MaxNDSSIMonth <- ggplot(DeltaDatawLocations, aes(y = Lat, x = MaxMeanNDSSImonth)) + 
  geom_point() +
  scale_x_discrete(limits = c(1:12), breaks = c(1:12))
MinNDSSIMonth <- ggplot(DeltaDatawLocations, aes(y = Lat, x = MinMeanNDSSImonth)) + 
  geom_point() +
  scale_x_discrete(limits = c(1:12), breaks = c(1:12))

MaxNDVIMonth
MinNDVIMonth
MaxNDSSIMonth
MinNDSSIMonth