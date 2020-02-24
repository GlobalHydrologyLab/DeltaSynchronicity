reflectancePull <- function(pyEnv = "earthEngineGrabR", 
                            driveFolder = '__dumbdumbtesttest', 
                            shapefile = 'out/ryanfire.shp', 
                            idCol = 'fid', 
                            mask = 'dswe', 
                            threshold = 80, 
                            collection = 'SR', 
                            cloudyScene = 30, 
                            cloudyFeature = 25, 
                            dateStart = '1984-01-01', 
                            dateEnd = as.character(Sys.Date()), 
                            dlDir = NULL, 
                            pullName = ''){
  
  save(pyEnv, driveFolder, shapefile, idCol, mask, threshold, collection, 
       cloudyScene, cloudyFeature, dateStart, dateEnd, dlDir, pullName,
       file = 'tmpPullArgs.Rdata', 
       envir = environment())
  
  rmarkdown::render(input = 'functions/ReflectancePull.Rmd', 
                    output_file = paste0('pullDescriptions/ReflectancePull_',pullName,'.html'))
  
  file.remove('tmpPullArgs.Rdata')
}