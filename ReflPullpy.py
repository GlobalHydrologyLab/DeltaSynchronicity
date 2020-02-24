## This is just an alternative way to pull all the SR values without using the utility.


#Import necessary libraries
import time
import ee
import os
#Initialize earth engine
ee.Initialize()

#Source necessary functions.
execfile('PullFunctions.py')

#Load in Pekel water occurance Layer if necessary. Occurence is just the 
#percentage of water occurence over a given pixel from 1985-2015.
#Set the percent occurance threshold and create a watermask from the result.
#If mask != 'Pekel', reverts to dswe water mask pulling only 2 highest confidences 
#(1/2). Maybe change this later. 

if r.mask == 'Pekel':
  PekelMask = True
  pekel = ee.Image('JRC/GSW1_0/GlobalSurfaceWater')
  threshold = r.threshold
  water = pekel.select('occurrence').gt(threshold)
  water = water.updateMask(water)
else:
  PekelMask = False
  
#Identify collection for use in sourced functions.  Right now we're only
#set up for surface relfectance, but we'll need to add TOA.
collection = r.collection  
  
#Load in Landsat Collections, eventually add S2 to this as well.
l8 = ee.ImageCollection('LANDSAT/LC08/C01/T1_SR')
l7 = ee.ImageCollection('LANDSAT/LE07/C01/T1_SR')
l5 = ee.ImageCollection('LANDSAT/LT05/C01/T1_SR')

#Identify collection for use in sourced functions.
collection = r.collection

#Standardize band names between the various collections and aggregate 
#them into one image collection

bn8 = ['B2','B3', 'B4', 'B5', 'B6','B7', 'pixel_qa']
bn57 = ['B1', 'B2', 'B3', 'B4', 'B5','B7', 'pixel_qa']
bns = ['Blue', 'Green', 'Red', 'Nir', 'Swir1', 'Swir2', 'qa']
  
ls5 = l5.select(bn57, bns)
ls7 = l7.select(bn57, bns)
ls8 = l8.select(bn8, bns)

ls = ee.ImageCollection(ls5.merge(ls7).merge(ls8))\
.filter(ee.Filter.lt('CLOUD_COVER', r.cloudyScene))

## Read in the shapefile, pull the ID column and send geometries up to EE.
shpPy = r.shapesUp
ids = shpPy[r.idCol]
geos = shpPy['geometry']

geometries = ee.FeatureCollection([ee.Feature(ee.Geometry.MultiPolygon(coords = [l.tolist() for l in geos[str(i)][0]]), {r.idCol:ids[i]}) for i in range(0,len(ids))])

#If download directory provided, check for work already pulled.  
if r.dlDir != None:
  dlDir = r.dlDir
  filesDown = os.listdir(dlDir)
  filesDown = [int(i.replace(".csv", "")) for i in filesDown]
  ids  = [i for i in ids if i not in filesDown]
  
# Finally send it all up and pull the data.                        
for x in range(0, 1): #len(ids)):
  #We're iterating over features individually.
  feature = geometries.filter(ee.Filter.eq(r.idCol, ids[x])).first()
  # Create empty shell to add attributes to for export
  shell = ee.Feature(None)
  #FilterBounds for the feature and filter to selected dates,
  #Clip the image to the feature
  #Potentially remove the clip image, it's slow if geometries are complicated.
  lsover = (ls.filterBounds(feature.geometry())
            .filterDate(str(r.dateStart),str(r.dateEnd))
            .map(clipImage))
    
  # Pull reflectance values for allthe images overlapping the feature.
  #Filter for cScore, which is basically a measure of non-land/non-water pixels
  #over the feature.
  data = lsover.map(featurePull).filter(ee.Filter.lt('cScore', r.cloudyFeature))
  dataOut = ee.batch.Export.table.toDrive(collection = data, \
                                            description = str(ids[x]),\
                                            folder = r.driveFolder,\
                                            fileFormat = 'csv')
  #Check how many existing tasks are running and take a break if it's >15  
  maximum_no_of_tasks(15, 60)
  #Send next task.
  dataOut.start()
  print('done_' + str(ids[x]))

#Make sure all Earth engine tasks are completed prior to moving on.  
maximum_no_of_tasks(1,300)
print('done_all')
