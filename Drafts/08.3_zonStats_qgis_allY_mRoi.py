# This Script calculates zonal statistics of ALL year for ONE roi

import os
from glob import glob

# Global Setting: "bandNo - year" Pairs
# Pair up Time Stamp Index and Years: In order to match year and band number 
keys = [str(i) for i in range(36)]
years = [i for i in range(1984,2020)]
pair = dict([[i,j] for i,j in zip(keys, years)])
#print(pair)

mypath = "C:/EAGLE/AquaPonds/Data/zonstats_qgis/v_r_pairs/1VmR/Subsets/"
myfiles = glob(os.path.join(mypath, '*'))

for i in myfiles:
    file_v = glob(os.path.join(i, "*.geojson"))[0]
    file_r = glob(os.path.join(i, "*.tif"))[0]
    
    # Load Vector and Raster
    vlayer = iface.addVectorLayer(file_v, "", "ogr")
    rlayer = iface.addRasterLayer(file_r, file_r.split("\\")[-2], "gdal")
    
    # Band Count of Image
    band_count = rlayer.bandCount()

    # Band Index of Image X
    band_index = list(range(1, band_count+1))

    # Band Names of Image
    band_names = [rlayer.bandName(i) for i in band_index]

    # Band Index of Image X on Time Series
    band_index_ts = [i.split(":")[1].split("_")[0].strip() for i in band_names]
    
    for j in band_index:
        
        zonStats_param = {
            'INPUT_RASTER': rlayer,
            'INPUT_VECTOR': vlayer,
            'RASTER_BAND': j,
            'STATS': [9],
            'COLUMN_PREFIX': str(pair.get(band_index_ts[j-1]))+'_'
        }
        processing.run("qgis:zonalstatistics", zonStats_param)