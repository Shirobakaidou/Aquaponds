# This Script calculates zonal statistics of ALL year for ONE roi

import os
from glob import glob

# Global Setting: "bandNo - year" Pairs
# Pair up Time Stamp Index and Years: In order to match year and band number 
keys = [str(i) for i in range(36)]
years = [i for i in range(1984,2020)]
pair = dict([[i,j] for i,j in zip(keys, years)])
print(pair)

# Load Vectors
path_v = "C:/EAGLE/AquaPonds/Data/zonstats_qgis"
file_v = glob(os.path.join(path_v, "*667*"))
#print(file_v)
vlayer = iface.addVectorLayer(file_v[0], "", "ogr")

# Load Rasters
path_r = "C:\EAGLE\AquaPonds\Data\Images\wmsk_wifi_global_thrhd"
file_r = glob(os.path.join(path_r, "*667*.tif"))
#print(file_r)
rlayer = iface.addRasterLayer(file_r[0], "wmsk_667", "gdal")

# Band Count of Image
band_count = rlayer.bandCount()

# Band Index of Image X
band_index = list(range(1, band_count+1))

# Band Names of Image
band_names = [rlayer.bandName(i) for i in band_index]

# Band Index of Image X on Time Series
# .strip() for remove leading whitespace
band_index_ts = [i.split(":")[1].split("_")[0].strip() for i in band_names]

# Pair up the Index
#band_pairs = dict([[i,j] for i,j in zip(band_index, band_No)])

for i in band_index:
    
    zonStats_param = {
        'INPUT_RASTER': rlayer,
        'INPUT_VECTOR': vlayer,
        'RASTER_BAND': i,
        'STATS': [9],
        'COLUMN_PREFIX': str(pair.get(band_index_ts[i-1]))+'_'
    }
    processing.run("qgis:zonalstatistics", zonStats_param)