import spectral.io.envi as envi
import pandas as pd
import numpy as np

img = envi.open('Data_AU_WaSSI.hdr', 'Data_AU_WaSSI')
imginfo=img.shape

parameters=pd.read_csv('parameters.txt',sep=" ")
parameters['value'][]

nrows=imginfo[0]
ncols=imginfo[1]
print("nrows=")
print(nrows)
print("ncols=")
print(ncols)
# set default parameters for data processing
## set the start and end year of input data for climate, LAI and land cover data

S_y=parameters['value'][1-1]
E_y=parameters['value'][2-1]
S_y_LAI=parameters['value'][3-1]
E_y_LAI=parameters['value'][4-1]
S_y_LC=parameters['value'][5-1]
E_y_LC=parameters['value'][6-1]

## set the start line for each parameters of the ENVI data

Line_DEM=parameters['value'][7-1]
Line_S_PRE=parameters['value'][8-1]
Line_E_PRE=parameters['value'][9-1]
Line_S_TEMP=parameters['value'][10-1]
Line_E_TEMP=parameters['value'][11-1]
Line_S_LAI=parameters['value'][12-1]
Line_E_LAI=parameters['value'][13-1]
Line_S_LC=parameters['value'][14-1]
Line_E_LC=parameters['value'][15-1]
Line_S_SOIL= parameters['value'][16-1]

## the coordation of the left-up pixel and the size of pixel
  
S_lat= parameters['value'][17-1]
cell_size=parameters['value'][18-1]
S_long= parameters['value'][19-1]
#----------

bands=list(range(imginfo[2]))
data=img.read_bands(bands)

soil=open("INPUTS/SOILINFO.TXT","w")
climate=open("INPUTS/CLIMATE.TXT","w")
cellinfo=open("INPUTS/CELLINFO.TXT","w")
lai=open("INPUTS/LANDLAI.TXT","w")
veg=open("INPUTS/VEGINFO.TXT","w")

for nrow in range(0,nrows):
	for ncol in range(0,ncols):
		for YEAR in range(YEAR_START,YEAR_END+1):
			for MONTH in range(1,13):
			
				if YEAR >= S_y and YEAR <=E_y:
				
					input=list(nrow*ncols+ncol+1,YEAR,MONTH,data[nrow,ncol,Line_S_PRE],data[nrow,ncol,Line_S_TEMP])
					input=np.array(input)
					input=np.reshape(input,(1,5))
					input = pd.DataFrame(input)
					input.to_csv(climate, index=False, index_label=None,header=False)
					climate.writelines(input)
					if Line_S_PRE<Line_E_PRE:
						Line_S_PRE=Line_S_PRE+1
						Line_S_TEMP=Line_S_TEMP+1
					else:
						Line_S_PRE=Line_S_PRE-1
						Line_S_TEMP=Line_S_TEMP+1
					
				if YEAR >= S_y_LAI and YEAR <=E_y_LAI:
					
				else:
					
				if YEAR >= S_y_LAI and YEAR <=E_y_LAI:
					
				else:
					





import sys
sys.getsizeof(da)