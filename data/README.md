### EDS_parameters.csv

**Summary -** Here you will find information regarding the columns within the EDS_parameters.csv file; how to fill them out and what they are used for. This file is where information for new datasets you want to download will be added. It is used within the 2a_Download_ERDDAP.R script. 
 
**PARAMETER.NAME:**  This will become the name of the folder your data is stored in. It can be whatever makes the most sense to you.

**DOWNLOAD:** When running the download script, this will determine if this data will be downloaded. This way you can redownload only one dataset (ex: because you want to include more dates) without removing info on others.

**SENSOR_DATASET:** The institution where the dataset originated from. This is not used in EDS, but is included as a reference for users that are interested in where the data comes from.

**FREQUENCY:** This can be found in the ERDDAP metadata. The current options are Climatology, Monthly, 8day, Daily.

**URL:** The url of the host where the data is downloaded from. We currently only download from two ERDDAP servers: OceanWatch (https://oceanwatch.pifsc.noaa.gov/erddap/) and CoastWatch ((https://coastwatch.pifsc.noaa.gov/erddap/)

**DATASET.ID:** The dataset ID needs to be the same as provided in the metadata on the ERDDAP server. This ID (along with the url) will be used to find the data you are trying to download.
![pic1](https://user-images.githubusercontent.com/93793084/202333536-2d2453e9-5e0e-4576-8ae1-9164fe9e377f.PNG)

**GRID.VARIABLE:** The grid variable needs to be the same as provided in the metadata on the ERDDAP server. This will be used to determine which variable you want to download from the dataset previously specified since there can be multiple variables in one dataset.

![pic2](https://user-images.githubusercontent.com/93793084/202334183-30f93721-2f03-45dc-bc6f-7684ac53a4a8.PNG)

**START_DATE:** The start date you are interested in downloading. Climatology datasets do not require an actual START_DATE. Instead the column is filled out with useful information for that dataset since it is included in the name of the file  (E.g."Bathymetry_ETOP1_Depth_M").

**STOP_DATE:**  The stop date you are interested in downloading. Climatology datasets do not require an actual STOP_DATE. Instead the column is filled out with useful information for that dataset since it is included in the name of the file  (E.g."Bathymetry_ETOP1_Depth_M").

**TIMESTEP:** The number of timesteps based on the start date, end date, and frequency. This calculation can be done manually. Ex: start date = 1/12/2019, end date = 4/3/202019, frequency = Monthly, timestep =  3

**BLOCKSIZE:** The default for this is 10,000. If you attempt to download a large amount of data from an ERDDAP server all in one block, the likelihood of your connection to the server timing out is high. With the implementation of block sizes, the data to be downloaded is separated into blocks, each with its own new connection to the ERDDAP server.

**Summaries:** The types of datasummaries will be conducted in the 2a_Download_ERDDAP.R script. Summary statistics are also rerun later on in the EDS process.
 
 **MaskForOceanColor:** This will either be TRUE or FALSE. Datasets like chlorophyll need to be masked when close to shore.

*Last updated on: November 16th, 2022*
 
---
 
### Island_Extents.csv 

**Summary -** This file is created in 1_Create_Bounding_Boxes.R. It includes bounding box coordinates encompassing each island within the Pacific Islands Fisheries Science Center region of interest. 
