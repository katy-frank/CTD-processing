# CTD-processing

*Note*: Make sure that you set your working directory to where you've stored this script (or modified it to change the file paths associated with the configuration files) before running.

## Configuration
Configuration is handled by two csv files:
- **global_parameters.csv**
- **station_details.csv**

### Global Parameters
Global parameters are parameters that apply to all data from the cruise. These are set in the file *global_parameters.csv*. They are:
- **input_folder**: The path to the folder that the RAW data files are stored in, e.g. /20220725_WE/RAW/
- **output_folder**: The path to the folder where the OUTPUT directory will be created and processed data written to, e.g. /20220725_WE/
- **lake**: The lake that the cruise took place on, e.g. Erie
- **vessel**: The vessel used for the cruise, e.g. R4108
- **program**: The program for the cruise, e.g. HABS
- **shortname**: The abbreviated name for this group of stations that should appear in the output summary file name. Historically this has been the station/sitename without its numeric id. For example, for a group of stations like WE8, WE4, and WE6 the shortname has historically been WE.

### Station Details
On each cruise, multiple stations are typically visited. This results in the creation of multiple raw CTD data files, with names that end in a number e.g. 0001, 0002, etc. However, sometimes a particular data file is corrupt/shouldn't be used. The configuration file *station_details.csv* should be used to list the station names that were visited, the number associated with the raw CTD file for that station (e.g. if WE8 goes with the data file ending in 0001, use 1 for this value), and a value indicating whether surface only or surface and depth samples were taken for this station. List as many stations as were visited in this file.
- **station**: The name of the station, e.g. WE8
- **cast_number**: The number at the end of the raw data file associated with this station. For example, if the file SBE19plus_01906337_2022_07_25_0001.asc holds data for WE8, then cast_number would be 1. Another example: SBE19plus_01906337_2022_07_25_0032.asc, cast_number would be 32.
- **surface_depth**: If only surface samples were taken for this station, enter 1. If surface and depth samples were taken, enter 2.


## More Information
See standard operating procedure document for detailed instructions: https://docs.google.com/document/d/1cvMNQeB9_rFZINc6xgzRWdxKIWmrATwF/edit?usp=sharing&ouid=103453782401265965636&rtpof=true&sd=true
