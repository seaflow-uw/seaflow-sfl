# seaflow-sfl

SFL files contains metadata for each SeaFlow files recorded during the cruise. These metadata are provided by the ship's data broadcast system and are being copied to the SeaFlow instrument as is.

The metadata contains the following information:

- FILE: SeaFlow filename

- DATE: data and time in GMT

- FILE DURATION: acquisition time (sec), usually set at 180

- LAT: latitude (deg N)

- LON: longitude (deg W)

- CONDUCTIVITY: seawater conductivity (s/m)

- SALINITY: seawater salinity (psu)

- OCEAN TEMP: seawater temperature (deg C)

- PAR: Photosynthetic Active Radiations above surface water (µmol/m2/s)

- BULK RED: bulk red fluorescence measured by SeaFlow (unitless)

- STREAM PRESSURE: pressure of the sample (psi), usually set at 12

- EVENT RATE: number of events (i.e., particles) recorded per second (should be below 18,000 for quality data)


<iframe width="900" height="800" frameborder="0" scrolling="no" src="//plot.ly/~ribalet/1.embed"></iframe>


***François Ribalet, Chris Berthiaume, Sophie Clayton, Megan Schatz and Jarred Swalwell contributed to this project***
