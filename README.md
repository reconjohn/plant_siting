# Code for: "Disparities in the location of electric power system infrastructure in the United States"

The following includes descriptions of data, code, and functions used for data analyses and visualizations for the paper titled, "Disparities in the location of electric power system infrastructure in the United States"

Please cite as: 



## Script Descriptions

### 1. `Data.R`
- Loads essential libraries, data, and functions required for analysis.

### 2. `Code.R`
- Generates visual outputs (figures) for the main manuscript.

### 3. `SI.R`
- Generates visual outputs (figures) for the Supplemental Information (SI) section.

---

## Data Descriptions

### 1. `tr.sf`
- Contains geospatial data directly downloaded from the U.S. census at the census tract level, identified by GEOID. 

### 2. `demographic_df_feats_tract.csv`
- Sociodemographic data from ACS 2021 for each census tract.

### 3. `demographic_df_feats_county.csv`
- Sociodemographic data from ACS 2021 for each county, mirroring the tract-level dataset.

### 4. `data.Rdata`
- Generator datasets:
  - `pw`: Generators with weighted sociodemographic features by buffer size.
  - `ah`: Host community identification by generator type, buffer size, and spatial unit (tract or county).
  - `pm`: Individual generator geospatial data for mapping.

### 5. `data_s.Rdata`
- Substation datasets:
  - `pw_s`: Substations with weighted sociodemographic features by buffer size.
  - `ah_s`: Host community identification by substation voltage type, buffer size, and spatial unit.
  - `pm_s`: Individual substation geospatial data for mapping.

### 6. `TL.Rdata`
- Transmission line datasets:
  - `tl_d`: Transmission lines with weighted sociodemographic features by buffer size.
  - `ah_t`: Host community identification by transmission line voltage type, buffer size, and spatial unit.

### 7. `TL.shp`
- Geospatial shapefile for transmission lines.

### 8. `DAC_s.csv`
- Includes:
  - ACS 2021 sociodemographic indicators.
  - EJ and Supplemental indices (Environmental Justice Screening Tool).
  - CEJST disadvantaged community classification.
  - DAC scores from the Energy Justice Mapping Tool.



