# Code for: "Characteristics of Power Plant Siting and Equity"

The following includes descriptions of data, code, and functions used for data analyses and visualizations for the paper titled, ""

Please cite as: 


### Data Descriptions

1. **`tr_sf.Rdata`**
   - *Description:* This file contains geospatial data at the tract level, identified by GEOID.

2. **`st.shp`**
   - *Description:* This file contains state-level geospatial data.

3. **`data.generators.csv`**
   - *Description:* This dataset provides individual generator information, including prime mover, fuel type, capacity, operating start year, and geospatial data.

4. **`powerplant_grouping.xlsx`**
   - *Description:* This data file contains power plant groups with associated codes.

5. **`pplants_tract_"buffers".csv`**
   - *Description:* This set of data files includes generator information joined with intersected census tracts using various buffer sizes.

6. **`pplants_county_"buffers".csv`**
   - *Description:* Similar to the previous set, this collection of data files contains generator information joined with intersected counties using various buffer sizes.

7. **`substation_tract_"buffers".csv`**
   - *Description:* This set of data files includes susbstation information joined with intersected census tracts using various buffer sizes.

8. **`substation_county_"buffers".csv`**
   - *Description:* Similar to the previous set, this collection of data files contains susbstation information joined with intersected counties using various buffer sizes.

9. **`TL.shp`**
   - *Description:* This file contains transmission line geospatial data.

10. **`demographic_df_feats_tract.csv`**
      - *Description:* This data file contains various sociodemographic data from ACS 2021 for each census tract.

11. **`demographic_df_feats_county.csv`**
      - *Description:* Similar to the tract-level file, this dataset contains various sociodemographic data from ACS 2021, but for each county.

12. **`DAC_s.csv`**
      - *Description:* This data file includes socio-demographic indicators based on ACS 2021 and EJ and Supplemental indices (Environmental Justice Screening Tool), disadvantaged community classification (CEJST), and DAC scores (Energy Justice Mapping Tool).

### Script Descriptions

1. **`Data.R`**
   - *Description:* This file serves as a repository for essential libraries and data required for analysis, ensuring necessary resources are readily available for analytical processes.

2. **`Cleaning.R`**
   - *Description:* This script outlines procedures for data cleaning and organization for generators, leading to the generation of final datasets. It encompasses steps necessary to prepare the data for analysis.

3. **`Cleaning_s.R`**
   - *Description:* This script outlines procedures for data cleaning and organization for substations, leading to the generation of final datasets. It encompasses steps necessary to prepare the data for analysis.

4. **`Cleaning_t.R`**
   - *Description:* This script outlines procedures for data cleaning and organization for transmission lines, leading to the generation of final datasets. It encompasses steps necessary to prepare the data for analysis.

5. **`Function.R`*
   - *Description:* This script contains the code for executing various analytical functions and methods. It serves as the engine behind the analysis, facilitating the investigation of research questions and hypotheses.

6. **`Code.R`**
   - *Description:* This script executes the code for generating visual representations (figures) for Supplemental Information (SI).   

7. **`Summary.R`**
   - *Description:* This script executes the code for generating visual representations (figures) for the manuscript and performing analyses. It plays a crucial role in translating data and research methods into meaningful insights and results.


### Function Descriptions

Below are detailed explanations of each function:

1. **`donut`**:
   - *Description:* This function generates a boxplot illustrating the distribution of sociodemographic features for a specified power system infrastructure type, categorized by distance.
   - *Input:* 
      - Data: Area-weighted sociodemographic features for power system infrastructures with buffer sizes of 0.25, 0.5, 1, 2, 3, and 5 miles.
      - Power system infrastructure Type: A parameter specifying the type of power system infrastructure.
   - *Output:* The function produces a boxplot depicting the distribution of sociodemographic features based on distance.

2. **`box_pl`**:
   - *Description:* This function visualizes the boxplot illustrating the distribution of sociodemographic features for a specified power system infrastructure type, distinguishing between host and non-host tracts or counties, categorized by distance.
   - *Input:*
      - Data: Sociodemographic features for host or non-host census tracts or counties, with buffer sizes of 0, 1, and 3 miles.
      - Power system infrastructure Type: A parameter specifying the type of power system infrastructure.
      - Administrative Area: A parameter specifying the administrative type, either "Tract" or "County".
   - *Output:* The function generates a boxplot displaying the distribution of sociodemographic features categorized by distance and host vs. non-host census tracts or counties.

3. **`regr_pl`**:
   - *Description:* This function visualizes the results of logistic regression models for a specified power system infrastructure type, considering sociodemographic features for host or non-host census tracts or counties, with buffer sizes of 0, 1, and 3 miles.
      - Data: power system infrastructure information, including prime mover, fuel type, capacity, operating start year, and geospatial data.
      - Power system infrastructure Type: A parameter specifying the type of power system infrastructure.
      - Administrative Area: A parameter specifying the administrative type, either "Tract" or "County".
      - Buffer Size: A parameter specifying the buffer size (0, 1, or 3 miles).
   - *Output:* The function generates a figure illustrating the effect of significant predictors in terms of the odds ratio.

4. **`mpping`**:
   - *Description:* This function visualizes specified power system infrastructure locations in the U.S., including an indication of whether the power system infrastructures are located in disadvantaged communities.
   - *Input:*
      - Data: power system infrastructure information, including prime mover, fuel type, capacity, operating start year, and geospatial data.
      - Power system infrastructure Type: A parameter specifying the type of power system infrastructure.
   - *Output:* The function returns a figure displaying the power system infrastructure siting and associated data, such as capacity, community categories of low income and race, and whether it is sited in disadvantaged communities.