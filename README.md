
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Overview

<!-- badges: start -->

<!-- badges: end -->

**gtapshape** is an R package designed for the flexible aggregation of
land use and land cover data specifically tailored for use with the
[GTAP Agro-Ecological Zones (AEZ)
model](https://www.gtap.agecon.purdue.edu/resources/res_display.asp?RecordID=2605).
This package provides tools to efficiently manipulate and aggregate
spatial and country-level data on global land use and land cover to the
GTAP regions and sectors.

## Installation

You can install the latest version of `gtapshape` from
[GitHub](https://github.com/) with:

``` r
## Install devtools if you don't already have it
install.packages("devtools")
## Install gtapshapeagg from GitHub
devtools::install_github("nvilloria/gtapshape", build_vignettes = TRUE)
```

## Usage

The function `build.dbase.from.sf` processes all the data needed to
create the land use and land cover headers and sets needed to split
national land rents into subnational land rents. The function takes four
inputs: \* An R special features file with the subnational boundaries
(SF files for the 18 standard agroecological zones used in the GTAP-AEZ
model and the [14 WWF
biomes](https://en.wikipedia.org/wiki/List_of_terrestrial_ecoregions_(WWF))
are included in the package), \* The year for which the FAO data on
production, prices and area harvested should be processed (2011-2022 are
included in the package). \* The source of the gridded data on global
crop production (either [Monfreda et
al.](https://agupubs.onlinelibrary.wiley.com/doi/full/10.1029/2007GB002947)
or [CROPGRIDS](https://www.nature.com/articles/s41597-024-03247-7), both
provided in the package). \* The name of the har file that will be
generated with the physical data on land use and land cover needed by
the GTAP AEZ mode.

The processed data are stored in a
[har](https://www.copsmodels.com/webhelp/rungtap/index.html?hc_harfiles.htm)
file with eight headers:

- QCR8: Crop production (MT) for the 8 gtap crops’
- VCR8: Value of crop production (1000 USD) for the 8 gtap crops’
- HARV: Harvested area (ha) for the 8 gtap crops’
- \$QLV3: Livestock production (heads) for the 3 gtap livestock sectors’
- VLV3: Livestock output value (1000 USD) for the 3 gtap livestock
  sectors’
- LAND: Land cover areas (ha)’
- RTMB: Timber land rents (USD Million)’

In addition, `build.dbase.from.sf` writes a har file with the following
headers (sets):

- REG: GTAP regions (defaults to the GTAP database V11c).
- SUBN: Subnational boundaries (defaults to 18 AEZs)
- CRP8: The eight GTAP crop categories (pdr, wht, gro, v_f, osd, c_b,
  pfb, and ocr)
- CRP9: CRP8 + forest products (frs)
- LCOV: Seven land cover categoris (Forest, SavnGrasslnd, Shrubland,
  Cropland, Pastureland, Builtupland, Otherland)

Finally, `build.dbase.from.sf` writes a GTAP Aggregation Template text
file based on the GTAP Database V11c, that can be used to aggregate the
GTAP database.

The function defaults to splitting the data into 18 AEZs based on
production, area, and price data for year 2017, using the [Monfreda et
al. (2008)](https://doi.org/10.1029/2007GB002947) data on global gridded
production, and produces three data files with the default names:
‘gtaplulc.har’, ‘gtaplulc-sets.har’, and ‘gtaplulc-map.har’:

``` r
library(gtapshape)
## Build the land use and land cover headers and sets needed to split
## subnational land rents into 18 AEZS:
aez18 <- build.dbase.from.sf()
```

The following call to the function updates the GTAP AEZ 18 database to
year 2022, using the more recent data on global gridded crop production
and area from CROPGRIDS (the resulting files are saved as
“gtaplulc-cropgrid-2022.har”, “gtaplulc-cropgrid-2022-sets.har”, and
“gtaplulc-cropgrid-2022-map.txt”):

``` r
system.time(aez18_cropgrid_2022 <- build.dbase.from.sf(subnat_bound_file="aez18",
                                           crop_rasters = "cropgrids",
                                           year=2022,
                                           file="gtaplulc-cropgrid-2022.har"))
```

As another example, the user may choose a different scheme for the
subnational bounds, such as the 14 biomes defined by the WWF:

``` r
system.time(biome14 <- build.dbase.from.sf(subnat_bound_file="biomes14",
                                           file="gtaplulc-biome14-2017.har"))
```

The user has flexibility in changing these defaults. Users with
knowledge of R can adapt the functions and workflow to their needs.
Please refer to the following vignette for a more in-depth explanation
of the functions built into the `build.dbase.from.sf` command:

``` r
vignette('Country_level_landcover_ts', package = 'gtapshape')
```

In the companion package
[`gtapshapeagg`](https://github.com/nvilloria/gtapshapeagg) we provide
routines that split the national land rents of the standard GTAP
database using the sub-national data explained above. It also allows for
custom aggregations to different sectors and regions in a similar
fashion to
[FlexAgg2](https://www.gtap.agecon.purdue.edu/databases/flexagg2.asp).

## Data Preprocessing:

Detailed descriptions of the procedures used to process the different
land use and land cover datasets and generate the necessary inputs for
the GTAP-AEZ framework are provided in the following vignettes. Please
note, the vignettes need to performed in order due to the dependence on
certain files.

### Creating Global Administrative Areas County Raster

Executing the following code will open the vignette which documents how
the ‘gadm_rast.tif’ file included in the ‘inst/GADM’ folder of the
*gtapshape* package is created. The ‘gadm_rast.tif’ file is a raster
which depicts countries on a global scale using version 4.1 of the GADM
(Global Administrative Areas) data (included in the ‘raw_data’ folder
downloaded by ). The raw v4.1 GADM data is a global shapefile of country
boundaries available
[here](https://gadm.org/download_world.html#google_vignette).

``` r
vignette('GADM.country.raster', package = 'gtapshape')
```

### Building the shapefile with 18 AEZs:

Executing the following code will open the vignette which describes how
we created the 18 Agro-Ecological Zones (AEZ) shapefile included in this
package. We follow the procedure used to create version 11 of the
GTAP-AEZ Land Use and Land Cover database described in [Baldoz and
Corong
(2025)](https://www.gtap.agecon.purdue.edu/resources/res_display.asp?RecordID=7407).

``` r
vignette('create.18.aez.shapefile', package = 'gtapshape')
```

### Processing raster data on global land cover circa 2000:

The following code will open the vignette describing how we generate
data on the 7 land cover types present in the GTAP Land Use and Land
Cover datasets used in the *gtapshape* package. The data are created
using publicly available raster data depicting the distribution of
cropland and pastureland, potential vegetation classes, and urban areas.

``` r
vignette('land.cover', package = 'gtapshape')
```

### Downloading and processing data from FAOSTAT:

The vignette in the following code block uses the function from the
*FAOSTAT* package to download the raw data on agricultural production
and harvested area for years 2010 through 2022.

``` r
vignette('faostat.data', package = 'gtapshape')
```

### Updating land cover to specified year using FAOSTAT data:

This vignette explains how the gridded data on land cover for year 2000
(explained ) is used to estimate the shares of land covers other the
FAOSTAT values for croplands and pastures. The procedures in this
vignette allow use to update the land cover values for a given year of
FAOSTAT data.

``` r
vignette('Country_level_landcover_ts', package = 'gtapshape')
```

### Processing crop and livestock production rasters:

The two vignettes in the following code block describe how we generate
land use data in the GTAP Land Use and Land Cover datasets used in the
*gtapshape* package. The data are created using publicly available
raster data depicting global crop yields and harvested area for 172
crops and 4 livestock species. The first vignette includes code for
processing the crop data representing production circa the year 2000
(from [Monfreda, C., N. Ramankutty, and J. A. Foley
(2008)](https://doi.org/10.1029/2007GB002947)) and livestock production
for the year 2005 (from [Robinson et
al. (2014)](https://doi.org/10.1371/journal.pone.0096084)). The second
vignette goes over the processing of crop area data depicting production
in the year 2020 (from
[CROPGRIDS](https://doi.org/10.1038/s41597-024-03247-7)).

``` r
#Crop production in 2000 and livestock production in 2005 vignette
vignette('land.use', package = 'gtapshape')
#Crop production in 2020 vignette
vignette('CROPGRIDS.convert.to.dataframes', package = 'gtapshape')
```

### Pre-processing forest rents

The vignette called by the following code details how the national
timber land rents included in the package are created. The raw data used
in this vignette contain the forest rental rates included in the [GTAP
Land Use Data Base, Release 2.1, July 9,
2009](https://www.gtap.agecon.purdue.edu/resources/res_display.asp?RecordID=1900).

``` r
vignette('preprocessing.of.forest.rents', package = 'gtapshape')
```

## Authors

- **Micah Cameron-Harp (Kansas State University)**
- **Nelson B. Villoria (Kansas State University)**
- **Jayson Beckman (USDA Economic Research Service)**

## Acknowledgments

- Thanks to the [GTAP](https://www.gtap.agecon.purdue.edu/) Center for
  developing and providing open source and free access to the the
  Agro-Ecological Zones data and modeling framework. Cameron-Harp and
  Villoriaauthors acknowledge funding from the USDA Economic Research
  Service Cooperative agreement \# 58-3000-2-0087 and from USDA National
  Institute of Food and Agriculture (NIFA) under Hatch project number
  S1072 and competitive grant number 2022-10683. The findings and
  conclusions in this publication are those of the author(s) and should
  not be construed to represent any official USDA or U.S. Government
  determination or policy.

## Contact

For questions or feedback, please contact [Micah
Cameron-Harp](mailto:mcameronharp@ksu.edu) or [Nelson B.
Villoria](mailto:nvilloriap@ksu.edu).
