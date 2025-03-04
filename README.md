
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
are included in the package), \* The yeare for which the FAO data on
production, prices and area harvested should be processed (2011-2022 are
included in the package). \* The source of the gridded data on global
crop produuction (either [Monfreda et
al.](https://agupubs.onlinelibrary.wiley.com/doi/full/10.1029/2007GB002947)
or [CROPGRIDS](https://www.nature.com/articles/s41597-024-03247-7), both
provided in the package). \* The name of the har file with the physical
data on land use and land cover needed by the GTAP AEZ mode.

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

The function defaults to the data split into 18 AEZs with production,
area, and prices for year 2017, using the Monfreda et al. data on global
gridded produuction, and produces three data files: ‘gtaplulc.har’,
‘gtaplulc-sets.har’, and ‘gtaplulc-map.har’:

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
system.time(biome14 <- build.dbase.from.sf(subnat_bound_file="aez18",
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

In the companion package
[`gtapshapeagg`](https://github.com/nvilloria/gtapshapeagg) we provide
routines that split the national land rents of the standard GTAP
database using the subnational data explained above, and also allows for
custom aggregation to different sectors and regions in a similar fashion
to
[FlexAgg2](https://www.gtap.agecon.purdue.edu/databases/flexagg2.asp).

## Data Preprocessing:

Detailed description of the procedures used to process the different
datasets needed to obtain the land use and land cover needed in the
GTAP-AEZ framework is provided in the following vignettes:

### Building the shapefile with 18 AEZs:

``` r
vignette('create.18.aez.shapefile.html')
```

(Micah)

## Authors

- **Micah Cameron-Harp**
- **Nelson B. Villoria**

## Acknowledgments

- Thanks to the [GTAP](https://www.gtap.agecon.purdue.edu/) Center for
  developing and providing open source and free access to the the
  Agro-Ecological Zones data and modeling framework.

## Contact

For questions or feedback, please contact [Micah
Cameron-Harp](mailto:mcameronharp@ksu.edu) or [Nelson B.
Villoria](mailto:nvilloriap@ksu.edu).
