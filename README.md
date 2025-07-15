# gapproductssynopsis
This repo provides reports and the underlying code to visualize results of the groundfish assessment program bottom trawl survey data.

These figures are currently still in development as of Fall 2024, but the format should stabilize within the next year. Please raise an issue if you find any data discrepancy. 

## Viewing reports
To view the reports, please download the html files, or paste their urls into this viewer https://htmlpreview.github.io/

e.g. https://htmlpreview.github.io/?https://github.com/MattCallahan-NOAA/gapproductssynopsis/blob/main/goa/goa_Pacific_cod.html

Stand alone figure downloads are coming soon.

## Data
The [Groundfish Assessment Program (GAP)](https://www.fisheries.noaa.gov/alaska/science-data/groundfish-assessment-program-bottom-trawl-surveys) conducts bottom trawl surveys annually in the Bering Sea and semi-annually in the Gulf of Alaska (odd years) and Aleutian Islands (even years).
Data from these surveys are available through the [GAP_PRODUCTS](https://github.com/afsc-gap-products/gap_products) tables on the AFSC oracle database, and are also available through the AKFIN database. 
For these reports, data are pulled from the AKFIN database using the [akfingapdata](https://github.com/MattCallahan-NOAA/akfingapdata) R package.

These visualizations use estimates for entire survey regions. For the Aleutians this includes the "Southern Bering Sea". For the Bering Sea this includes the Northwest

## Workflow
generate_synopsis.R is the workhorse of these reports. It loads data using akfingapdata functions. It also contains most data wrangling and figure code. The CPUE mapping function is contained in make_idw_map_gs.R, which generate_synopsis.R sources (make_idw_map_nolegend.R is also used for Aleutian plots). The output of this function is a list of figures.

draft_figs_quarto.qmd is parameterized quarto document that calls generate_synopsis.R and runs it for the species and area specified in its parameters. draft_figs_noage.qmd is essentially the same file, but run for species without age comp estimates. 

run_all_reports.R is the file that takes a list of species codes, determines whether they have age comp estimates, and then runs the appropriate .qmd in a loop for all species codes in the desired area. There is currently some babysitting required as the reports may fail for various reasons. This will hopefully be improved upon soon.

## Figures

### Biomass
Biomass estimates are plotted using data from the akfin_biomass table. The confidence intervals are standard deviations (square root of the biomass variance estimate provided by GAP).
The long term mean includes the current year.

### Size comps
Length compositions from the akfin_sizecomp table.

### Age comps
Age compositions from the akfin_agecomp table. 
Note that only species with age samples will have this figure.
Also note that for the Bering Sea the area includes Northwest corner stations. This is probably most relevant for the age comp data (area_id_footprint = "EBS STANDARD PLUS NW").

### Growth
This figure uses length and age data from the akfin_specimen table. We use the vbmod function in the FSA R package to fit a von Bertalanffy growth model to the data. 
Note that only species with age samples will have this figure.

### Length/weight
This figure uses length and weight data from the akfin_specimen table. It plots a log linear model fitted to the length and weight data in real (non log) space, but displays the log coefficients. 

### CPUE maps
These use a modified version of the [akgfmaps](https://github.com/afsc-gap-products/akgfmaps) package make_idw_map function to plot CPUE for the last three surveys. CPUE data is loaded for the last three years from the akfin_cpue table and joined to the akfin_haul table for plotting. Map color breaks are set for the combined years. Then CPUE maps for each year are generated separately, and combined into a single figure with the same color scale. 

### Sample size
This is just a table generated in ggplot. It uses length data from the akfin_length table and sums the frequency column to get sample size. Age and weight data are derived from the akfin_specimen table. The age sample size counts fish that have an age assigned, not the number of otoliths collected. There is a gap_products.samplesize view on the AFSC oracle database, which is currently not available through AKFIN. 
