## nwfscDiscard

Code developed to evaluate the confidentiality by strata and to provide bootsrap estimates of discard estimates for West Coast Groundfish Observer Program (WCGOP) data. Code maintained by @chantelwetzel-noaa.

Note: this code was developed for use by scientists at the Northwest Fisheries Science Center and is intended to work on the specific data products that we have access to using methods specific to the needs of this group.

## About the West Coast Groundfish Observer Program

WCGOP has been using on-vessel observers off the U.S. West Coast since 2002 to collect data on discarding practices. The on-vessel observer coverage varies by fishing sector. The Catch Share and At-sea Hake fisheries have observers on 100% of all fishing trips. The Electronic Monitoring (EM) program has 100% electronic monitoring and a target of 30% human observation for scientific data collection. All other observed fisheries have less than 100% observer coverage. [WCGOP](https://www.fisheries.noaa.gov/west-coast/fisheries-observers/west-coast-fishery-observer-bycatch-and-mortality-reports#fishery-management-reports) produces a number of annual reports providing information on observer coverage, groundfish mortality, etc. The 2024 [observer coverage report]https://repository.library.noaa.gov/view/noaa/61904) and the [annual groundfish mortality report](https://repository.library.noaa.gov/view/noaa/55949) can be found online. Detailed information on the fishery sectors observed by WCGOP is available [online](https://www.fisheries.noaa.gov/west-coast/fisheries-observers/fishery-sectors-covered-west-coast-groundfish-observer-program-and).

WCGOP observers a wide-range of fishing sectors for vessels participating in the
Individual Fishing Quota (IFQ, also referred to as catch share) fishery and 
non-catch share fisheries. Groundfish stock assessment most often define fleet 
structures within assessment models based on gear types. WCGOP data contains data
for the following gear groups: bottom trawl, fixed gears, hook & line, midwater
trawl, pot, and shrimp trawl. Grouping data based upon gear types will include data
from multiple sectors which may or may not catch particular species. Each gear 
type includes the data from the following sectors:

- bottom trawl: catch shares, limited entry Pacific halibut, limited entry trawl, 
open access California halibut, and sea cucumber

- fixed gear: nearshore

- hook & line: catch shares, directed Pacific halibut, limited entry fixed gear DTL,
limited entry sablefish, and open access fixed gear

- midwater trawl: catch shares, limited entry trawl, midwater hake, and midwater
rockfish

- pot: catch shares, limited entry sablefish, and open access fixed gear

- shrimp trawl: pink shrimp and ridgeback prawn. 

The groundfish expanded multi-year mortality (GEMM) report can be used to understand
how much mortality (landed and discarded) is coming from each sector for any particular 
species. Examining these data can help analysts understand how best to use these
data within their assessment.  The GEMM data can be accessed using the `pull_gemm` 
function in the [nwfscSurvey](https://github.com/pfmc-assessments/nwfscSurvey) package. 

As noted above, the observer coverage varies by sector with the catch share fishery
have 100% observer coverage starting in 2011.  Given that, these data are treated as a 
census with estimates of discarding rates provided separately between catch share
and non-catch share data. Additionally, electronic monitoring became available for 
catch share vessels starting in 2015. Vessels using electronic monitoring have 
video coverage of on the water discarding rather than always having an human onboard
observer. These data are also considered to be a full representation of discarding.  

The catch share fishery includes the following sectors: catch share, catch share 
electronic monitoring, limited entry California halibut, midwater hake, midwater 
hake electronic monitoring, midwater rockfish, and midwater rockfish electronic monitoring.

## Data Products for Assessments

This package provides range of data products to support stock assessments.  
Generally, WCGOP data can be used to calculate discard rates, mean body weight of
discarded fish, and compositions of discarded fish. The files provided by this 
package are:

Description of data products:

- Confidentiality Checks:

  - confidentiality_catch_share.csv: The number of observed vessels, trips, hauls, and observations on non-catch and catch-share vessels of selected species given the request data stratification by area and gear type. To meet confidentiality a total of three or more vessels must be observed by stratification and year.

  - confidentiality_em_catch_share.csv: The number of observed vessels, trips, hauls, and observations on vessels using electronic monitoring (EM) of selected species given the request data stratification by area and gear type. To meet confidentiality a total of three or more vessels must be observed by stratification and year. The confidentiality check for EM vessels is separate from the previous confidentiality check since the EM data are in a separate data file given the unique nature of the data. The EM program began in 2015 and is limited to catch-share vessels with participation increasing in recent years. 
  
- Biological Data:

  - biological_discard_lengths.csv and biological_discard_ages.csv: The length/age observations of discarded fish formatted based on requested stratification, data bin structure, and sex. Only select species include sex-specific data within WCGOP. Data are formatted for SS3. 

  - discard_mean_body_weights.csv: The mean body weight in kg of discarded fish. Only years with 30 or more observations by stratification are provided. Data are formatted for SS3. 
  
- Discard Rates: The discards rates bewteen catch share and non-catch share vessels should be combined together to create a single discard rate by year and fleet for use within SS3.  How to combine these discard rates should be carefully considered by analysts depending upon the magnitude of total landings (not just observed landings) between catch share and non-catch share vessels. 

  - discard_rates_catch_share.csv: The observed discard totals and rates for catch-share vessels. The catch-share fishery has full observer coverage, hence data are considered a census and no measure of uncertainty are provided. 

  - discard_rates_em_catch_share.csv:  The observed discard totals and rates for catch-share vessels using EM. These data are considered a census and no measure of uncertainty are provided. 

  - discard_rates_combined_catch_share.csv: The combined discard totals between the catch-share vessels with onboard observers and  those using EM. Analysts should use this file when determining how to combine discard rates between catch-share and non-catch share vessels for use in stock assessment.

  - discard_rates_noncatch_share.csv: The observed discard totals and rates for non-catch share vessels. These data are bootstrapped to estimate median discard rates and standard deviation for use in assessment.





