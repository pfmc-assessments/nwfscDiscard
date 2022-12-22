## nwfscDiscard

Code developed to evaluate the confidentiality by strata and to provide bootsrap estimates of discard estimates for West Coast Groundfish Observer Program (WCGOP) data. Code maintained by @chantelwetzel-noaa.

Note: this code was developed for use by scientists at the Northwest Fisheries Science Center and is intended to work on the specific data products that we have access to using methods specific to the needs of this group.

## About the West Coast Groundfish Observer Program

WCGOP has been using on-vessel observers off the U.S. West Coast since 2002 to collect data on discarding practices. The on-vessel observer coverage varies by fishing sector. The Catch Share and At-sea Hake fisheries have observers on 100% of all fishing trips. The Electronic Monitoring (EM) program has 100% electronic monitoring and a target of 30% human observation for scientific data collection. All other observed fisheries have less than 100% observer coverage. [WCGOP](https://www.fisheries.noaa.gov/west-coast/fisheries-observers/west-coast-fishery-observer-bycatch-and-mortality-reports#fishery-management-reports) produces a number of annual reports providing information on observer coverage, groundfish mortality, etc. The 2022 [observer coverage report](https://repository.library.noaa.gov/view/noaa/47111) and the [annual groundfish mortality report](https://www.pcouncil.org/documents/2022/08/g-1-b-nwfsc-report-1-estimated-discard-and-catch-of-groundfish-species-in-the-2021-west-coast-fisheries.pdf/) can be found online.


## Installation

```S
install.packages("devtools")
library(devtools)

devtools::install_github("pfmc-assessments/nwfscDiscard")
```

