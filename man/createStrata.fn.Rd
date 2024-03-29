% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CreateStrata.R
\name{createStrata.fn}
\alias{createStrata.fn}
\title{This function will classify and subset the data frame based on a single variable and a set of values or cut points.
It produces an additional column with the strata levels.
If the variable is numeric, it creates a strata name that indicates the range of values in that strata
Note that numeric cuts are always closed on the left and open on the right
For non-numeric variables, the specific quantities are subset.}
\usage{
createStrata.fn(
  dat,
  colnm,
  vars,
  strataNames = NULL,
  nmcol = paste0(colnm, "new"),
  subset = F
)
}
\arguments{
\item{dat}{the dataframe of observations}

\item{colnm}{unsure how this is different from colnms}

\item{vars}{the variables to subset by.
        If numeric & the variable is numeric (dat[,colnm]):
                    this must contain the lowest boundary and the upper boundary, and all divisions in between.
                    Anything less than the lowest boundary or greater than or equal to the largest boundary are given NA.
                    Note that anything EQUAL TO the largest boundary is given an NA

        if the variable to be stratified (dat[,colnm]) is a character or factor and the vars is numeric, character, or factor:
                   it simply subsets based on the specific values using the classifyV3.fxn

        if the variable to be stratified (dat[,colnm]) is numeric and the vars is not numeric:
                   A warnin gis issued that this is probably not the desired behavior (but could be, for example year, although I use yr.fxn)}

\item{nmcol}{name of additional column to add that indicates the values specified.  An NA will be put for values that are not in z.}

\item{subset}{whether or not to return a dataframe with only rows that meet criteria. Default value FALSE.}
}
\description{
This function will classify and subset the data frame based on a single variable and a set of values or cut points.
It produces an additional column with the strata levels.
If the variable is numeric, it creates a strata name that indicates the range of values in that strata
Note that numeric cuts are always closed on the left and open on the right
For non-numeric variables, the specific quantities are subset.
}
