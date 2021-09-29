Covid Vignette
================
Xavier Genelin
9/29/2021

-   [Packages](#packages)
-   [Functions](#functions)
-   [Data Exporation](#data-exporation)

## Packages

`tidyverse` `httr` `jsonlite`

``` r
library(tidyverse)
library(httr)
library(jsonlite)
```

## Functions

This will be used in each of the functions to get the data from the API.
Just an example for easy copying

``` r
getAPI <- GET("https://api.covid19api.com/all")

dat <- fromJSON(rawToChar(getAPI$content))

as_tibble(dat)
```

    ## # A tibble: 1 x 1
    ##   message  
    ##   <chr>    
    ## 1 Not Found

``` r
# The user can either choose a global summary or country

covidSummary <- function(type = "global"){
  ### you can either select global or country to see the global summary or 
  # the summary by country
  getAPI <- GET("https://api.covid19api.com/summary")
  dat <- fromJSON(rawToChar(getAPI$content))
  
  if(type == "global"){
    output <- as_tiblle(dat$Global)
  } else if(type == "country"){
    output <- as_tibble(dat$Countries)
  }
  return(output)
}
```

``` r
covidSummary("country")
```

    ## # A tibble: 192 x 12
    ##    ID               Country     CountryCode Slug      NewConfirmed TotalConfirmed NewDeaths TotalDeaths NewRecovered TotalRecovered Date        Premium
    ##    <chr>            <chr>       <chr>       <chr>            <int>          <int>     <int>       <int>        <int>          <int> <chr>       <df[,0>
    ##  1 359e7714-650f-4~ Afghanistan AF          afghanis~            0         155093         0        7201            0              0 2021-09-29~        
    ##  2 00cadcfc-1030-4~ Albania     AL          albania              0         168782         0        2668            0              0 2021-09-29~        
    ##  3 07687a81-833b-4~ Algeria     DZ          algeria              0         203045         0        5797            0              0 2021-09-29~        
    ##  4 3eb67117-7e72-4~ Andorra     AD          andorra              0          15192         0         130            0              0 2021-09-29~        
    ##  5 05bfd8ca-8d5b-4~ Angola      AO          angola               0          56040         0        1526            0              0 2021-09-29~        
    ##  6 3eab9249-a585-4~ Antigua an~ AG          antigua-~            0           3160         0          76            0              0 2021-09-29~        
    ##  7 23f2374e-e618-4~ Argentina   AR          argentina            0        5253765         0      115038            0              0 2021-09-29~        
    ##  8 7440a545-c508-4~ Armenia     AM          armenia              0         259779         0        5277            0              0 2021-09-29~        
    ##  9 ddde5515-8e93-4~ Australia   AU          australia         1812         102723        23        1279            0              0 2021-09-29~        
    ## 10 740c5e7d-adae-4~ Austria     AT          austria              0         738763         0       10986            0              0 2021-09-29~        
    ## # ... with 182 more rows

``` r
getAPI <- GET("https://api.covid19api.com/countries")

dat <- fromJSON(rawToChar(getAPI$content))

as_tibble(dat)
```

    ## # A tibble: 248 x 3
    ##    Country                     Slug                        ISO2 
    ##    <chr>                       <chr>                       <chr>
    ##  1 Montenegro                  montenegro                  ME   
    ##  2 Spain                       spain                       ES   
    ##  3 Colombia                    colombia                    CO   
    ##  4 French Southern Territories french-southern-territories TF   
    ##  5 American Samoa              american-samoa              AS   
    ##  6 Congo (Kinshasa)            congo-kinshasa              CD   
    ##  7 Mayotte                     mayotte                     YT   
    ##  8 Australia                   australia                   AU   
    ##  9 Brazil                      brazil                      BR   
    ## 10 El Salvador                 el-salvador                 SV   
    ## # ... with 238 more rows

Not sure if this will be necessary

``` r
getCountry <- function(country, IDType){
  # This will return a tibble with the country name, the country name in lowercase
  # with dashes instead of spaces, and the country code
  getAPI <- GET("https://api.covid19api.com/countries")
  dat <- fromJSON(rawToChar(getAPI$content))
  data <- as_tibble(dat)
  
  if(IDType == "country"){
    output <- data
  }
}
```

## Data Exporation
