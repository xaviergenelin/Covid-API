Covid Vignette
================
Xavier Genelin
9/29/2021

-   [Packages](#packages)
-   [Functions](#functions)
-   [Data Exporation](#data-exporation)

## Packages

\-`tidyverse`  
-`httr`  
-`jsonlite`

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
    output <- as_tibble(dat$Global)
  } else if(type == "country"){
    output <- as_tibble(dat$Countries)
    
    output <- output %>% select("Country", "CountryCode", "Slug", "NewConfirmed", 
                                "TotalConfirmed", "NewDeaths", "TotalDeaths", "NewRecovered", "TotalRecovered", "Date")
  }
  return(output)
}
```

``` r
a <- covidSummary("country")
a
```

    ## # A tibble: 192 x 10
    ##    Country             CountryCode Slug                NewConfirmed TotalConfirmed NewDeaths TotalDeaths NewRecovered TotalRecovered Date              
    ##    <chr>               <chr>       <chr>                      <int>          <int>     <int>       <int>        <int>          <int> <chr>             
    ##  1 Afghanistan         AF          afghanistan                    0         155093         0        7201            0              0 2021-09-29T18:14:~
    ##  2 Albania             AL          albania                        0         168782         0        2668            0              0 2021-09-29T18:14:~
    ##  3 Algeria             DZ          algeria                        0         203045         0        5797            0              0 2021-09-29T18:14:~
    ##  4 Andorra             AD          andorra                        0          15192         0         130            0              0 2021-09-29T18:14:~
    ##  5 Angola              AO          angola                         0          56040         0        1526            0              0 2021-09-29T18:14:~
    ##  6 Antigua and Barbuda AG          antigua-and-barbuda            0           3160         0          76            0              0 2021-09-29T18:14:~
    ##  7 Argentina           AR          argentina                      0        5253765         0      115038            0              0 2021-09-29T18:14:~
    ##  8 Armenia             AM          armenia                        0         259779         0        5277            0              0 2021-09-29T18:14:~
    ##  9 Australia           AU          australia                   1812         102723        23        1279            0              0 2021-09-29T18:14:~
    ## 10 Austria             AT          austria                        0         738763         0       10986            0              0 2021-09-29T18:14:~
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

Need this because country filters need to be the slug, so the user can
choose anything but return the slug

``` r
getCountry <- function(country, IDType){
  # This will return a tibble with the country name, the country name in lowercase
  # with dashes instead of spaces (slug), and the country code of length 2
  # For example, if we want the United States:
  # country: United States of America, slug: united-states, id: US
  getAPI <- GET("https://api.covid19api.com/countries")
  dat <- fromJSON(rawToChar(getAPI$content))
  data <- as_tibble(dat)
  
  if(IDType == "country"){
    output <- data %>% filter(Country == country) %>% select(Slug)
  } else if(IDType == "id"){
    output <- data %>% filter(ISO2 == toupper(country)) %>% select(Slug)
  } else if(IDType == "slug"){
    output <- data %>% filter(slug == country) %>% select(Slug)
  } else {
    stop("ERROR: Invalid IDType. Should be country, id, or slug")
  }
  
  if(nrow(output) == 0){
    stop("ERROR: Country not found. Check spelling and possible capitalization")
  }
  return(output)
}
```

Testing to see if this will work as an error for the total function

``` r
getAPI <- GET("https://api.covid19api.com/total/country/united-states/status/death")
dat <- fromJSON(rawToChar(getAPI$content))
data <- as_tibble(dat)

data[1,1] == ""
```

    ##      Country
    ## [1,]    TRUE

``` r
total <- function(country, IDType, case = "all") {
  # get the total covid numbers for a country by day
  # user has the option to get all numbers or just confirmed, recovered, or deaths
  # Find an easy way to get the date in here along with the other functions
  # Date needs to be added in after the country and case portions
  if(case == "all"){
    main <- "https://api.covid19api.com/total/country/"
    country <- getCountry(country, IDType)
    url <- paste0(main, country)
    getAPI <- GET(url)
    dat <- fromJSON(rawToChar(getAPI$content))
    output <- as_tibble(dat)
    
  } else if(case != "all"){
    main <- "https://api.covid19api.com/total/country/"
    country <- getCountry(country, IDType)
    url <- paste0(main, country, "/status/", case)
    getAPI <- GET(url)
    dat <- fromJSON(rawToChar(getAPI$content))
    output <- as_tibble(dat)
  } 
  if(output[1,1] == ""){
    stop("ERROR: make sure case is either all, confirmed, recovered, or deaths")
  }
  return(output)
}

total(country= "AL", IDType = "id", case = "recovered")
```

    ## # A tibble: 616 x 10
    ##    Country CountryCode Province City  CityCode Lat   Lon   Cases Status    Date                
    ##    <chr>   <chr>       <chr>    <chr> <chr>    <chr> <chr> <int> <chr>     <chr>               
    ##  1 Albania ""          ""       ""    ""       0     0         0 recovered 2020-01-22T00:00:00Z
    ##  2 Albania ""          ""       ""    ""       0     0         0 recovered 2020-01-23T00:00:00Z
    ##  3 Albania ""          ""       ""    ""       0     0         0 recovered 2020-01-24T00:00:00Z
    ##  4 Albania ""          ""       ""    ""       0     0         0 recovered 2020-01-25T00:00:00Z
    ##  5 Albania ""          ""       ""    ""       0     0         0 recovered 2020-01-26T00:00:00Z
    ##  6 Albania ""          ""       ""    ""       0     0         0 recovered 2020-01-27T00:00:00Z
    ##  7 Albania ""          ""       ""    ""       0     0         0 recovered 2020-01-28T00:00:00Z
    ##  8 Albania ""          ""       ""    ""       0     0         0 recovered 2020-01-29T00:00:00Z
    ##  9 Albania ""          ""       ""    ""       0     0         0 recovered 2020-01-30T00:00:00Z
    ## 10 Albania ""          ""       ""    ""       0     0         0 recovered 2020-01-31T00:00:00Z
    ## # ... with 606 more rows

Live data for each country

``` r
live <- function(country, IDType, case = "all") {
  # get the total covid numbers for a country by day
  # user has the option to get all numbers or just confirmed, recovered, or deaths
  if(case == "all"){
    main <- "https://api.covid19api.com/live/country/"
    country <- getCountry(country, IDType)
    url <- paste0(main, country)
    getAPI <- GET(url)
    dat <- fromJSON(rawToChar(getAPI$content))
    output <- as_tibble(dat)
    
  } else if(case != "all"){
    main <- "https://api.covid19api.com/live/country/"
    country <- getCountry(country, IDType)
    url <- paste0(main, country, "/status/", case)
    getAPI <- GET(url)
    dat <- fromJSON(rawToChar(getAPI$content))
    output <- as_tibble(dat)
  } 
  if(output[1,1] == ""){
    stop("ERROR: make sure case is either all, confirmed, recovered, or deaths")
  }
  return(output)
}
```

``` r
live(country = "US", IDType = "id")
```

    ## # A tibble: 5,283 x 13
    ##    ID                                   Country      CountryCode Province       City  CityCode Lat    Lon   Confirmed Deaths Recovered Active Date     
    ##    <chr>                                <chr>        <chr>       <chr>          <chr> <chr>    <chr>  <chr>     <int>  <int>     <int>  <int> <chr>    
    ##  1 9cb646d4-fb86-4b74-8c16-df1f8d0bedaf United Stat~ US          United States~ ""    ""       18.35  -64.~         0      0         0      0 1970-01-~
    ##  2 f27bd486-5cf5-4424-ae99-45b123578efd United Stat~ US          American Samoa ""    ""       -14.27 -170~         0      0         0      0 1970-01-~
    ##  3 1b5a049b-59b6-474a-a4b3-290ff563da2b United Stat~ US          Diamond Princ~ ""    ""       35.44  139.~        49      0         0     49 2020-08-~
    ##  4 d3458b9a-8446-4760-89ab-7367195b6c8e United Stat~ US          Grand Princess ""    ""       37.65  -122~       103      3         0    100 2020-08-~
    ##  5 03329e68-7b08-413e-8a83-d7a5baa6537d United Stat~ US          Massachusetts  ""    ""       42.23  -71.~    709622  17984         0 691638 2021-06-~
    ##  6 0338b49e-4d88-4752-934f-6cc765df0734 United Stat~ US          South Carolina ""    ""       33.86  -80.~    596032   9805         0 586227 2021-06-~
    ##  7 0bfba11f-e3d4-4a6b-9f75-5ff59dced0c2 United Stat~ US          Utah           ""    ""       40.15  -111~    413008   2337         0 410671 2021-06-~
    ##  8 1aa77813-7bcf-45bb-8f9b-ff859b1ecdcd United Stat~ US          Oklahoma       ""    ""       35.57  -96.~    456686   7384         0 449302 2021-06-~
    ##  9 1cece2e5-7730-4b4c-8bd3-415cb3e58a5d United Stat~ US          Mississippi    ""    ""       32.74  -89.~    320594   7391         0 313203 2021-06-~
    ## 10 24ce2465-e676-41a5-9a5b-423a52654c70 United Stat~ US          Rhode Island   ""    ""       41.68  -71.~    152501   2727         0 149774 2021-06-~
    ## # ... with 5,273 more rows

Day one data for each country

``` r
dayOne <- function(country, IDType, case = "all") {
  # get the total covid numbers for a country by day
  # user has the option to get all numbers or just confirmed, recovered, or deaths
  if(case == "all"){
    main <- "https://api.covid19api.com/dayone/country/"
    country <- getCountry(country, IDType)
    url <- paste0(main, country)
    getAPI <- GET(url)
    dat <- fromJSON(rawToChar(getAPI$content))
    output <- as_tibble(dat)
    
  } else if(case != "all"){
    main <- "https://api.covid19api.com/dayone/country/"
    country <- getCountry(country, IDType)
    url <- paste0(main, country, "/status/", case)
    getAPI <- GET(url)
    dat <- fromJSON(rawToChar(getAPI$content))
    output <- as_tibble(dat)
  } 
  if(output[1,1] == ""){
    stop("ERROR: make sure case is either all, confirmed, recovered, or deaths")
  }
  return(output)
}
```

``` r
dayOne(country = "AL", IDType = "id")
```

    ## # A tibble: 570 x 13
    ##    ID                                   Country CountryCode Province City  CityCode Lat   Lon   Confirmed Deaths Recovered Active Date                
    ##    <chr>                                <chr>   <chr>       <chr>    <chr> <chr>    <chr> <chr>     <int>  <int>     <int>  <int> <chr>               
    ##  1 bfa83dd7-5d7b-4880-b1e8-6f089336ffdb Albania AL          ""       ""    ""       41.15 20.17         2      0         0      2 2020-03-09T00:00:00Z
    ##  2 ade7bbaa-f0ac-4863-b363-e9b315e20ab3 Albania AL          ""       ""    ""       41.15 20.17        10      0         0     10 2020-03-10T00:00:00Z
    ##  3 7302621c-6253-42da-96e7-45714eb464a6 Albania AL          ""       ""    ""       41.15 20.17        12      1         0     11 2020-03-11T00:00:00Z
    ##  4 d8f3cdc5-e73e-4c3e-91b2-776e904a6847 Albania AL          ""       ""    ""       41.15 20.17        23      1         0     22 2020-03-12T00:00:00Z
    ##  5 6fe7f707-c5a1-48d3-9d78-f4cf0364046d Albania AL          ""       ""    ""       41.15 20.17        33      1         0     32 2020-03-13T00:00:00Z
    ##  6 f44979d5-9afb-417b-8898-8d59265607a8 Albania AL          ""       ""    ""       41.15 20.17        38      1         0     37 2020-03-14T00:00:00Z
    ##  7 76a4e0ea-1f31-4428-8f3b-61f33c3e34fd Albania AL          ""       ""    ""       41.15 20.17        42      1         0     41 2020-03-15T00:00:00Z
    ##  8 bb32762a-c746-4fe1-8bdd-2c2597257a6e Albania AL          ""       ""    ""       41.15 20.17        51      1         0     50 2020-03-16T00:00:00Z
    ##  9 7aa941cd-db2b-4e37-8b09-801a4aefd6da Albania AL          ""       ""    ""       41.15 20.17        55      1         0     54 2020-03-17T00:00:00Z
    ## 10 e27c60ed-b0a7-410b-b105-7c995d717a93 Albania AL          ""       ""    ""       41.15 20.17        59      2         0     57 2020-03-18T00:00:00Z
    ## # ... with 560 more rows

Create one final function that uses the other functions above to collect
whatever data is needed

``` r
covid <- function(func, ...){
  if(func == "total"){
    output <- total(...)
  } else if(func == "live"){
    output <- live(...)
  } else if(func == "dayOne"){
    output <- dayOne(...)
  } else {
    stop("ERROR: choose total, live, or dayOne as function")
  }
}
```

## Data Exporation
