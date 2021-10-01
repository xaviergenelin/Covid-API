Covid Vignette
================
Xavier Genelin
9/30/2021

-   [Packages](#packages)
-   [Functions](#functions)
    -   [`covidSummary`](#covidsummary)
    -   [`getCountry`](#getcountry)
    -   [`live`](#live)
-   [Data Exporation](#data-exporation)

############## Current idea of what to do with this

Rework the live, total, and oneDay functions to allow the user to select
a date range, province, and city/city code. Probably just the city
because the code was a little odd. Not sure how it’s choosing that. I
should be able to print the list of options from the choices by using
`toString(sort(unique(data$City)))`. Maybe make a function for that too,
idk.

Maybe having a function for live, total and oneday isn’t the way to go?

The current issue is some of the urls don’t work in the same order by
just adding on things at the end. I’ll have to go into the API site to
see which one should be used for what the user might filter on.

After that the visuals/EDA should be it for creating the vignette and
then move on to the blog and stuff.

This document is a vignette that’ll show how we can retrieve data from
an API and perform exploratory analysis. This will use the [Covid
API](https://covid19api.com/). We will build our own functions that will
collect data from the API and allow a user to interact with it by
choosing different endpoints they can retrieve.

# Packages

To be able to interact with the API and manipulate the data we want to
return, the following packages will need to be installed and used:

-   `tidyverse`: this package will allow us to manipulate and visualize
    the data  
-   `httr`: gives us a response object from the API url  
-   `jsonlite`: allows us to interact with the API  
-   `countrycode`: this lets us tie the country name to the continent
    which is defined by the World Bank Development Indicators. The
    Republic of Kosovo is the only one that seems to be missing the
    continent. This is located in Europe and will be added

``` r
library(tidyverse)
library(httr)
library(jsonlite)
library(countrycode)
```

urls for each type to refer back to

``` r
#day one: "https://api.covid19api.com/dayone/country/south-africa"
#by country: "https://api.covid19api.com/country/south-africa"
#
#
#
#
#
#
#
#
#
#
#
cities <- c("Wake", "Durham", "Johnston")

paste("Please select: ", paste(cities, collapse = ","), sep = " ")
```

    ## [1] "Please select:  Wake,Durham,Johnston"

``` r
# Day one all status
getAPI <- GET("https://api.covid19api.com/dayone/country/south-africa/status/confirmed/live")

dat <- fromJSON(rawToChar(getAPI$content))

test1 <- as_tibble(dat)
test1
```

    ## # A tibble: 576 x 10
    ##    Country      CountryCode Province City  CityCode Lat    Lon   Cases Status    Date                
    ##    <chr>        <chr>       <chr>    <chr> <chr>    <chr>  <chr> <int> <chr>     <chr>               
    ##  1 South Africa ZA          ""       ""    ""       -30.56 22.94     1 confirmed 2020-03-05T00:00:00Z
    ##  2 South Africa ZA          ""       ""    ""       -30.56 22.94     1 confirmed 2020-03-06T00:00:00Z
    ##  3 South Africa ZA          ""       ""    ""       -30.56 22.94     1 confirmed 2020-03-07T00:00:00Z
    ##  4 South Africa ZA          ""       ""    ""       -30.56 22.94     3 confirmed 2020-03-08T00:00:00Z
    ##  5 South Africa ZA          ""       ""    ""       -30.56 22.94     3 confirmed 2020-03-09T00:00:00Z
    ##  6 South Africa ZA          ""       ""    ""       -30.56 22.94     7 confirmed 2020-03-10T00:00:00Z
    ##  7 South Africa ZA          ""       ""    ""       -30.56 22.94    13 confirmed 2020-03-11T00:00:00Z
    ##  8 South Africa ZA          ""       ""    ""       -30.56 22.94    17 confirmed 2020-03-12T00:00:00Z
    ##  9 South Africa ZA          ""       ""    ""       -30.56 22.94    24 confirmed 2020-03-13T00:00:00Z
    ## 10 South Africa ZA          ""       ""    ""       -30.56 22.94    38 confirmed 2020-03-14T00:00:00Z
    ## # ... with 566 more rows

``` r
test1 %>% arrange(desc(Date))
```

    ## # A tibble: 576 x 10
    ##    Country      CountryCode Province City  CityCode Lat    Lon     Cases Status    Date                
    ##    <chr>        <chr>       <chr>    <chr> <chr>    <chr>  <chr>   <int> <chr>     <chr>               
    ##  1 South Africa ZA          ""       ""    ""       -30.56 22.94 2902672 confirmed 2021-10-01T00:00:00Z
    ##  2 South Africa ZA          ""       ""    ""       -30.56 22.94 2902672 confirmed 2021-09-30T00:00:00Z
    ##  3 South Africa ZA          ""       ""    ""       -30.56 22.94 2898888 confirmed 2021-09-29T00:00:00Z
    ##  4 South Africa ZA          ""       ""    ""       -30.56 22.94 2898888 confirmed 2021-09-28T00:00:00Z
    ##  5 South Africa ZA          ""       ""    ""       -30.56 22.94 2897521 confirmed 2021-09-27T00:00:00Z
    ##  6 South Africa ZA          ""       ""    ""       -30.56 22.94 2896943 confirmed 2021-09-26T00:00:00Z
    ##  7 South Africa ZA          ""       ""    ""       -30.56 22.94 2895976 confirmed 2021-09-25T00:00:00Z
    ##  8 South Africa ZA          ""       ""    ""       -30.56 22.94 2894342 confirmed 2021-09-24T00:00:00Z
    ##  9 South Africa ZA          ""       ""    ""       -30.56 22.94 2892081 confirmed 2021-09-23T00:00:00Z
    ## 10 South Africa ZA          ""       ""    ""       -30.56 22.94 2889298 confirmed 2021-09-22T00:00:00Z
    ## # ... with 566 more rows

``` r
test1$continent <- countrycode(test1$Country, origin = "country.name", destination = "continent")
```

``` r
# day one live 
getAPI <- GET("https://api.covid19api.com/dayone/country/south-africa/status/deaths/live")

dat <- fromJSON(rawToChar(getAPI$content))

test2 <- as_tibble(dat)
test2 %>% arrange(desc(Date))
```

    ## # A tibble: 576 x 10
    ##    Country      CountryCode Province City  CityCode Lat    Lon   Cases Status Date                
    ##    <chr>        <chr>       <chr>    <chr> <chr>    <chr>  <chr> <int> <chr>  <chr>               
    ##  1 South Africa ZA          ""       ""    ""       -30.56 22.94 87626 deaths 2021-10-01T00:00:00Z
    ##  2 South Africa ZA          ""       ""    ""       -30.56 22.94 87626 deaths 2021-09-30T00:00:00Z
    ##  3 South Africa ZA          ""       ""    ""       -30.56 22.94 87417 deaths 2021-09-29T00:00:00Z
    ##  4 South Africa ZA          ""       ""    ""       -30.56 22.94 87417 deaths 2021-09-28T00:00:00Z
    ##  5 South Africa ZA          ""       ""    ""       -30.56 22.94 87216 deaths 2021-09-27T00:00:00Z
    ##  6 South Africa ZA          ""       ""    ""       -30.56 22.94 87052 deaths 2021-09-26T00:00:00Z
    ##  7 South Africa ZA          ""       ""    ""       -30.56 22.94 87001 deaths 2021-09-25T00:00:00Z
    ##  8 South Africa ZA          ""       ""    ""       -30.56 22.94 86967 deaths 2021-09-24T00:00:00Z
    ##  9 South Africa ZA          ""       ""    ""       -30.56 22.94 86655 deaths 2021-09-23T00:00:00Z
    ## 10 South Africa ZA          ""       ""    ""       -30.56 22.94 86500 deaths 2021-09-22T00:00:00Z
    ## # ... with 566 more rows

``` r
test2
```

    ## # A tibble: 576 x 10
    ##    Country      CountryCode Province City  CityCode Lat    Lon   Cases Status Date                
    ##    <chr>        <chr>       <chr>    <chr> <chr>    <chr>  <chr> <int> <chr>  <chr>               
    ##  1 South Africa ZA          ""       ""    ""       -30.56 22.94     0 deaths 2020-03-05T00:00:00Z
    ##  2 South Africa ZA          ""       ""    ""       -30.56 22.94     0 deaths 2020-03-06T00:00:00Z
    ##  3 South Africa ZA          ""       ""    ""       -30.56 22.94     0 deaths 2020-03-07T00:00:00Z
    ##  4 South Africa ZA          ""       ""    ""       -30.56 22.94     0 deaths 2020-03-08T00:00:00Z
    ##  5 South Africa ZA          ""       ""    ""       -30.56 22.94     0 deaths 2020-03-09T00:00:00Z
    ##  6 South Africa ZA          ""       ""    ""       -30.56 22.94     0 deaths 2020-03-10T00:00:00Z
    ##  7 South Africa ZA          ""       ""    ""       -30.56 22.94     0 deaths 2020-03-11T00:00:00Z
    ##  8 South Africa ZA          ""       ""    ""       -30.56 22.94     0 deaths 2020-03-12T00:00:00Z
    ##  9 South Africa ZA          ""       ""    ""       -30.56 22.94     0 deaths 2020-03-13T00:00:00Z
    ## 10 South Africa ZA          ""       ""    ""       -30.56 22.94     0 deaths 2020-03-14T00:00:00Z
    ## # ... with 566 more rows

# Functions

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
covidSummary("global")
```

    ## # A tibble: 1 x 8
    ##   NewConfirmed TotalConfirmed NewDeaths TotalDeaths NewRecovered TotalRecovered Date       percentDeath
    ##          <int>          <int>     <int>       <int>        <int>          <int> <date>            <dbl>
    ## 1       289050      233403939      5901     4779803            0              0 2021-10-01         2.05

## `covidSummary`

This first function gets access to the covid summary data and allows the
user to choose either a global or country summary. The global summary
will return one row with data on NewConfirmed, TotalConfirmed,
NewDeaths, TotalDeaths, NewRecovered, and TotalRecovered along with the
date that this data is from. The country data will show the same data
for 192 countries as well as their country name, country code, and slug.
We’re also adding in the continent based on the country. If the user
writes in a continent while trying to access the global information, it
will still return the global information. For both the global and
country summaries, this function will create a new variable that
calculates the total percent that have died from covid.

``` r
# The user can either choose a global summary or country

covidSummary <- function(type, continent = "all"){
  ### you can either select global or country to see the global summary or 
  # the summary by country
  getAPI <- GET("https://api.covid19api.com/summary")
  dat <- fromJSON(rawToChar(getAPI$content))
  
  # This lets the user type in capital letters for the type and continent without erroring it out
  type <- tolower(type)

  
  if(type == "global"){
    output <- as_tibble(dat$Global)
    
    output <- output %>% mutate(percentDeath = round(TotalDeaths / TotalConfirmed * 100, 2), Date = as.Date(Date))
    
  } else if(type == "country"){
    output <- as_tibble(dat$Countries)
    
    output <- output %>% select("Country", "CountryCode", "Slug", "NewConfirmed", 
                                "TotalConfirmed", "NewDeaths", "TotalDeaths", "NewRecovered", "TotalRecovered", "Date")
    
    output$Continent <- countrycode(output$Country, origin = "country.name", destination = "continent")
    
    output$Continent[is.na(output$Continent)] <- "Europe"
    continents <- unique(output$Continent)
    
    output <- output %>% select("Country", "NewConfirmed", "TotalConfirmed", "NewDeaths", "TotalDeaths", "NewRecovered", "TotalRecovered", "Date", "Continent") %>% mutate(percentDeath = round(TotalDeaths / TotalConfirmed * 100, 2), Date = as.Date(Date))
    
      if(continent != "all"){
        continent <- str_to_title(continent)
        output <- output %>% filter(Continent == continent)
      
        if(nrow(output) == 0){
          # If the user selects a continent that isn't in the data set, it provides a list to choose from
          stop(paste("Please select one of the following continents: ", paste(continents, collapse = ","), sep = " "))
        }
      }
  }
  


  return(output)
}
```

``` r
covidSummary("country")
```

    ## # A tibble: 192 x 10
    ##    Country             NewConfirmed TotalConfirmed NewDeaths TotalDeaths NewRecovered TotalRecovered Date       Continent percentDeath
    ##    <chr>                      <int>          <int>     <int>       <int>        <int>          <int> <date>     <chr>            <dbl>
    ##  1 Afghanistan                    0         155174         0        7204            0              0 2021-10-01 Asia              4.64
    ##  2 Albania                        0         170131         0        2698            0              0 2021-10-01 Europe            1.59
    ##  3 Algeria                        0         203359         0        5812            0              0 2021-10-01 Africa            2.86
    ##  4 Andorra                        0          15222         0         130            0              0 2021-10-01 Europe            0.85
    ##  5 Angola                         0          56583         0        1537            0              0 2021-10-01 Africa            2.72
    ##  6 Antigua and Barbuda            0           3231         0          79            0              0 2021-10-01 Americas          2.45
    ##  7 Argentina                      0        5256902         0      115179            0              0 2021-10-01 Americas          2.19
    ##  8 Armenia                        0         261697         0        5319            0              0 2021-10-01 Asia              2.03
    ##  9 Australia                   2058         107181        20        1311            0              0 2021-10-01 Oceania           1.22
    ## 10 Austria                        0         743095         0       11009            0              0 2021-10-01 Europe            1.48
    ## # ... with 182 more rows

``` r
getAPI <- GET("https://api.covid19api.com/countries")

dat <- fromJSON(rawToChar(getAPI$content))

as_tibble(dat)
```

    ## # A tibble: 248 x 3
    ##    Country                      Slug                      ISO2 
    ##    <chr>                        <chr>                     <chr>
    ##  1 Tanzania, United Republic of tanzania                  TZ   
    ##  2 Gambia                       gambia                    GM   
    ##  3 Italy                        italy                     IT   
    ##  4 Macao, SAR China             macao-sar-china           MO   
    ##  5 Saint Pierre and Miquelon    saint-pierre-and-miquelon PM   
    ##  6 South Africa                 south-africa              ZA   
    ##  7 Zimbabwe                     zimbabwe                  ZW   
    ##  8 Isle of Man                  isle-of-man               IM   
    ##  9 Luxembourg                   luxembourg                LU   
    ## 10 Lao PDR                      lao-pdr                   LA   
    ## # ... with 238 more rows

## `getCountry`

The next function will allow the user to either type out the country
name, country code, or slug it will return the slug value. This is
necessary because country filters need to be the slug, so the user can
choose anything and not have later functions error out. So for example,
if someone wanted to get data on the United States, they would either
have to type in “United States of America”, “united-states”, “US”, or
“us”.

``` r
getCountry <- function(country, IDType){
  # This will return a tibble with the country name, the country name in lowercase
  # with dashes instead of spaces (slug), and the country code of length 2
  # For example, if we want the United States:
  # country: United States of America, slug: united-states, id: US
  getAPI <- GET("https://api.covid19api.com/countries")
  dat <- fromJSON(rawToChar(getAPI$content))
  data <- as_tibble(dat)
  
  # This lets the user type in with capital letters but won't error out the function
  IDtype <- tolower(IDType)
  
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

It works for this one!!!!

``` r
getAPI <- GET("https://api.covid19api.com/country/united-states/status/confirmed/live?province=ohio")
dat <- fromJSON(rawToChar(getAPI$content))
data <- as_tibble(dat)

data %>% arrange(City)
```

    ## # A tibble: 28,170 x 10
    ##    Country                  CountryCode Province City  CityCode Lat   Lon    Cases Status    Date                
    ##    <chr>                    <chr>       <chr>    <chr> <chr>    <chr> <chr>  <int> <chr>     <chr>               
    ##  1 United States of America US          Ohio     Adams 39001    38.85 -83.47   706 confirmed 2020-11-22T00:00:00Z
    ##  2 United States of America US          Ohio     Adams 39001    38.85 -83.47   750 confirmed 2020-11-23T00:00:00Z
    ##  3 United States of America US          Ohio     Adams 39001    38.85 -83.47   783 confirmed 2020-11-24T00:00:00Z
    ##  4 United States of America US          Ohio     Adams 39001    38.85 -83.47   803 confirmed 2020-11-25T00:00:00Z
    ##  5 United States of America US          Ohio     Adams 39001    38.85 -83.47   803 confirmed 2020-11-26T00:00:00Z
    ##  6 United States of America US          Ohio     Adams 39001    38.85 -83.47   838 confirmed 2020-11-27T00:00:00Z
    ##  7 United States of America US          Ohio     Adams 39001    38.85 -83.47   847 confirmed 2020-11-28T00:00:00Z
    ##  8 United States of America US          Ohio     Adams 39001    38.85 -83.47   869 confirmed 2020-11-29T00:00:00Z
    ##  9 United States of America US          Ohio     Adams 39001    38.85 -83.47   877 confirmed 2020-11-30T00:00:00Z
    ## 10 United States of America US          Ohio     Adams 39001    38.85 -83.47   892 confirmed 2020-12-01T00:00:00Z
    ## # ... with 28,160 more rows

``` r
# Dane county in Wisconsin
getAPI <- GET("https://api.covid19api.com/country/united-states?province=wisconsin")
dat <- fromJSON(rawToChar(getAPI$content))
data <- as_tibble(dat)

data %>% filter(City == "Dane")
```

    ## # A tibble: 313 x 13
    ##    ID                                   Country         CountryCode Province  City  CityCode Lat   Lon   Confirmed Deaths Recovered Active Date        
    ##    <chr>                                <chr>           <chr>       <chr>     <chr> <chr>    <chr> <chr>     <int>  <int>     <int>  <int> <chr>       
    ##  1 177b98c7-a92e-4ce8-8d98-fb895025f6f0 United States ~ US          Wisconsin Dane  55025    43.07 -89.~     25554     78         0  25476 2020-11-22T~
    ##  2 a04421f7-9f48-47ff-b79d-dd1c62b89046 United States ~ US          Wisconsin Dane  55025    43.07 -89.~     25722     78         0  25644 2020-11-23T~
    ##  3 b473d124-e059-4885-9b61-494652b54349 United States ~ US          Wisconsin Dane  55025    43.07 -89.~     26485     80         0  26405 2020-11-24T~
    ##  4 0980a19e-bc65-4633-8f9f-ec8d14ff0871 United States ~ US          Wisconsin Dane  55025    43.07 -89.~     26727     80         0  26647 2020-11-25T~
    ##  5 10de94d3-6982-487d-816d-30b9e437566a United States ~ US          Wisconsin Dane  55025    43.07 -89.~     27120     83         0  27037 2020-11-26T~
    ##  6 e07bcc4a-c7ee-427d-aec4-3028c5b2f418 United States ~ US          Wisconsin Dane  55025    43.07 -89.~     27164     83         0  27081 2020-11-27T~
    ##  7 3fd160f3-348f-4f3a-8814-9303cd481c49 United States ~ US          Wisconsin Dane  55025    43.07 -89.~     27619     83         0  27536 2020-11-28T~
    ##  8 cf596f72-7105-4651-9b4a-3763ca6f9ec1 United States ~ US          Wisconsin Dane  55025    43.07 -89.~     28017     83         0  27934 2020-11-29T~
    ##  9 8188f23a-54a3-4b59-99e1-8bab94af7d0e United States ~ US          Wisconsin Dane  55025    43.07 -89.~     28265     83         0  28182 2020-11-30T~
    ## 10 329ad997-9fc6-46c4-8d6f-2a6abfc81427 United States ~ US          Wisconsin Dane  55025    43.07 -89.~     28370     84         0  28286 2020-12-01T~
    ## # ... with 303 more rows

``` r
#getAPI <- GET("https://api.covid19api.com/dayone/country/united-states?province=wisconsin")
#dat <- fromJSON(rawToChar(getAPI$content))
#data <- as_tibble(dat)
#data
county <- toString(sort(unique(data$City)))
print(paste("Please select one of the following:", county))
```

    ## [1] "Please select one of the following: , Adams, Ashland, Barron, Bayfield, Brown, Buffalo, Burnett, Calumet, Chippewa, Clark, Columbia, Crawford, Dane, Dodge, Door, Douglas, Dunn, Eau Claire, Florence, Fond du Lac, Forest, Grant, Green, Green Lake, Iowa, Iron, Jackson, Jefferson, Juneau, Kenosha, Kewaunee, La Crosse, Lafayette, Langlade, Lincoln, Manitowoc, Marathon, Marinette, Marquette, Menominee, Milwaukee, Monroe, Oconto, Oneida, Out of WI, Outagamie, Ozaukee, Pepin, Pierce, Polk, Portage, Price, Racine, Richland, Rock, Rusk, Sauk, Sawyer, Shawano, Sheboygan, St. Croix, Taylor, Trempealeau, Unassigned, Vernon, Vilas, Walworth, Washburn, Washington, Waukesha, Waupaca, Waushara, Winnebago, Wood"

``` r
print(county)
```

    ## [1] ", Adams, Ashland, Barron, Bayfield, Brown, Buffalo, Burnett, Calumet, Chippewa, Clark, Columbia, Crawford, Dane, Dodge, Door, Douglas, Dunn, Eau Claire, Florence, Fond du Lac, Forest, Grant, Green, Green Lake, Iowa, Iron, Jackson, Jefferson, Juneau, Kenosha, Kewaunee, La Crosse, Lafayette, Langlade, Lincoln, Manitowoc, Marathon, Marinette, Marquette, Menominee, Milwaukee, Monroe, Oconto, Oneida, Out of WI, Outagamie, Ozaukee, Pepin, Pierce, Polk, Portage, Price, Racine, Richland, Rock, Rusk, Sauk, Sawyer, Shawano, Sheboygan, St. Croix, Taylor, Trempealeau, Unassigned, Vernon, Vilas, Walworth, Washburn, Washington, Waukesha, Waupaca, Waushara, Winnebago, Wood"

``` r
teststring <- toString(county)
teststring
```

    ## [1] ", Adams, Ashland, Barron, Bayfield, Brown, Buffalo, Burnett, Calumet, Chippewa, Clark, Columbia, Crawford, Dane, Dodge, Door, Douglas, Dunn, Eau Claire, Florence, Fond du Lac, Forest, Grant, Green, Green Lake, Iowa, Iron, Jackson, Jefferson, Juneau, Kenosha, Kewaunee, La Crosse, Lafayette, Langlade, Lincoln, Manitowoc, Marathon, Marinette, Marquette, Menominee, Milwaukee, Monroe, Oconto, Oneida, Out of WI, Outagamie, Ozaukee, Pepin, Pierce, Polk, Portage, Price, Racine, Richland, Rock, Rusk, Sauk, Sawyer, Shawano, Sheboygan, St. Croix, Taylor, Trempealeau, Unassigned, Vernon, Vilas, Walworth, Washburn, Washington, Waukesha, Waupaca, Waushara, Winnebago, Wood"

The `total` function collects data from the API

########### add in a time filter to this

``` r
total <- function(country, IDType, status = "all") {
  # get the total covid numbers for a country by day
  # user has the option to get all numbers or just confirmed, recovered, or deaths
  # Find an easy way to get the date in here along with the other functions
  # Date needs to be added in after the country and case portions
  status <- tolower(status)
  
  if(status == "all"){
    main <- "https://api.covid19api.com/total/country/"
    country <- getCountry(country, IDType)
    url <- paste0(main, country)
    getAPI <- GET(url)
    dat <- fromJSON(rawToChar(getAPI$content))
    output <- as_tibble(dat)
    
    output <- output %>% mutate(Date = as.Date(Date))
    
  } else if(status != "all"){
    main <- "https://api.covid19api.com/total/country/"
    country <- getCountry(country, IDType)
    url <- paste0(main, country, "/status/", status)
    getAPI <- GET(url)
    dat <- fromJSON(rawToChar(getAPI$content))
    output <- as_tibble(dat)
    
    output <- output %>% mutate(Date = as.Date(Date))
  } 
  if(output[1,1] == ""){
    stop("ERROR: make sure case is either all, confirmed, recovered, or deaths")
  }
  return(output)
}

total(country= "US", IDType = "id", status = "Deaths")
```

    ## # A tibble: 618 x 10
    ##    Country                  CountryCode Province City  CityCode Lat   Lon   Cases Status Date      
    ##    <chr>                    <chr>       <chr>    <chr> <chr>    <chr> <chr> <int> <chr>  <date>    
    ##  1 United States of America ""          ""       ""    ""       0     0         0 deaths 2020-01-22
    ##  2 United States of America ""          ""       ""    ""       0     0         0 deaths 2020-01-23
    ##  3 United States of America ""          ""       ""    ""       0     0         0 deaths 2020-01-24
    ##  4 United States of America ""          ""       ""    ""       0     0         0 deaths 2020-01-25
    ##  5 United States of America ""          ""       ""    ""       0     0         0 deaths 2020-01-26
    ##  6 United States of America ""          ""       ""    ""       0     0         0 deaths 2020-01-27
    ##  7 United States of America ""          ""       ""    ""       0     0         0 deaths 2020-01-28
    ##  8 United States of America ""          ""       ""    ""       0     0         0 deaths 2020-01-29
    ##  9 United States of America ""          ""       ""    ""       0     0         0 deaths 2020-01-30
    ## 10 United States of America ""          ""       ""    ""       0     0         0 deaths 2020-01-31
    ## # ... with 608 more rows

## `live`

Live data for each country

``` r
live <- function(country, IDType, case = "all", province = "all") {
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
  # the country will be missing but it will still have a dataframe of more than 1 row if there's something wrong with the output
  # this will check to see if that error happened to let the user know
  if(output[1,1] == ""){
    stop("ERROR: make sure case is either all, confirmed, recovered, or deaths")
  }
  
  if(province != "all"){
    output <- output %>% filter(Province == province)
  }
  
  return(output)
}
```

``` r
live(country = "US", IDType = "id", case = " all", province = "Wisconsin")
```

    ## # A tibble: 98 x 13
    ##    ID                                   Country         CountryCode Province  City  CityCode Lat   Lon   Confirmed Deaths Recovered Active Date        
    ##    <chr>                                <chr>           <chr>       <chr>     <chr> <chr>    <chr> <chr>     <int>  <int>     <int>  <int> <chr>       
    ##  1 4352eb34-ce89-4e84-8286-1880014145b3 United States ~ US          Wisconsin ""    ""       44.27 -89.~    677186   8092         0 669094 2021-06-25T~
    ##  2 0d523717-5dc2-40be-8d01-2684643f1366 United States ~ US          Wisconsin ""    ""       44.27 -89.~    677252   8099         0 669153 2021-06-26T~
    ##  3 1e202581-df3d-4679-8908-3b05857f1f7c United States ~ US          Wisconsin ""    ""       44.27 -89.~    677252   8099         0 669153 2021-06-27T~
    ##  4 66e6c157-a414-4b1a-ba04-58d1a728d336 United States ~ US          Wisconsin ""    ""       44.27 -89.~    677252   8099         0 669153 2021-06-28T~
    ##  5 5a305668-b614-43b0-83dd-c819cd44bffb United States ~ US          Wisconsin ""    ""       44.27 -89.~    677396   8109         0 669287 2021-06-29T~
    ##  6 718ff5e6-88c0-4303-b771-d9d94490de6c United States ~ US          Wisconsin ""    ""       44.27 -89.~    677531   8126         0 669405 2021-06-30T~
    ##  7 c6f53679-a158-4b23-a04c-93f1b0d8091e United States ~ US          Wisconsin ""    ""       44.27 -89.~    677622   8128         0 669494 2021-07-01T~
    ##  8 78451df8-859a-4607-af75-984423951de8 United States ~ US          Wisconsin ""    ""       44.27 -89.~    677740   8134         0 669606 2021-07-02T~
    ##  9 19ffbac7-0366-4ae2-85d5-e012e7dfc8fe United States ~ US          Wisconsin ""    ""       44.27 -89.~    677859   8135         0 669724 2021-07-03T~
    ## 10 d9231cc8-56e8-4d1f-be54-b19f1ebcedba United States ~ US          Wisconsin ""    ""       44.27 -89.~    677859   8135         0 669724 2021-07-04T~
    ## # ... with 88 more rows

Day one data for each country

``` r
dayOne <- function(country, IDType, case = "all", province = "all") {
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
  
  if(province != "all"){
    output <- output %>% filter(Province == province)
  }
  
  return(output)
}
```

``` r
dayOne(country = "AU", IDType = "id", case = "deaths")
```

    ## # A tibble: 4,744 x 10
    ##    Country   CountryCode Province        City  CityCode Lat    Lon    Cases Status Date                
    ##    <chr>     <chr>       <chr>           <chr> <chr>    <chr>  <chr>  <int> <chr>  <chr>               
    ##  1 Australia AU          New South Wales ""    ""       -33.87 151.21     0 deaths 2020-01-26T00:00:00Z
    ##  2 Australia AU          Victoria        ""    ""       -37.81 144.96     0 deaths 2020-01-26T00:00:00Z
    ##  3 Australia AU          New South Wales ""    ""       -33.87 151.21     0 deaths 2020-01-27T00:00:00Z
    ##  4 Australia AU          Victoria        ""    ""       -37.81 144.96     0 deaths 2020-01-27T00:00:00Z
    ##  5 Australia AU          New South Wales ""    ""       -33.87 151.21     0 deaths 2020-01-28T00:00:00Z
    ##  6 Australia AU          Victoria        ""    ""       -37.81 144.96     0 deaths 2020-01-28T00:00:00Z
    ##  7 Australia AU          Victoria        ""    ""       -37.81 144.96     0 deaths 2020-01-29T00:00:00Z
    ##  8 Australia AU          New South Wales ""    ""       -33.87 151.21     0 deaths 2020-01-29T00:00:00Z
    ##  9 Australia AU          Queensland      ""    ""       -27.47 153.03     0 deaths 2020-01-29T00:00:00Z
    ## 10 Australia AU          Queensland      ""    ""       -27.47 153.03     0 deaths 2020-01-30T00:00:00Z
    ## # ... with 4,734 more rows

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
  } else if(func == "summary"){
    output <- covidSummary(...)
  } else {
    stop("ERROR: choose total, live, dayOne, or summary as function")
  }
  return(output)
}
```

``` r
covid("summary", "country")
```

    ## # A tibble: 192 x 10
    ##    Country             NewConfirmed TotalConfirmed NewDeaths TotalDeaths NewRecovered TotalRecovered Date       Continent percentDeath
    ##    <chr>                      <int>          <int>     <int>       <int>        <int>          <int> <date>     <chr>            <dbl>
    ##  1 Afghanistan                    0         155174         0        7204            0              0 2021-10-01 Asia              4.64
    ##  2 Albania                        0         170131         0        2698            0              0 2021-10-01 Europe            1.59
    ##  3 Algeria                        0         203359         0        5812            0              0 2021-10-01 Africa            2.86
    ##  4 Andorra                        0          15222         0         130            0              0 2021-10-01 Europe            0.85
    ##  5 Angola                         0          56583         0        1537            0              0 2021-10-01 Africa            2.72
    ##  6 Antigua and Barbuda            0           3231         0          79            0              0 2021-10-01 Americas          2.45
    ##  7 Argentina                      0        5256902         0      115179            0              0 2021-10-01 Americas          2.19
    ##  8 Armenia                        0         261697         0        5319            0              0 2021-10-01 Asia              2.03
    ##  9 Australia                   2058         107181        20        1311            0              0 2021-10-01 Oceania           1.22
    ## 10 Austria                        0         743095         0       11009            0              0 2021-10-01 Europe            1.48
    ## # ... with 182 more rows

``` r
covid("live", "gb", "id", "confirmed") %>% arrange(Province, Date) #%>% filter(Province == "Wisconsin")
```

    ## # A tibble: 1,470 x 13
    ##    ID                                   Country        CountryCode Province City  CityCode Lat   Lon    Confirmed Deaths Recovered Active Date         
    ##    <chr>                                <chr>          <chr>       <chr>    <chr> <chr>    <chr> <chr>      <int>  <int>     <int>  <int> <chr>        
    ##  1 4bcc437a-bed0-4ed0-91b5-ed12376dc857 United Kingdom GB          Anguilla ""    ""       18.22 -63.07       109      0       109      0 2021-06-25T0~
    ##  2 d58a0550-69d5-4478-80ab-7b331ea90840 United Kingdom GB          Anguilla ""    ""       18.22 -63.07       109      0       109      0 2021-06-26T0~
    ##  3 63671b21-741b-44f9-8de3-2e2d70df23a8 United Kingdom GB          Anguilla ""    ""       18.22 -63.07       109      0       109      0 2021-06-27T0~
    ##  4 49f467a7-a520-47e5-a57b-663892999235 United Kingdom GB          Anguilla ""    ""       18.22 -63.07       109      0       109      0 2021-06-28T0~
    ##  5 43c333f9-b938-4d28-8126-58a8c4313487 United Kingdom GB          Anguilla ""    ""       18.22 -63.07       109      0       109      0 2021-06-29T0~
    ##  6 1e2ec39f-65c5-4f90-b4bf-ff4d28a47548 United Kingdom GB          Anguilla ""    ""       18.22 -63.07       109      0       109      0 2021-06-30T0~
    ##  7 f4c5e405-c301-4629-a689-3ccf07329ed5 United Kingdom GB          Anguilla ""    ""       18.22 -63.07       109      0       109      0 2021-07-01T0~
    ##  8 9f9e9fa8-18b0-4c81-aed7-35d7f7ddc4d7 United Kingdom GB          Anguilla ""    ""       18.22 -63.07       109      0       109      0 2021-07-02T0~
    ##  9 383faecc-1405-4af9-9b15-80255d587592 United Kingdom GB          Anguilla ""    ""       18.22 -63.07       109      0       109      0 2021-07-03T0~
    ## 10 190b7980-9088-4b8c-903e-42dbbb800639 United Kingdom GB          Anguilla ""    ""       18.22 -63.07       111      0       109      2 2021-07-04T0~
    ## # ... with 1,460 more rows

Trying to get the province since some countries like the united states
won’t allow for the whole thing to be brought in because it’s too large.

``` r
test <- GET("https://api.covid19api.com/dayone/country/united-states")
another <- fromJSON(rawToChar(test$content))
print(another)
```

    ## $message
    ## [1] "for performance reasons, please specify a province"

# Data Exporation

Make a scatter plot of values for the country summary with continents in
different colors or totals in bars. Maybe both

``` r
regionData <- covidSummary("country")
regionData
```

    ## # A tibble: 192 x 10
    ##    Country             NewConfirmed TotalConfirmed NewDeaths TotalDeaths NewRecovered TotalRecovered Date       Continent percentDeath
    ##    <chr>                      <int>          <int>     <int>       <int>        <int>          <int> <date>     <chr>            <dbl>
    ##  1 Afghanistan                    0         155174         0        7204            0              0 2021-10-01 Asia              4.64
    ##  2 Albania                        0         170131         0        2698            0              0 2021-10-01 Europe            1.59
    ##  3 Algeria                        0         203359         0        5812            0              0 2021-10-01 Africa            2.86
    ##  4 Andorra                        0          15222         0         130            0              0 2021-10-01 Europe            0.85
    ##  5 Angola                         0          56583         0        1537            0              0 2021-10-01 Africa            2.72
    ##  6 Antigua and Barbuda            0           3231         0          79            0              0 2021-10-01 Americas          2.45
    ##  7 Argentina                      0        5256902         0      115179            0              0 2021-10-01 Americas          2.19
    ##  8 Armenia                        0         261697         0        5319            0              0 2021-10-01 Asia              2.03
    ##  9 Australia                   2058         107181        20        1311            0              0 2021-10-01 Oceania           1.22
    ## 10 Austria                        0         743095         0       11009            0              0 2021-10-01 Europe            1.48
    ## # ... with 182 more rows

``` r
ggplot(data = regionData, aes(x = NewDeaths, y = NewConfirmed)) + 
  geom_point(aes(color = Continent))
```

![](README_files/figure-gfm/unnamed-chunk-24-1.png)<!-- -->

``` r
ggplot(data = regionData, aes(x = Continent, y = NewConfirmed)) + 
  geom_bar(stat = "identity") +
  labs(title = "New Confirmed Cases by Continent")
```

![](README_files/figure-gfm/unnamed-chunk-25-1.png)<!-- -->

``` r
ggplot(data = regionData, aes(x = Continent, y = TotalConfirmed)) + 
  geom_bar(stat = "identity") +
  labs(title = "Total Confirmed Cases by Continent")
```

![](README_files/figure-gfm/unnamed-chunk-25-2.png)<!-- -->

Trying something quick. Plot the top 10-15 countires with the highest
new confirmed or new deaths or something

``` r
check <- regionData %>% arrange(desc(NewConfirmed)) %>% slice(1:15)

  ggplot(data = check, aes(x = Country, y = NewConfirmed)) + 
  geom_bar(stat = "identity") +
  labs(title = "Total Confirmed Cases by Continent")
```

![](README_files/figure-gfm/unnamed-chunk-26-1.png)<!-- -->
