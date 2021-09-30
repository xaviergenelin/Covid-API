Covid Vignette
================
Xavier Genelin
9/30/2021

-   [Packages](#packages)
    -   [Functions](#functions)
-   [Data Exporation](#data-exporation)

############## Current idea of what to do with this

I have the function created for the global and country summaries. Add in
the regions to allow the user to select a specific region for the
countries, can’t do that for the global one

Rework the live, total, and oneDay functions to allow the user to select
a date range, province, and city/city code. Probably just the city
because the code was a little odd. Not sure how it’s choosing that. I
should be able to print the list of options from the choices by using
`toString(sort(unique(data$City)))`. Maybe make a function for that too,
idk.

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

## Packages

To be able to interact with the API and manipulate the data we want to
return, the following packages will need to be installed and used:

*`tidyverse`: this package will allow us to manipulate and visualize the
data  
*`httr`: gives us a response object from the API url  
*`jsonlite`: allows us to interact with the API  
* `countrycode`: this lets us tie the country name to the continent
which is defined by the World Bank Development Indicators

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
```

``` r
# Day one all status
getAPI <- GET("https://api.covid19api.com/dayone/country/south-africa")

dat <- fromJSON(rawToChar(getAPI$content))

test1 <- as_tibble(dat)
test1 %>% arrange(desc(Date))
```

    ## # A tibble: 575 x 13
    ##    ID                                   Country      CountryCode Province City  CityCode Lat    Lon   Confirmed Deaths Recovered  Active Date          
    ##    <chr>                                <chr>        <chr>       <chr>    <chr> <chr>    <chr>  <chr>     <int>  <int>     <int>   <int> <chr>         
    ##  1 e4e18132-3ba3-459c-939e-8caee53e366b South Africa ZA          ""       ""    ""       -30.56 22.94   2898888  87417         0 2811471 2021-09-30T00~
    ##  2 5b946578-46dd-4627-b710-d56f81614b28 South Africa ZA          ""       ""    ""       -30.56 22.94   2898888  87417         0 2811471 2021-09-29T00~
    ##  3 0125bd67-56b7-46d6-a5ad-88a37d91c36c South Africa ZA          ""       ""    ""       -30.56 22.94   2898888  87417         0 2811471 2021-09-28T00~
    ##  4 fd723942-1255-4eff-96f6-637c3eba8973 South Africa ZA          ""       ""    ""       -30.56 22.94   2897521  87216         0 2810305 2021-09-27T00~
    ##  5 0a6851c4-b9fc-4da4-a64e-18256c27ad97 South Africa ZA          ""       ""    ""       -30.56 22.94   2896943  87052         0 2809891 2021-09-26T00~
    ##  6 f1f581d2-5a1d-4051-b569-d1cbca8d816d South Africa ZA          ""       ""    ""       -30.56 22.94   2895976  87001         0 2808975 2021-09-25T00~
    ##  7 d5588a54-eccc-4bf5-96b8-d68cb7fbf9d3 South Africa ZA          ""       ""    ""       -30.56 22.94   2894342  86967         0 2807375 2021-09-24T00~
    ##  8 bdefb549-7170-414d-b9ff-2aa6bbccd3b0 South Africa ZA          ""       ""    ""       -30.56 22.94   2892081  86655         0 2805426 2021-09-23T00~
    ##  9 af41b469-a99b-4a5e-8e05-38d4a6d13aa2 South Africa ZA          ""       ""    ""       -30.56 22.94   2889298  86500         0 2802798 2021-09-22T00~
    ## 10 dae8dfce-3b14-4361-b7b1-f80ec5a4388f South Africa ZA          ""       ""    ""       -30.56 22.94   2886331  86376         0 2799955 2021-09-21T00~
    ## # ... with 565 more rows

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

    ## # A tibble: 575 x 10
    ##    Country      CountryCode Province City  CityCode Lat    Lon   Cases Status Date                
    ##    <chr>        <chr>       <chr>    <chr> <chr>    <chr>  <chr> <int> <chr>  <chr>               
    ##  1 South Africa ZA          ""       ""    ""       -30.56 22.94 87417 deaths 2021-09-30T00:00:00Z
    ##  2 South Africa ZA          ""       ""    ""       -30.56 22.94 87417 deaths 2021-09-29T00:00:00Z
    ##  3 South Africa ZA          ""       ""    ""       -30.56 22.94 87417 deaths 2021-09-28T00:00:00Z
    ##  4 South Africa ZA          ""       ""    ""       -30.56 22.94 87216 deaths 2021-09-27T00:00:00Z
    ##  5 South Africa ZA          ""       ""    ""       -30.56 22.94 87052 deaths 2021-09-26T00:00:00Z
    ##  6 South Africa ZA          ""       ""    ""       -30.56 22.94 87001 deaths 2021-09-25T00:00:00Z
    ##  7 South Africa ZA          ""       ""    ""       -30.56 22.94 86967 deaths 2021-09-24T00:00:00Z
    ##  8 South Africa ZA          ""       ""    ""       -30.56 22.94 86655 deaths 2021-09-23T00:00:00Z
    ##  9 South Africa ZA          ""       ""    ""       -30.56 22.94 86500 deaths 2021-09-22T00:00:00Z
    ## 10 South Africa ZA          ""       ""    ""       -30.56 22.94 86376 deaths 2021-09-21T00:00:00Z
    ## # ... with 565 more rows

``` r
test2
```

    ## # A tibble: 575 x 10
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
    ## # ... with 565 more rows

### Functions

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

    ## # A tibble: 1 x 7
    ##   NewConfirmed TotalConfirmed NewDeaths TotalDeaths NewRecovered TotalRecovered Date                    
    ##          <int>          <int>     <int>       <int>        <int>          <int> <chr>                   
    ## 1       289562      232918565      5970     4771047            0              0 2021-09-30T18:52:53.477Z

This first function gets access to the covid summary data and allows the
user to choose either a global or country summary. The global summary
will return one row with data on NewConfirmed, TotalConfirmed,
NewDeaths, TotalDeaths, NewRecovered, and TotalRecovered along with the
date that this data is from. The country data will show the same data
for 192 countries as well as their country name, country code, and slug.

``` r
# The user can either choose a global summary or country

covidSummary <- function(type){
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
getAPI <- GET("https://api.covid19api.com/countries")

dat <- fromJSON(rawToChar(getAPI$content))

as_tibble(dat)
```

    ## # A tibble: 248 x 3
    ##    Country                Slug             ISO2 
    ##    <chr>                  <chr>            <chr>
    ##  1 Costa Rica             costa-rica       CR   
    ##  2 French Polynesia       french-polynesia PF   
    ##  3 Tunisia                tunisia          TN   
    ##  4 Macedonia, Republic of macedonia        MK   
    ##  5 Virgin Islands, US     virgin-islands   VI   
    ##  6 Bahrain                bahrain          BH   
    ##  7 Bhutan                 bhutan           BT   
    ##  8 Kenya                  kenya            KE   
    ##  9 China                  china            CN   
    ## 10 Israel                 israel           IL   
    ## # ... with 238 more rows

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

    ## # A tibble: 28,080 x 10
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
    ## # ... with 28,070 more rows

``` r
getAPI <- GET("https://api.covid19api.com/country/united-states?province=wisconsin")
dat <- fromJSON(rawToChar(getAPI$content))
data <- as_tibble(dat)

data %>% filter(City == "Dane")
```

    ## # A tibble: 312 x 13
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
    ## # ... with 302 more rows

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

    ## # A tibble: 617 x 10
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
    ## # ... with 607 more rows

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

    ## # A tibble: 97 x 13
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
    ## # ... with 87 more rows

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

    ## # A tibble: 4,736 x 10
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
    ## # ... with 4,726 more rows

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
    ##    Country             CountryCode Slug                NewConfirmed TotalConfirmed NewDeaths TotalDeaths NewRecovered TotalRecovered Date              
    ##    <chr>               <chr>       <chr>                      <int>          <int>     <int>       <int>        <int>          <int> <chr>             
    ##  1 Afghanistan         AF          afghanistan                    0         155128         0        7204            0              0 2021-09-30T18:52:~
    ##  2 Albania             AL          albania                        0         169462         0        2685            0              0 2021-09-30T18:52:~
    ##  3 Algeria             DZ          algeria                        0         203198         0        5805            0              0 2021-09-30T18:52:~
    ##  4 Andorra             AD          andorra                        0          15209         0         130            0              0 2021-09-30T18:52:~
    ##  5 Angola              AO          angola                         0          56583         0        1537            0              0 2021-09-30T18:52:~
    ##  6 Antigua and Barbuda AG          antigua-and-barbuda            0           3188         0          76            0              0 2021-09-30T18:52:~
    ##  7 Argentina           AR          argentina                      0        5255261         0      115130            0              0 2021-09-30T18:52:~
    ##  8 Armenia             AM          armenia                        0         260675         0        5299            0              0 2021-09-30T18:52:~
    ##  9 Australia           AU          australia                   2400         105123        12        1291            0              0 2021-09-30T18:52:~
    ## 10 Austria             AT          austria                        0         741046         0       10998            0              0 2021-09-30T18:52:~
    ## # ... with 182 more rows

``` r
covid("live", "gb", "id", "confirmed") %>% arrange(Province, Date) #%>% filter(Province == "Wisconsin")
```

    ## # A tibble: 1,455 x 13
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
    ## # ... with 1,445 more rows

Trying to get the province since some countries like the united states
won’t allow for the whole thing to be brought in because it’s too large.

``` r
test <- GET("https://api.covid19api.com/dayone/country/united-states")
another <- fromJSON(rawToChar(test$content))
print(another)
```

    ## $message
    ## [1] "for performance reasons, please specify a province"

## Data Exporation
