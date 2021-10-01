Covid Vignette
================
Xavier Genelin
10/2/2021

-   [Packages](#packages)
-   [Functions](#functions)
    -   [`covidSummary`](#covidsummary)
    -   [`getCountry`](#getcountry)
    -   [`getState`](#getstate)
    -   [`dayOne`](#dayone)
    -   [`covid`](#covid)
-   [Data Exporation](#data-exporation)

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

This also uses the `state.abb` and `state.name` from the `state`
dataset.

# Functions

The following functions help get the data from the covid API that allows
the users to select the type of data they want to see.

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
    
    # Creates a new variable that calculates the percentage of deaths globally
    output <- output %>% mutate(percentDeath = round(TotalDeaths / TotalConfirmed * 100, 2), Date = as.Date(Date))
    
  } else if(type == "country"){
    output <- as_tibble(dat$Countries)
    
    output <- output %>% select("Country", "CountryCode", "Slug", "NewConfirmed", 
                                "TotalConfirmed", "NewDeaths", "TotalDeaths", "NewRecovered", "TotalRecovered", "Date")
    
    output$Continent <- countrycode(output$Country, origin = "country.name", destination = "continent")
    
    output$Continent[is.na(output$Continent)] <- "Europe"
    continents <- unique(output$Continent)
    
    # This removes unnecessary columns and creates a new column that calculates the percentage of deaths by country
    output <- output %>% select("Country", "NewConfirmed", "TotalConfirmed", "NewDeaths", "TotalDeaths", "NewRecovered", "TotalRecovered", "Date", "Continent") %>% mutate(percentDeath = round(TotalDeaths / TotalConfirmed * 100, 2), Date = as.Date(Date))
    
      if(continent != "all"){
        continent <- str_to_title(continent)
        output <- output %>% filter(Continent == continent)
      
        if(nrow(output) == 0){
          # If the user selects a continent that isn't in the data set, it provides a list to choose from
          stop(paste("Please select one of the following continents: ", paste(sort(continents), collapse = ", "), sep = " "))
        }
      }
  }
  


  return(output)
}
```

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
    stop("ERROR: Country not found. Check spelling, possible capitalization, or IDType")
  }
  return(output)
}
```

## `getState`

This acts in a similar way to the `getCountry` function. Some of the
APIs require us to filter down either by time or province after
selecting the province. So if we wanted data on the United States, we
may need to filter by one of those two things. If we wanted to see
Wisconsin’s data, that is easy to put into the url and get the data. But
for states like West Virginia where there are more than one word in the
state’s name, this has to be the two letter abbreviation of the state.
This function will let the user choose either option for the state if
they need to filter down the United States data. There may be a similar
issue with provinces in other countries, but there doesn’t seem to be
anything in R to account for that at this time. This function defaults
to the state name and has a built in error message if the user doesn’t
spell the state corecctly or uses the right abbrevation.

``` r
getState <- function(state, type = "name"){
  states <- data.frame(abbreviation = state.abb, name = state.name)
  
  # removes any capitalization that the user may input
  type <- tolower(type)
  
  if(type == "name"){
    # This capitalizes the first letter of each word 
    # Prevents the filter from getting nothing if the user doesn't enter it properly
    state <- str_to_title(state)
    output <- states %>% filter(name == state) %>% select(abbreviation)
  } else{
    # Capitalizes each letter if the user doesn't to prevent it from returning nothing
    state <- toupper(state)
    output <- states %>% filter(abbreviation == state) %>% select(abbreviation)
  }
  
  if(nrow(output) == 0){
    stop("EROR: Make sure the state name is spelled correctly or the proper abbrevation")
  }
  
  return(output)
}
```

## `dayOne`

Difference versus day one and by country live is the start date. The day
one starts when the first case enters the country until the most recent
data while the live starts in january 2020 (probably the first case
globally) until the most recent data.

Quick notes It seems like they can only filter on the time range or
province in the url The date is still there after filtering on the
province (i.e. Wisconsin) so they could still see a certain range, but
that will need to be fitlered after getting the data They also need to
have a status of confirmed, deaths, or recovered

``` r
dayOne <- function(country, IDType, status = "all", province = "all", provType, city = "all", fromDate = "", toDate = ""){
  # This checks to see if there will be an error with the status later on before manipulating the dataset at all
  status <- tolower(status)
  if(!(status %in% c("all", "confirmed", "deaths", "recovered"))){
    stop("Status has to be one of the following: all, confirmed, deaths, recovered")
  }
  
  main <- "https://api.covid19api.com/dayone/country/"
  country <- getCountry(country, IDType)
  
  # This gets the main part of the url for the country the user wants to see data for
  url <- paste0(main, country)
  
  # This first checks to see if the user wants to see a specific type of status and put that into the url
  if(status != "all"){
    url <- paste0(url, "/status/", status)
  } # if they leave the status as all, they can specify the province, city, or date range
  
  # This will return country level data if possible, might return a 
  if(province == "all"){
    getAPI <- GET(url)
    dat <- fromJSON(rawToChar(getAPI$content))
    output <- as_tibble(dat)
    
  } else {
    province <- getState(province, type = provType)
    url <- paste0(url, "?province=", province)
    getAPI <- GET(url)
    dat <- fromJSON(rawToChar(getAPI$content))
    output <- as_tibble(dat)
  }
  
  if(output == "for performance reasons, please specify a province"){
    stop("Please specify a province for this country")
  }
  
  # if there is a status that will change the data frame 
  # this will remove any unnecessary information from the output
  if(status == "all"){
    output <- output %>% select("Country", "Province", "City", "Confirmed", "Deaths", "Recovered", "Active", "Date") %>% mutate(Date = as.Date(Date))
  } else {
    output <- output %>% select("Country", "Province", "City", "Cases", "Status", "Date") %>% mutate(Date = as.Date(Date))
  }
  
  if(city != "all"){
    # Create a vector of all the cities for the selected province in case of a wrong input
    cities <- sort(unique(output$City))
    output <- output %>% filter(City == city)
    # If the city isn't in the data set, the above code will return a data frame with 0 rows
    if(nrow(output) == 0){
      stop(paste("ERROR: Please select one of these cities for the selected province: ", paste(cities, collapse = ", "), sep = " "))
    }
  }
  
  # if the user specifies either the toDate and/or fromDate, this will filter the data set 
  if(toDate != ""){
    check <- nrow(output)
    output <- output %>% filter(Date <= as.Date(toDate))
    
    if(nrow(output) == check | nrow(output) == 0){
      stop("Plesae enter date in year-month-day format. Data wasn't filtered on this date")
    }
    
  }
  
  if(fromDate != ""){
    check <- nrow(output)
    output <- output %>% filter(Date >= as.Date(fromDate))
    
    if(nrow(output) == check | nrow(output) == 0){
      stop("Plesae enter date in year-month-day format. Data wasn't filtered on this date")
    }
  }
  
  return(output)
}
```

Testing to see if this will work as an error for the total function

It works for this one!!!!

``` r
getAPI <- GET("https://api.covid19api.com/dayone/country/united-states/status/confirmed?province=wv")
dat <- fromJSON(rawToChar(getAPI$content))
test <- as_tibble(dat)
test
```

    ## # A tibble: 17,325 x 10
    ##    Country         CountryCode Province    City  CityCode Lat   Lon   Cases Status  Date       
    ##    <chr>           <chr>       <chr>       <chr> <chr>    <chr> <chr> <int> <chr>   <chr>      
    ##  1 United States ~ US          West Virgi~ Hanc~ 54029    40.52 -80.~   519 confir~ 2020-11-22~
    ##  2 United States ~ US          West Virgi~ Mine~ 54057    39.42 -78.~   989 confir~ 2020-11-22~
    ##  3 United States ~ US          West Virgi~ Jack~ 54035    38.84 -81.~   654 confir~ 2020-11-22~
    ##  4 United States ~ US          West Virgi~ Calh~ 54013    38.84 -81.~    56 confir~ 2020-11-22~
    ##  5 United States ~ US          West Virgi~ Mari~ 54049    39.51 -80.~   715 confir~ 2020-11-22~
    ##  6 United States ~ US          West Virgi~ Putn~ 54079    38.51 -81.~  1645 confir~ 2020-11-22~
    ##  7 United States ~ US          West Virgi~ Ohio  54069    40.1  -80.~  1301 confir~ 2020-11-22~
    ##  8 United States ~ US          West Virgi~ Barb~ 54001    39.13 -80     344 confir~ 2020-11-22~
    ##  9 United States ~ US          West Virgi~ Boone 54005    38.02 -81.7   581 confir~ 2020-11-22~
    ## 10 United States ~ US          West Virgi~ Wyom~ 54109    37.61 -81.~   608 confir~ 2020-11-22~
    ## # ... with 17,315 more rows

## `covid`

Create one final function that uses the other functions above to collect
whatever data is needed

``` r
covid <- function(func, ...){
  func <- tolower(func)
  
  if(func == "summary"){
    output <- covidSummary(...)
  } else if(func == "dayone"){
    output <- dayOne(...)
  } else {
    stop("Please select either summary or dayone")
  }
  return(output)
}
```

``` r
covid("Summary", "Country", continent = "Americas")
```

    ## # A tibble: 35 x 10
    ##    Country             NewConfirmed TotalConfirmed NewDeaths TotalDeaths NewRecovered TotalRecovered
    ##    <chr>                      <int>          <int>     <int>       <int>        <int>          <int>
    ##  1 Antigua and Barbuda            0           3403         0          84            0              0
    ##  2 Argentina                      0        5259352         0      115239            0              0
    ##  3 Bahamas                        0          21114         0         533            0              0
    ##  4 Barbados                       0           8792         0          79            0              0
    ##  5 Belize                         0          21003         0         418            0              0
    ##  6 Bolivia                        0         500823         0       18750            0              0
    ##  7 Brazil                     13466       21459117       468      597723            0              0
    ##  8 Canada                      1728        1339341        22       25264            0              0
    ##  9 Chile                        813        1655884         8       37484            0              0
    ## 10 Colombia                    1497        4960641        36      126372            0              0
    ## # ... with 25 more rows, and 3 more variables: Date <date>, Continent <chr>,
    ## #   percentDeath <dbl>

# Data Exporation

Make a scatter plot of values for the country summary with continents in
different colors or totals in bars. Maybe both

``` r
regionData <- covid("summary", "country")
```

``` r
ggplot(data = regionData, aes(x = Continent, y = NewConfirmed)) + 
  geom_bar(stat = "identity") +
  labs(title = "New Confirmed Cases by Continent")
```

![](README_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

``` r
ggplot(data = regionData, aes(x = Continent, y = TotalConfirmed)) + 
  geom_bar(stat = "identity") +
  labs(title = "Total Confirmed Cases by Continent")
```

![](README_files/figure-gfm/unnamed-chunk-10-2.png)<!-- -->

Plot of the top 15 countires with the highest death percentage with
confirmed cases above 100. The bars have the total confirmed cases to
give us an idea of what the percentage of deaths represents.

``` r
check <- regionData %>% filter(TotalConfirmed > 100) %>% arrange(desc(percentDeath)) %>% slice(1:15)

ggplot(data = check, aes(x = Country, y = percentDeath)) + 
  geom_bar(stat = "identity", aes(fill = Continent)) +
  labs(title = "Top 15 Countries by Percentage of Deaths", y = "Percent of Deaths" ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_text(aes(label = TotalConfirmed), size = 2.5, vjust = -0.2)
```

![](README_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

We can see there’s a decent mix of continents that have the highest
death percentages overall, but this may not be a good indicator of how
they’re doing recently. As we can see with Yemen, they have a large
percentage but they also have less than 10,000 confirmed cases. This may
be a country with a small population so this is still significant, or
they have done a good job of containing the virus and reducing spread.
This could be something that can be analyze further if necessary.

Next we’ll look at the new confirmed cases

``` r
country <- covid("summary", "country")
```

``` r
ggplot(country, aes(x = NewConfirmed)) +
  geom_histogram(binwidth = 500)
```

![](README_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->
