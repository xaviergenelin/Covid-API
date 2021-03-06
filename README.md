Covid Vignette
================
Xavier Genelin
10/4/2021

-   [Packages](#packages)
-   [Functions](#functions)
    -   [`covidSummary`](#covidsummary)
    -   [`getCountry`](#getcountry)
    -   [`getState`](#getstate)
    -   [`dayOne`](#dayone)
    -   [`covid`](#covid)
-   [Data Exporation](#data-exporation)
-   [Wrap Up](#wrap-up)

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
-   `knitr`: used to create a table for summary statistics

``` r
library(tidyverse)
library(httr)
library(jsonlite)
library(countrycode)
library(knitr)
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
    output <- output %>% mutate(percentDeath = round(TotalDeaths / TotalConfirmed * 100, 2), 
                                Date = as.Date(Date))
    
  } else if(type == "country"){
    output <- as_tibble(dat$Countries)
    
    output <- output %>% select("Country", "CountryCode", "Slug", "NewConfirmed", "TotalConfirmed", 
                                "NewDeaths", "TotalDeaths", "NewRecovered", "TotalRecovered", "Date")
    
    # Adds the continent to the dataset based on the country name
    output$Continent <- countrycode(output$Country, origin = "country.name", destination = "continent")
    # The Republic of Kosovo is not in the dataset. After looking it up this is in Europe
    output$Continent[is.na(output$Continent)] <- "Europe"
    
    # Get a list of continents to display to the user if the result doesn't return any data
    continents <- unique(output$Continent)
    
    # This removes unnecessary columns and creates a new column that calculates the percentage 
    # of deaths by country and groups that percentage in 3 different levels
    output <- output %>% select("Country", "NewConfirmed", "TotalConfirmed", "NewDeaths", 
                                "TotalDeaths", "NewRecovered", "TotalRecovered", 
                                "Date", "Continent") %>% 
      mutate(percentDeath = round(TotalDeaths / TotalConfirmed * 100, 2), Date = as.Date(Date), 
             deathPercentGroup = ifelse(round(TotalDeaths / TotalConfirmed * 100, 2) >= 5, "High", 
                                        ifelse(round(TotalDeaths / TotalConfirmed * 100, 2) >= 2.5, 
                                               "Medium", "Low")))
    
    # Makes the groups into a factor
    output$deathPercentGroup <- as.factor(output$deathPercentGroup)
    # Set the levels of the factor 
    levels(output$deathPercentGroup) <- c("Low", "Medium", "High")
      if(continent != "all"){
        continent <- str_to_title(continent)
        output <- output %>% filter(Continent == continent)
      
        if(nrow(output) == 0){
          # If the user selects a continent that isn't in the data set
          # This provides a list to choose from
          stop(paste("Please select one of the following continents: ", 
                     paste(sort(continents), collapse = ", "), sep = " "))
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
  # If the user enters a country that returns nothing, stop the function
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
spell the state correctly or uses the right abbreviation.

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
  
  # checks to make sure the user inputs something that will return a state abbreviation
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
dayOne <- function(country, IDType, status = "all", province = "all", 
                   provType, city = "all", fromDate = "", toDate = ""){
  # This checks to see if there will be an error with the status
  # Prevents an error later on before manipulating the data set at all
  status <- tolower(status)
  if(!(status %in% c("all", "confirmed", "deaths", "recovered"))){
    stop("Status has to be one of the following: all, confirmed, deaths, recovered")
  }
  
  main <- "https://api.covid19api.com/dayone/country/"
  country <- getCountry(country, IDType)
  
  # This gets the main part of the url for the country the user wants to see data for
  url <- paste0(main, country)
  
  # This first checks to see if the user wants to see 
  # a specific type of status and put that into the url
  if(status != "all"){
    url <- paste0(url, "/status/", status)
  } # if they leave the status as all, they can specify the province, city, or date range
  
  # This will return country level data if possible 
  # Might need to return the provinces and cities depending on the country 
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
  
  # This is a message that gets outputted by the API if the data is too large
  if(output == "for performance reasons, please specify a province"){
    stop("Please specify a province for this country")
  }
  
  # if there is a status that will change the data frame 
  # this will remove any unnecessary information from the output
  if(status == "all"){
    output <- output %>% select("Country", "Province", "City", "Confirmed", "Deaths", 
                                "Recovered", "Active", "Date") %>% 
      mutate(Date = as.Date(Date), deathPercentage = round(Deaths/Confirmed, 2))
  } else {
    output <- output %>% select("Country", "Province", "City", "Cases", "Status", "Date") %>% 
      mutate(Date = as.Date(Date))
  }
  
  if(city != "all"){
    # Create a vector of all the cities for the selected province in case of a wrong input
    cities <- sort(unique(output$City))
    output <- output %>% filter(City == city)
    # If the city isn't in the data set, the above code will return a data frame with 0 rows
    if(nrow(output) == 0){
      stop(paste("ERROR: Please select one of these cities for the selected province: ", 
                 paste(cities, collapse = ", "), sep = " "))
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

# Data Exporation

The first thing we can look at is the global covid numbers. This table
will show the total numbers since the beginning while also having the
new confirmed, deaths, and recovered by the most recent date. The
percentDeath column was created to show the total death percentage for
the globe.

``` r
kable(covid("summary", "global"))
```

| NewConfirmed | TotalConfirmed | NewDeaths | TotalDeaths | NewRecovered | TotalRecovered | Date       | percentDeath |
|-------------:|---------------:|----------:|------------:|-------------:|---------------:|:-----------|-------------:|
|       149291 |      234582768 |      1961 |     4798370 |            0 |              0 | 2021-10-04 |         2.05 |

There are a lot of countries in the world, so it would be difficult to
see any sort of detail if we were to show every one of the countries. So
next we’ll plot the top 15 countries with the highest death percentage
with confirmed cases above 100. The bars have the number of total
confirmed cases to give us an idea of what the percentage of deaths
represents.

``` r
country <- covid("summary", "country")
topCountry <- country %>% filter(TotalConfirmed > 100) %>% arrange(desc(percentDeath)) %>% slice(1:15)

ggplot(data = topCountry, aes(x = Country, y = percentDeath)) + 
  geom_bar(stat = "identity", aes(fill = Continent)) +
  labs(title = "Top 15 Countries by Percentage of Deaths", y = "Percent of Deaths" ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_text(aes(label = TotalConfirmed), size = 2.5, vjust = -0.2)
```

![](README_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

We can see there’s a decent mix of continents that have the highest
death percentages overall, but this may not be a good indicator of how
they’re doing recently. As we can see with Yemen, they have a large
percentage but they also have less than 10,000 confirmed cases. This may
be a country with a small population so this is still significant, or
they have done a good job of containing the virus and reducing spread.
This could be something that can be analyze further if necessary. The
top percentages look to be a little under 5%, so we check how each
continent compares to one another based on the different categories.

``` r
table(country$Continent, country$deathPercentGroup)
```

    ##           
    ##            Low Medium High
    ##   Africa     3     33   18
    ##   Americas   3     22   10
    ##   Asia       3     39    5
    ##   Europe     0     36    9
    ##   Oceania    1     10    0

Oceania doesn’t have any high percentage of deaths, which could be due
to their stricter rules (like in Australia for example). A lot of
countries for each continent are in the medium category, which is a
total death percentage between 2.5% and 5%. Africa has the highest
number of high death percentages, which may be due to less accessible
health services. Unfortunately, not a lot of countries have low death
percentages and Europe doesn’t have any. Next we’ll look at a scatter
plot of the new confirmed cases and the new confirmed deaths for each
country. This will be the most recent day’s data.

``` r
ggplot(country, aes(x = NewConfirmed, y = NewDeaths)) +
  geom_point(aes(color = Continent)) + 
  labs(title = "New Covid Cases vs New Covid Deaths", x = "New Cases", y = "New Deaths")
```

![](README_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

There seems to be a lot of variation in deaths with high number of
cases. We can also see that 3 of the top 4 death numbers are from the
Americas, with the top one being a European country. We can use the same
data set to find out what those 4 countries are to do a further
analysis.

``` r
kable(country %>% arrange(desc(NewDeaths)) %>% slice(1:4))
```

| Country                  | NewConfirmed | TotalConfirmed | NewDeaths | TotalDeaths | NewRecovered | TotalRecovered | Date       | Continent | percentDeath | deathPercentGroup |
|:-------------------------|-------------:|---------------:|----------:|------------:|-------------:|---------------:|:-----------|:----------|-------------:|:------------------|
| Russian Federation       |        25161 |        7474850 |       882 |      206179 |            0 |              0 | 2021-10-04 | Europe    |         2.76 | High              |
| United States of America |        25215 |       43683048 |       237 |      701169 |            0 |              0 | 2021-10-04 | Americas  |         1.61 | Medium            |
| Brazil                   |         9004 |       21468121 |       225 |      597948 |            0 |              0 | 2021-10-04 | Americas  |         2.79 | High              |
| India                    |        20799 |       33834702 |       180 |      448997 |            0 |              0 | 2021-10-04 | Asia      |         1.33 | Medium            |

Now we’ll examine the Russian Federation, the United States, Mexico, and
Brazil to see how they have been doing with confirmed cases and deaths
since their first confirmed covid case. To get country level data, we
can use the `dayone` option from the `covid` function. With the United
States being as large as it is, we’d need to filter on one of the 50
states and can’t get country level data quickly/easily. So we’ll look at
just Russia, Brazil, and Mexico. The boxes show the daily number of
cases from each country’s first confirmed case.

``` r
russia <- covid("dayone", "Russian Federation", "country")
brazil <- covid("dayone", "Brazil", "country")
mexico <- covid("dayone", "Mexico", "country")

# combines the 3 datasets into one to put the next couple plots side by side
boxes <- rbind(russia, brazil, mexico)

ggplot(boxes, aes(x = Country, y = Confirmed)) +
  geom_boxplot() +
  labs(title = "Confirmed Cases by Country", y = "Confirmed Cases")
```

![](README_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

``` r
ggplot(boxes, aes(x = Country, y = Deaths)) +
  geom_boxplot() +
  labs(title = "Deaths by Country")
```

![](README_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

So we can see that Brazil has had much higher confirmed numbers and also
more deaths compared to Brazil and Russia. Since this is a highly
populated country, this would make sense. The interesting aspect of this
is that Russia has higher number of confirmed cases compared to Mexico,
but Mexico has a higher number of deaths than Russia.

To dive in a little deeper with the United States we need to choose a
state. I’ve been in Wisconsin during the whole pandemic, so I’m
interested to see what our numbers look like. This will bring in data
for each of the counties in Wisconsin as well, so we can choose to see
it at either a state or county level. We’ll look at this at a state
level and Trempealeau county (this is my county).

``` r
wisco <- covid("dayone", "US", "id", status = "all", "wisconsin", "name")
# filter on trempealeau county
tremp <- wisco %>% filter(City == "Trempealeau") %>% select(Confirmed, Deaths, Recovered, Date)

# get state level data for confirmed, deaths, recovered
wisco <- wisco %>% group_by(Date) %>% 
  summarize(Confirmed = sum(Confirmed), Deaths = sum(Deaths), Recovered = sum(Recovered))
```

Both plots will be showing the distribution of the daily number of
confirmed cases.

``` r
ggplot(wisco, aes(x = Confirmed)) +
  geom_histogram(color = "black", fill = "red", bins = 50) +
  labs(title = "Histogram of Confirmed Cases in Wisconsin")
```

![](README_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

``` r
ggplot(tremp, aes(x = Confirmed)) +
  geom_histogram(color = "black", fill = "red", bins = 50) +
  labs(title = "Histogram of Confirmed Cases in Trempealeau County")
```

![](README_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->

Both of these look to be distributed similarly, although on different
scales due to the difference in populations. The cases aren’t the entire
story of this though. There are also people that recover from this daily
and also unfortunately die. We can look at Wisconsin

``` r
badgers <- gather(wisco, status, value, Confirmed, Deaths, Recovered)

output <- badgers %>% group_by(status) %>% summarize(min = min(value), q1 = quantile(value, 0.25), 
                                                     med = median(value), avg = mean(value), 
                                                     q3 = quantile(value, 0.75), max = max(value))

kable(output)
```

| status    |    min |        q1 |    med |        avg |        q3 |    max |
|:----------|-------:|----------:|-------:|-----------:|----------:|-------:|
| Confirmed | 376238 | 602039.75 | 659148 | 636678.199 | 679826.00 | 814187 |
| Deaths    |   3150 |   6650.75 |   7538 |   7166.734 |   8200.75 |   8901 |
| Recovered |      0 |      0.00 |      0 |      0.000 |      0.00 |      0 |

It’s interesting to see that the recovered numbers are all 0 for the
state level. This could be something specific with the API or something
else that is going wrong. We do a similar analysis for country levels as
well. Let’s take a look at how the numbers look for the place where this
all started, China.

``` r
china <- covid("dayone", "China", "country")

china <- china %>% group_by(Date) %>% summarize(Confirmed = sum(Confirmed), Deaths = sum(Deaths), 
                                                Recovered = sum(Recovered))

results <- gather(china, status, value, Confirmed, Deaths, Recovered) %>% group_by(status) %>% 
  summarize(min = min(value), q1 = quantile(value, 0.25), med = median(value), 
            avg = mean(value), q3 = quantile(value, 0.75), max = max(value))

kable(results)
```

| status    | min |    q1 |   med |       avg |     q3 |    max |
|:----------|----:|------:|------:|----------:|-------:|-------:|
| Confirmed | 548 | 84701 | 92586 | 91622.163 | 102517 | 108555 |
| Deaths    |  17 |  4641 |  4742 |  4429.715 |   4846 |   4849 |
| Recovered |   0 | 78374 | 85398 | 73985.599 |  95718 |  99228 |

Unlike when we examined the Wisconsin data set, the China data set does
have recovered numbers. We can see there’s a large jump from the minimum
values to the first quantile, which makes sense because viruses become
more contagious as they spread.

# Wrap Up

This vignette is just a small part of what someone can do with the Covid
API. There are other endpoints within the API that one can go to, and
within each of the endpoints there are countless things someone can
examine with this. This did some exploratory analysis into summary data
and country, state, and county level data.
