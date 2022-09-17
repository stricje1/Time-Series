# Rolling or moving averages are a way to reduce noise and smooth time series data. During the Covid-19 pandemic, rolling averages have been used by researchers and journalists around the world to understand and visualize cases and deaths. This post will cover how to compute and visualize rolling averages for the new confirmed cases and deaths from Covid-19 in the United States.
# The Johns Hopkins COVID data

# The code block below imports the COVID-19 data from the Center for Systems Science and Engineering at the Johns Hopkins Whiting School of Engineering
# Wrangling steps

#All the steps for wrangling these data are in this gist.
#State-level Johns Hopkins COVID data

#We ended up with a data frame that has the following new columns.

#state - us state
#state_abbr - abbreviated state name
#month_abbr - month for data reported (with abbreviation)
#date - as_date() version of last_update
library(installr)
#uninstall.packages("tibble")

library(ggplot2)
library(zoo) # moving averages 
library(tibble)
library(tidyverse) # all tidyverse packages
library(hrbrthemes) # themes for graphs
library(socviz) # %nin%
library(geofacet) # maps
library(usmap) # lat and long
library(socviz) # for %nin%
library(ggmap) # mapping

covid19States2 <- readr::read_csv("https://raw.githubusercontent.com/mjfrigaard/storybench/master/drafts/data/jhsph/2020-06-22-JHCovid19States.csv")
class(covid19States2)
metax2<-contents(covid19States2)
covid19States <- readr::read_csv("D:/Documents/Data/2020-06-22-JHCovid19States.csv")
class(covid19States)
metax<-contents(covid19States)
utils::head(covid19States)
dta <- read_csv("https://raw.githubusercontent.com/mjfrigaard/storybench/master/drafts/data/jhsph/2020-06-22-JHCovid19States.csv",
                col_types = cols()) %>% mutate(date = ymd(date))

start = (zoom_time.AddMinutes(-1)).ToOADate();
end = (zoom_time.AddMinutes(1)).ToOADate();

chart1.ChartAreas[0].AxisX.Minimum = start;
chart1.ChartAreas[0].AxisX.Maximum = end;

#Calculating rolling averages

#Two states (Colorado and Iowa) have seen an increase in their death rates. We're going to calculate and visualize the rolling averages for cumulative deaths and new cases in these states and compare them to the other 48 states.

#To calculate a simple moving average (over 7 days), we can use the rollmean() function from the zoo package. This function takes a k, which is an 'integer width of the rolling window. 

#The code below calculates a 3, 5, 7, 15, and 21-day rolling average for the deathsfrom COVID in the US.



covid19States <- covid19States %>%
  dplyr::arrange(desc(state)) %>% 
  dplyr::group_by(state) %>% 
  dplyr::mutate(death_03da = zoo::rollmean(deaths, k = 3, fill = NA),
                death_05da = zoo::rollmean(deaths, k = 5, fill = NA),
                death_07da = zoo::rollmean(deaths, k = 7, fill = NA),
                death_15da = zoo::rollmean(deaths, k = 15, fill = NA),
                death_21da = zoo::rollmean(deaths, k = 21, fill = NA)) %>% 
  dplyr::ungroup()

#Below is an example of this calculation for the state of Colorado,

covid19States %>% 
  dplyr::arrange(date) %>% 
  dplyr::filter(state == "Colorado") %>% 
  dplyr::select(state,
                date,
                deaths,
                death_03da:death_07da) %>% 
  utils::head(7)

# The calculation works like so,

## the first value in our new death_03da variable (510.3333) is the average deaths in Colorado from the first date with a data point on either side of it (i.e. the date 2020-04-13 has 2020-04-12 preceding it, and 2020-04-14 following it). We can check our math below.

mean(c(461, 499, 571))

#the first value in death_05da (132.0) is the average deaths in Colorado from the first date with two data points on either side of it (2020-04-14 has 2020-04-12 and 2020-04-13 preceding it, and 2020-04-15 and 2020-04-16following it). We can check our math below.

mean(c(461, 499, 571, 596, 668))
# And the first value in death_07da (609.7143) is the average death rate in Colorado from the first date with three data points on either side (2020-04-15 has 2020-04-12, 2020-04-13 and 2020-04-14 preceding it, and 2020-04-16, 2020-04-17, and 2020-04-18 following it). Check our math again:

mean(c(461, 499, 571, 596, 668, 725, 748))

#It's good practice to calculate rolling averages using an odd number for k (it makes the resulting values symmetrical).
# Symmetrical
# Each rolling mean is calculated from the numbers surrounding it. If we want to visualize and compare the three rolling means against the original deaths data, we can do this with a little pivot_ing,

covid19States %>% 
  dplyr::filter(state == "Colorado") %>% 
  tidyr::pivot_longer(names_to = "rolling_mean_key", 
                      values_to = "rolling_mean_value", 
                      cols = c(deaths, 
                               death_03da, 
                               death_21da)) %>%
  # after may 15
  dplyr::filter(date >= lubridate::as_date("2020-03-01") &
                  # before june 20
                  date <= lubridate::as_date("2020-08-31")) %>% 
  ggplot2::ggplot(aes(x = date, 
                      y = rolling_mean_value, 
                      color = rolling_mean_key)) +
  ggplot2::geom_line(size=1.25) +   
  ggplot2::labs(title = "Colorado's rolling average total COVID deaths", 
                subtitle = "Between 2020-05-15 and 2020-06-20",
                y = "Deaths", 
                color = "Metric",
                x = "Date") + 
  theme_classic()
###############################################################

covid19States <- covid19States %>%
  dplyr::arrange(desc(state)) %>% 
  dplyr::group_by(state) %>% 
  dplyr::mutate(death_03da = zoo::rollmean(new_deaths, k = 3, fill = NA),
                death_05da = zoo::rollmean(new_deaths, k = 5, fill = NA),
                death_07da = zoo::rollmean(new_deaths, k = 7, fill = NA),
                death_15da = zoo::rollmean(new_deaths, k = 15, fill = NA),
                death_21da = zoo::rollmean(new_deaths, k = 21, fill = NA)) %>% 
  dplyr::ungroup()

covid19States %>% 
  dplyr::filter(state == "Iowa") %>% 
  tidyr::pivot_longer(names_to = "rolling_mean_key", 
                      values_to = "rolling_mean_value", 
                      cols = c(deaths, 
                               death_03da, 
                               death_21da)) %>%
  dplyr::filter(date >= lubridate::as_date("2020-04-15") & # after may 15
                  date <= lubridate::as_date("2020-06-20")) %>% # before june 20
  ggplot2::ggplot(aes(x = date, 
                      y = rolling_mean_value, 
                      color = rolling_mean_key)) +
  ggplot2::geom_line(size=1.25) +   
  ggplot2::labs(title = "Iowa's rolling average total COVID deaths", 
                subtitle = "Between 2020-05-15 and 2020-06-20",
                y = "Deaths", 
                color = "Metric",
                x = "Date") + 
  hrbrthemes::theme_ipsum_rc()

#####################################################################
covid19States %>% 
  dplyr::filter(state == "Iowa") %>% 
  tidyr::pivot_longer(names_to = "rolling_mean_key", 
                      values_to = "rolling_mean_value", 
                      cols = c(deaths, 
                               death_03da, 
                               death_21da)) %>%
  dplyr::filter(date >= lubridate::as_date("2020-04-15") & # after may 15
                  date <= lubridate::as_date("2020-06-20")) %>% # before june 20
  ggplot2::ggplot(aes(x = date, 
                      y = rolling_mean_value, 
                      color = rolling_mean_key)) +
  ggplot2::geom_line(size=1.25) +   
  ggplot2::labs(title = "Iowa's rolling average total COVID deaths", 
                subtitle = "Between 2020-05-15 and 2020-06-20",
                y = "Deaths", 
                color = "Metric",
                x = "Date") + 
  hrbrthemes::theme_ipsum_rc()

#Which mean should I use?
  
#  The zoo::rollmean() function works by successively averaging each period (k) together. Knowing which period (k) to use in zoo::rollmean() is a judgment call. The higher the value of k, the smoother the line gets, but we are also sacrificing more data.

#If we compare the 3-day average (death_3da) to the 21-day average (death_21da), we see the line for deaths gets increasingly smooth.
#Calculating new cases in each state

#Below we get some help from dplyr::lag() to calculate the new cases in each state per day.

#We join this new calculation back to the JHCovid19States dataset, but rename it JHCovid19NewCases.

JHCovid19NewCases <- covid19States %>%
  # group this by state and day
  group_by(state, date) %>% 
  # get total deaths per day
  dplyr::summarize(
    confirmed_sum = (sum(confirmed, na.rm = TRUE))) %>% 
  # calculate 'new deaths' = todays deaths - yesterdays deaths
  mutate(new_confirmed_cases = confirmed_sum - dplyr::lag(x = confirmed_sum, n = 1, 
                                                          order_by = date)) %>% 
  dplyr::select(state, 
                new_confirmed_cases, 
                date) %>% 
  # join back to JHCovid19
  dplyr::left_join(., y = JHCovid19States, 
                   by = c("state", "date")) %>% 
  # reorganize
  dplyr::select(state,
                state_abbr,
                date,
                month_abbr,
                day,
                confirmed,
                dplyr::contains("confirm"),
                dplyr::contains("death"),
                lat, 
                long, 
                dplyr::ends_with("rate"))

# check ga
JHCovid19NewCases %>% 
  dplyr::filter(state == "Iowa") %>% 
  dplyr::select(state_abbr, date, confirmed, new_confirmed_cases) %>% 
  utils::head()

#We can see this calculation is getting the number of new confirmed cases each day correct. Now we can calculate the rolling mean for the new confirmed cases in each state.

JHCovid19NewCases <- JHCovid19NewCases %>%
  dplyr::group_by(state) %>% 
  dplyr::mutate(
    new_conf_03da = zoo::rollmean(new_confirmed_cases, k = 3, fill = NA),
    new_conf_05da = zoo::rollmean(new_confirmed_cases, k = 5, fill = NA),
    new_conf_07da = zoo::rollmean(new_confirmed_cases, k = 7, fill = NA),
    new_conf_15da = zoo::rollmean(new_confirmed_cases, k = 15, fill = NA),
    new_conf_21da = zoo::rollmean(new_confirmed_cases, k = 21, fill = NA)) %>% 
  dplyr::ungroup()

#Moving averages with geofacets

#We'll take a look at the seven-day moving averages of new cases across all states using the geofacet package.

#First we'll build two plots for Colorado, combine them, and then extend this to the entire country.
#Column graph for new cases

#We will limit the JHCovid19NewCases data to June 1st - June 21st.

JHCovid19NewCasesJun <- JHCovid19NewCases %>% 
  dplyr::filter(date >= lubridate::as_date("2020-06-01") & # after june 1
                  date <= lubridate::as_date("2020-06-20")) # before june 20

#Then we will create a ggplot2::geom_col() for the new_confirmed_cases.

#We will build these two graphs with hrbrthemes::theme_modern_rc().
library(graphics)
par(cex.main = 3, lwd = 1.5, font = 2, col.lab = 'green')
JHCovid19NewCasesJun %>% 
  dplyr::filter(state == "Colorado") %>% 
  ggplot(aes(x = day, y = new_confirmed_cases)) +
  geom_col(alpha = 1/10, color = 'dodgerblue4', fill = 'dodgerblue2') + 
  labs(title = "Colorado's new COVID cases", cex.main = 2,
                subtitle = "Rolling average between 2020-06-01 and 2020-06-20") +
  ylab(label = "New Cases") + 
  xlab(label ="Day") +
  hrbrthemes::theme_ipsum_rc()
  #theme_light()
par()

  #hrbrthemes::theme_ipsum_rc()

#Tidy dataset of new cases

#Now we want to add lines for the new_conf_ variables, so we'll use pivot_longer.

FLNewCasesTidy <- JHCovid19NewCasesJun %>% 
  # only Colorado
  dplyr::filter(state == "Colorado") %>% 
  # pivot longer
  tidyr::pivot_longer(names_to = "new_conf_av_key", 
                      values_to = "new_conf_av_value", 
                      cols = c(new_conf_03da,
                               new_conf_05da,
                               new_conf_07da)) %>% 
  # reduce vars
  dplyr::select(day, 
                date, 
                state, 
                state_abbr, 
                new_conf_av_value, 
                new_conf_av_key)

#Now we can combine them into a single plot.
#Line graph for new cases

JHCovid19NewCasesJun %>% 
  # Colorado new cases 
  dplyr::filter(state == "Colorado") %>% 
  ggplot2::ggplot(aes(x = day, 
                      y = new_confirmed_cases,
                      group(date))) +
  geom_col(alpha = 1/10, color = 'dodgerblue4', fill = 'dodgerblue2', lwd = .5) + 
  # add the line with new data
  ggplot2::geom_line(data = FLNewCasesTidy, 
                     mapping = aes(x = day, 
                                   y = new_conf_av_value, 
                                   color = new_conf_av_key), size=1.05) +   
  ggplot2::labs(title = "Colorado's new COVID cases", 
                subtitle = "Rolling average between 2020-06-01 and 2020-06-20",
                y = "New Cases", 
                color = "Metric",
                x = "Day") + 
  hrbrthemes::theme_ipsum_rc()


#We can see that the blue (7-day average) of new confirmed cases is definitely the smoothest line.

#Let's compare it to the 3-day average using a geofacet for the other states in the US.

#Again, we build our tidy data frame of new confirmed case metrics.

NewCasesTidy <- JHCovid19NewCasesJun %>% 
  # pivot longer
  tidyr::pivot_longer(names_to = "new_conf_av_key", 
                      values_to = "new_conf_av_value", 
                      cols = c(new_conf_03da,
                               new_conf_07da)) %>% 
  # better labels for printing
  dplyr::mutate(new_conf_av_key = dplyr::case_when(
    new_conf_av_key == "new_conf_03da" ~ "3-day new confirmed cases",
    new_conf_av_key == "new_conf_07da" ~ "7-day new confirmed cases",
    TRUE ~ NA_character_)) %>% 
  # reduce vars
  dplyr::select(day, 
                date, 
                state, 
                state_abbr, 
                new_conf_av_value, 
                new_conf_av_key)


#Column + line + facet_geo()

#And we'll switch the theme to hrbrthemes::theme_ipsum_tw(). We also use the min and max to get values for the subtitle.


# get min and max for labels
min_date <- min(JHCovid19NewCasesJun$date, na.rm = TRUE)
max_date <- max(JHCovid19NewCasesJun$date, na.rm = TRUE)

library(geofacet)
library(ggplot2)
library(hrbrthemes) # themes for graphs


JHCovid19NewCasesJun %>% 
  ggplot2::ggplot(aes(x = day, 
                      y = new_confirmed_cases)) +
  geom_col(alpha = 3/10, linetype = 0) + 
  ggplot2::geom_line(data = NewCasesTidy, 
                     mapping = aes(x = day, 
                                   y = new_conf_av_value, 
                                   color = new_conf_av_key)) +  
  geofacet::facet_geo( ~ state_abbr, 
                       grid = "us_state_grid2",
                       scales = "free_y")  +
  ggplot2::labs(title = "US rolling 3 and 7-day averages of new COVID cases", 
                subtitle = "Between 2020-05-31 and 2020-06-20",
                y = "New Cases",
                color = "Metric:", 
                x = "Day") + 
  hrbrthemes::theme_ipsum_tw() + 
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) + 
  ggplot2::theme(legend.position = "top")

# get data foronly 7-day average
JHCovid19NewCasesJun7da <- JHCovid19NewCasesJun %>% 
  dplyr::select(day, new_conf_07da, state, state_abbr)
# get min and max for labels
min_date <- min(JHCovid19NewCasesJun$date, na.rm = TRUE)
max_date <- max(JHCovid19NewCasesJun$date, na.rm = TRUE)
JHCovid19NewCasesJun %>% 
  ggplot2::ggplot(aes(x = day, 
                      y = new_confirmed_cases)) +
  geom_col(alpha = 2/10, linetype = 0) + 
  ggplot2::geom_line(data = JHCovid19NewCasesJun7da, 
                     mapping = aes(x = day, 
                                   y = new_conf_07da, 
                                   color = "darkred",
                     ), show.legend = FALSE) +  
  geofacet::facet_geo( ~ state_abbr, 
                       grid = "us_state_grid1",
                       scales = "free_y")  +
  ggplot2::labs(title = "US 7-day rolling average of new COVID cases", 
                subtitle = paste0("Between", min_date,  " and ", max_date),
                y = "New Cases",
                x = "Day") + 
  hrbrthemes::theme_ipsum_tw() +
  ggplot2::theme(axis.title.x = element_blank(),
                 axis.text.x = element_blank(),
                 axis.ticks.x = element_blank()) 

