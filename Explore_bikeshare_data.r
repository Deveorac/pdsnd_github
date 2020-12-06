
library(ggplot2)
library(dplyr)
library(forcats)

ny = read.csv('new_york_city.csv')
wash = read.csv('washington.csv')
chi = read.csv('chicago.csv')

head(ny)

summary(ny)

head(wash)

summary(wash)

head(chi)

summary(chi)

ny %>%
  group_by(Start.Station) %>%
  tally() %>%
  top_n(10) %>%
  arrange(desc(n)) %>%
  mutate(n=n/100,Start.Station=fct_reorder(Start.Station,n,last)) %>%
  
#plotting using ggplot : dataset, aesthetics, axis labels and grouping and arranging by maximum Start Stations

  ggplot()+
  geom_point(aes(y=Start.Station, x=n), fill='gray10', col="gray40")+
  geom_text(aes(y=Start.Station, x=n,label=round(n,1), hjust=-0.3), col="gray25", size=3)+
  labs(title="Top 10 most popular starting bike share locations in NYC",
       x="Popularity Measure",
       y="Stations"
)


wash%>%
  group_by(Start.Station) %>%
  tally() %>%
  top_n(10) %>%
  arrange(desc(n)) %>%
  mutate(n=n/100,Start.Station=fct_reorder(Start.Station,n,last)) %>%
  
#plotting using ggplot : dataset, aesthetics, axis labels and grouping and arranging by maximum Start Stations

  ggplot()+
  geom_point(aes(y=Start.Station, x=n), fill='gray10', col="gray40")+
  geom_text(aes(y=Start.Station, x=n,label=round(n,1), hjust=-0.3), col="gray25", size=3)+
  labs(title="Top 10 most popular starting bike share locations in Washington",
       x="Popularity Measure",
       y="Stations"
)



chi%>%
  group_by(Start.Station) %>%
  tally() %>%
  top_n(10) %>%
  arrange(desc(n)) %>%
  mutate(n=n/100,Start.Station=fct_reorder(Start.Station,n,last)) %>%
  
#plotting using ggplot : dataset, aesthetics, axis labels and grouping and arranging by maximum Start Stations

  ggplot()+
  geom_point(aes(y=Start.Station, x=n), fill='gray10', col="gray40")+
  geom_text(aes(y=Start.Station, x=n,label=round(n,1), hjust=-0.3), col="gray25", size=3)+
  labs(title="Top 10 most popular starting bike share locations in Chicago",
       x="Popularity Measure",
       y="Stations"
)



c1 <- rgb(173,216,230,max = 255)
c2 <- rgb(255,192,203, max = 255)
c3 <- rgb(159,128,64, max = 255)

dur_hist <- function(city, graph_color, c_name) {
    ggplot(aes(x = Trip.Duration), data=city) + geom_histogram(binwidth = 10, fill=graph_color, alpha = .9) + xlim(0,1440) + 
    labs(title= paste("Trip Durations under 24 Hours in ", c_name),
       x="Trip Duration",
       y="Count")
}

dur_hist(ny, c1, 'New York')

dur_hist(wash, c2, 'Washington') 

dur_hist(chi, c3, 'Chicago')

ggplot(ny, aes(x=Trip.Duration, color=User.Type)) + geom_histogram(binwidth = 50, fill = "white", position = 'dodge') + xlim(0,1440)

system('python -m nbconvert Explore_bikeshare_data.ipynb')
