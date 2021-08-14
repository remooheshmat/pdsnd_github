
# adding the important libraries
library(ggplot2)
library(scales)
library(lubridate)

ny = read.csv('new_york_city.csv')
wash = read.csv('washington.csv')
chi = read.csv('chicago.csv')

head(ny)

head(wash)

head(chi)

# coding a function to visualize the number of trips per month for each city
commonMonth <- function(data){
 
    data$Start.Time <- as.Date(data$Start.Time)

    ggplot(data, aes(format(Start.Time, '%m')))+
      geom_bar(stat = 'count')+
      scale_x_discrete(labels=c("Jan", "Feb", "Mar", "Apr", "May", "Jun")) +
      labs(x = "Month of the year", y = "Count of trips")+
      ggtitle("Number of trips per month")    
}
commonMonth(wash)
commonMonth(ny)
commonMonth(chi)

# coding a function to visualize the number of trips for each weekday for each city
weekday <- function(data){
    data$dayoftheweek <- wday(as.Date(data$Start.Time))
    ggplot(data, aes(dayoftheweek, stat='count'))+
    geom_bar()+
    scale_x_continuous(breaks=c(1,2,3,4,5,6,7), labels=c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")) +
    labs(x = "WeekDay", y = "Count of trips")+
    ggtitle("Number of trips per Weekday")
}
weekday(wash)
weekday(ny)
weekday(chi)

# coding a function to get the most common start station for each city
commonstartStation <- function(data){
    commonstation <- unique(data$Start.Station)
    commonstation[which.max(tabulate(match(data$Start.Station, commonstation)))]   
}

commonstartStation(ny)
commonstartStation(chi)
commonstartStation(wash)

# coding a function to get the user type counts and visualization for each city
usercounts <- function(data){
    users_count <-summary(data$User.Type)
    ggplot(data, aes(x=User.Type, stat='count'))+
    geom_bar()+
    labs(x='User Type', y='count', title='counts of each user type')
    
}
usercounts(wash)
usercounts(chi)
usercounts(ny)

# coding a function to get the user gender counts and visualization for each city
usergender <- function(data){
    users_gender <-summary(data$Gender)
    ggplot(data, aes(x=Gender, stat='count'))+
    geom_bar()+
    labs(x='User Gender', y='count', title='counts of each gender')   
}
usergender(chi)
usergender(ny)

system('python -m nbconvert Explore_bikeshare_data.ipynb')
print("Hello refactoring")
print("welcome github")
