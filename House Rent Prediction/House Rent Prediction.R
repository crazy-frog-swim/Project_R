## PFDA  GROUP ASSIGNMENT - APD2F2209CS(CYB)

## MEMBERS
# MEMBER - 1
# MEMBER - 2
# MEMBER - 3
# MEMBER - 4


# ---- Package Initialization -------


install.packages("ggplot2")
install.packages("dplyr")
install.packages("gridExtra")
install.packages("scales")
install.packages("plotly")
install.packages("ggpubr")


library(dplyr)
library(ggplot2)
library(plotly)
library(gridExtra)
library(scales)
library(ggpubr)



# Import File
house_rental <- read.csv(file.choose())

View(house_rental)

# ---- Data Cleansing & Transformation ----
## Remove
house_rental <- house_rental[,-1]

## Rename
house_rental <- rename(house_rental, Type_of_Area = Area.Type,Locality = Area.Locality, Furnishing_Status = Furnishing.Status,
                       Tenant_Prefered_Type = Tenant.Preferred, Person_to_Contact = Point.of.Contact)
### Transform
house_rental <- mutate(house_rental, Total_Rooms = BHK + Bathroom)


# ---- Data Exploration ----

## Calculate rows and columns in table
nrow(house_rental)
ncol(house_rental)

## To view summary of entire data set
summary(house_rental)

## Listing variables in Dataset
names(house_rental)

## Viewing selected number of Sample Data
sample_n(house_rental,10)

## View Distinct Localities
n_distinct(house_rental$Locality)

## View Distinct City
n_distinct(house_rental$City)

## View Unique data from Dataset
unique(house_rental$City)

## Displaying data in tabular format
table(house_rental$Tenant_Prefered_Type)

table(house_rental$Person_to_Contact)

# ---- Data Visualization ----

### --- Q1) Factors that affect rental pricing -----

##Q1A1 - Rent by Furnishing Status
rent_by_fstatus <- house_rental%>%group_by(Furnishing_Status)%>%summarize(avg_rent = mean(Rent))

ggplotly(ggplot(rent_by_fstatus, aes(Furnishing_Status, avg_rent)) +
         geom_bar(stat = 'Identity', fill='lightgreen') + 
         labs(title = 'Rent by Furnishing State', y = 'Rental (₹)',x ="Furnishing State") +
         scale_y_continuous(limits=c(0, 60000),n.breaks = 15,labels = scales::comma) + coord_flip())


##Q1A2 - Rent by No. of BHK
rent_by_BHK <- house_rental%>%group_by(BHK)%>%summarize(avg_rent = mean(Rent))

ggplotly(ggplot(rent_by_BHK, aes(factor(BHK), avg_rent)) + 
         geom_bar(stat = 'Identity', fill='lightpink') + 
         labs(title = 'Rent by BHK Count', y = 'Rental (₹)',x ="Number of BHKs") + 
         scale_y_continuous(n.breaks = 12,labels = scales::comma))


##Q1A3 - Rent by Size
rent_by_s_size = subset(house_rental, Rent <= 100000)

ggplot(rent_by_s_size, aes(Size,Rent)) + geom_point() +
      labs(x = "Property Size", y = "Rental (₹)", title = "Size of Property & Rental Price") + 
      scale_y_continuous(n.breaks = 12,labels = scales::comma) + 
      scale_x_continuous(n.breaks = 7,labels = scales::comma)


##Q1A4 - Rent by Total Rooms
rent_by_t_rooms <- house_rental%>%group_by(Total_Rooms)%>%summarize(avg_rent = mean(Rent))

ggplotly(ggplot(rent_by_t_rooms, aes(factor(Total_Rooms), avg_rent)) + 
         geom_bar(stat = 'Identity', fill='#FFCC66') +
         labs(title = 'Rent by Total Rooms', y = 'Rental (₹)',x ="Number of Rooms (Total)") + 
         scale_y_continuous(n.breaks = 15,labels = scales::comma) +
         theme_classic())


#____________________________________________________________________________________________________________________________________

### ---- Q2) Property Details in Each City ----

###Q2A1 - Average Rent
rent_by_City <- house_rental%>%group_by(City)%>%summarise(avg_rent=mean(Rent))

ggplotly(ggplot(rent_by_City, aes(City, avg_rent)) + 
         geom_bar(stat = 'Identity',col="blue", fill='red') +
         labs(title = 'Average Rental by City', y = 'Rental (₹)') + 
         scale_y_continuous(limits=c(0, 100000),n.breaks = 10,labels = scales::comma))
          

###Q2A2 - Area Type
ggplot(house_rental, aes(x=City, fill=Type_of_Area)) +
       geom_bar(stat = "count", position = "dodge") +
       geom_text(aes(label=..count..), stat="count", vjust= -0.4, position = position_dodge(0.9)) +
       labs(x="City", y="Area Type", title = "Type of Area in each city") +
       scale_y_continuous(limits=c(0, 1000),n.breaks = 5)


###Q2A3 - Maximum Rental Property Size
rent_by_size = house_rental%>% group_by(City)%>% summarise(size=max(Size))

plot_ly(type="pie",
        labels=rent_by_size$City,
        values=rent_by_size$size,
        textinfo="label+value+percent",
        insidetextfont=list(color="black"),
        rotation=0,
        pull=0.01) %>% layout(title="Max size for City")


###Q2A4 - Count of Furnishing State
ggplot(house_rental, aes(x=City,fill=Furnishing_Status)) + 
       geom_bar(stat = "count", position = "dodge") +
       geom_text(aes(label=..count..), stat="count", vjust= -0.4, position = position_dodge(0.9)) +
       labs(x="City", y="Property Count", title = "Furninshing State in each city") + 
       scale_y_continuous(n.breaks = 7)


#__________________________________________________________________________________________________________________________________

### ---- Q3) Factors that affect desired tenant ----

###Q3A1 - Max Room Count
tenant_room<-house_rental %>% group_by(Tenant_Prefered_Type)%>% summarise(avg_room=max(Total_Rooms))

plot_ly(type="pie",
        labels=tenant_room$Tenant_Prefered_Type,
        values=tenant_room$avg_room,
        textinfo="label+value+percent",
        insidetextfont=list(color="black"),
        rotation=0,
        pull=0.01) %>% layout(title="Most rooms per tenant perferred type")


####Q3A2 - Average Property Size
tenant_sizeK<-house_rental%>%group_by(Tenant_Prefered_Type)%>%summarise(avg_size=mean(Size))

ggplotly(ggplot(tenant_sizeK, aes(Tenant_Prefered_Type, avg_size)) + 
         geom_bar(stat='Identity', fill='#a1e9f0') +
         labs(title = 'Average Property Size by Preferred Tenant ', y='Size', x = "Preferred Tenant") + 
         scale_y_continuous(n.breaks = 7) + coord_flip())


###Q3A3 - Average BHK Count
tenant_by_BHK<-house_rental%>%group_by(Tenant_Prefered_Type)%>%summarise(avg_BHK=mean(BHK))

ggplotly(ggplot(tenant_by_BHK, aes(Tenant_Prefered_Type, avg_BHK)) + 
         geom_bar(stat='Identity', fill='#eb8060') +
         labs(title = 'Average BHK Count By Tenant Preferred Type', y='BHK', x='Tenant Preferred Type') + 
         theme_bw())


###Q3A4 - Minimum Rental for each Tenant Type
tenant_price<-house_rental%>%group_by(Tenant_Prefered_Type)%>%summarise(avg_price=min(Rent))

plot_ly(type="pie",
        labels=tenant_price$Tenant_Prefered_Type,
        values=tenant_price$avg_price,
        textinfo="label+value+percent",
        insidetextfont=list(color="black"),
        rotation=0,
        pull=0.01) %>% layout(title="Minimum rental price for each tenant group")


# ----------- END ----------------------
