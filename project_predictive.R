library(tidyverse)
library(ggplot2)
coffeeGroc <- read_table("coffee_groc_1114_1165.dat")
coffeeGroc$SY <- as.character(coffeeGroc$SY)
coffeeGroc$GE <- as.character(coffeeGroc$GE)
coffeeGroc$VEND <- as.character(coffeeGroc$VEND)
coffeeGroc$ITEM <- as.character(coffeeGroc$ITEM)

stores <- read_table('Delivery_Stores.dat') #delivery store data
prodCoffee <- readxl::read_xls('prod_coffee.xls') # coffee product data
weekTranslation <- readxl::read_xls('IRI week translation.xls') # week translations
grocProd <- dplyr::left_join(coffeeGroc, prodCoffee, 
                             by = c('SY'='SY','GE'='GE','VEND'='VEND','ITEM'='ITEM')) #joining grocery data and product data on UPC
groceryStores <- dplyr::left_join(grocProd, stores, by='IRI_KEY') #joining grocery data with week translations on IRI_key
dropCols <- c("SY","GE","VEND","ITEM","L1","L9","Level","*STUBSPEC 1440RC                                                         00004","FAT CONTENT","FORM...21","EST_ACV","Open","Clsd")
#Dropping SY, GE, VEND, ITEM (because they are present in upc code) and other redundant data
groceryStores <- select(groceryStores, -dropCols)
#Example
#write.csv(groceryStores,'groceryStores.csv')
top6Brands <- groceryStores %>% 
  group_by(L5) %>% 
  summarise(dollar_sales = sum(DOLLARS/UNITS)) %>% 
  arrange(desc(dollar_sales)) %>% 
  top_n(n=6)
ms <- top6Brands %>%
  mutate(share= dollar_sales/sum(dollar_sales))
ggplot(data=top6Brands, aes(reorder(L5,-dollar_sales),dollar_sales)) + 
  geom_bar(stat = "identity", color="black") +
  xlab("Brand") + 
  ylab("Sales") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#two tailed t test
# Test if the Dollar sales of Maxwell House is significantly different from STARBUCKS in ground coffee category
# Null Hyp H0: mean(dollar sales of Maxwell House)= mean(dollar sales of Starbucks)
#Alt Hyp Ha: mean(dollar sales of Maxwell House)!= mean(dollar sales of Starbucks)
max_star <- groceryStores %>%
  filter(L2=="GROUND COFFEE", L5 %in% c("MAXWELL HOUSE","STARBUCKS"))
t.test((max_star$DOLLARS/max_star$UNITS)~max_star$L5)
# Top brands by dollar sales within KRAFT FOOD INC.
groceryStores %>% 
  filter(L4=="KRAFT FOODS INC.") %>%
  group_by(L5) %>% 
  summarise(dollar_sales = sum(DOLLARS/UNITS)) %>% 
  arrange(desc(dollar_sales)) %>%
  top_n(n=5)
#Top brands selling Ground Coffee by dollar sales within KRAFT FOOD INC.
groceryStores %>% 
  filter(L4=="KRAFT FOODS INC.", L2=="GROUND COFFEE") %>%
  group_by(L5) %>% 
  summarise(dollar_sales = sum(DOLLARS/UNITS)) %>% 
  arrange(desc(dollar_sales)) %>%
  top_n(n=5)
#Top brands selling Ground Coffee by dollar sales within KRAFT FOOD INC. by region
groceryStores %>% 
  filter(L4=="KRAFT FOODS INC.", L2=="GROUND COFFEE", L5=="MAXWELL HOUSE") %>%
  group_by(Market_Name) %>% 
  summarise(dollar_sales = sum(DOLLARS/UNITS)) %>% 
  arrange(desc(dollar_sales)) %>%
  top_n(n=5)

groceryStores %>% 
  filter(L4=="KRAFT FOODS INC.", L2=="GROUND COFFEE", L5=="STARBUCKS") %>%
  group_by(Market_Name) %>% 
  summarise(dollar_sales = sum(DOLLARS/UNITS)) %>% 
  arrange(desc(dollar_sales)) %>%
  top_n(n=5)

mxStores <- groceryStores %>% 
  filter(L4=="KRAFT FOODS INC.", L2=="GROUND COFFEE", L5=="MAXWELL HOUSE") %>%
  group_by(Market_Name) %>% 
  summarise(dollar_sales = sum(DOLLARS/UNITS)) %>% 
  arrange(desc(dollar_sales))
stbStores <- groceryStores %>% 
  filter(L4=="KRAFT FOODS INC.", L2=="GROUND COFFEE", L5=="STARBUCKS") %>%
  group_by(Market_Name) %>% 
  summarise(dollar_sales = sum(DOLLARS/UNITS)) %>% 
  arrange(desc(dollar_sales))

mx <- mxStores %>% arrange(Market_Name)
stb <- stbStores %>% arrange(Market_Name)
regions <- dplyr::inner_join(mx,stb,by="Market_Name")
regions[regions$dollar_sales.x<regions$dollar_sales.y,]

groceryStores %>% 
  filter(L4=="KRAFT FOODS INC.", L2=="GROUND COFFEE", L5=="MAXWELL HOUSE") %>%
  group_by(MskdName) %>% 
  summarise(dollar_sales = sum(DOLLARS/UNITS)) %>% 
  arrange(desc(dollar_sales), .by_group=TRUE) %>%
  top_n(5)

groceryStores %>% 
  filter(L4=="KRAFT FOODS INC.", L2=="GROUND COFFEE", L5=="STARBUCKS") %>%
  group_by(MskdName) %>% 
  summarise(dollar_sales = sum(DOLLARS/UNITS)) %>% 
  arrange(desc(dollar_sales), .by_group=TRUE) %>%
  top_n(5)
# Sale of number of units per week
weeklySales <- groceryStores %>%
  filter(L4=="KRAFT FOODS INC.", L2=="GROUND COFFEE", L5=="MAXWELL HOUSE") %>%
  group_by(WEEK) %>%
  summarise(weekly_sales = sum(UNITS))
weeklySales
ggplot(weeklySales, aes(WEEK,weekly_sales)) +
  geom_line(stat="identity") +
  xlab("Week") +
  ylab("Units Sold")
# Weekly price per per ounce
weeklyPpoz <- groceryStores %>%
  filter(L4=="KRAFT FOODS INC.", L2=="GROUND COFFEE", L5=="MAXWELL HOUSE") %>%
  group_by(WEEK) %>%
  summarise(weekly_ppoz = mean((DOLLARS)/(UNITS*VOL_EQ*16)))
weeklyPpoz
ggplot(weeklyPpoz, aes(WEEK,weekly_ppoz)) +
  geom_line(stat="identity") +
  xlab("Week") +
  ylab("Avg Price/ounce")
# Line chart of weekly price per ounce

ggplot(weeklyPpoz, aes(WEEK,weekly_ppoz)) +
  geom_line(stat="identity") +
  xlab("Week") +
  ylab("Avg Price/ounce")


# Correlation between average price per ounce and number of units sold per week
cor(weeklySales$weekly_sales, weeklyPpoz$weekly_ppoz)
#There is a strong negative correlation between weekly sales and weekly price per ounce.
ggplot() +
  geom_line(data = weeklySales, aes(WEEK,log(weekly_sales)),color="darkred") +
  xlab("Week") +
  geom_line(data = weeklyPpoz, aes(WEEK,weekly_ppoz*30), color="steelblue") +
  scale_y_continuous(name="Units Sold", sec.axis=sec_axis(~./1,name="Avg Price/ounce")) +
  theme(
    axis.title.y.left=element_text(color="darkred"),
    axis.text.y.left=element_text(color="darkred"),
    axis.title.y.right=element_text(color="steelblue"),
    axis.text.y.right=element_text(color="steelblue")
  )
#It is evident that there is a sales increased during the week when the avg price per ounce was reduced.
# Create a subset of data for Maxwell House ground coffee
grocMaxwell <- groceryStores %>%
  filter(L2=="GROUND COFFEE", L5=="MAXWELL HOUSE")
# Create a new column with weight of annual share of display variable(D) for each group of UPC, convert F and PR to dummy variables

wtGrocMaxwell <- grocMaxwell %>%
  group_by(UPC) %>%
  mutate(wt = sum(D)/sum(.$D),
         feature = ifelse(F=="NONE",0,1),
         disp = ifelse(D==0,0,1))

featuredSales <- wtGrocMaxwell %>% 
  group_by(feature) %>%
  summarise(UnitsSold = sum(UNITS))
prReducedSales <- wtGrocMaxwell %>% 
  group_by(PR) %>%
  summarise(UnitsSold = sum(UNITS))
dispSales <- wtGrocMaxwell %>%
  group_by(disp) %>%
  summarise(UnitsSold = sum(UNITS))

ggplot(data=featuredSales, aes(reorder(feature,-UnitsSold),UnitsSold)) + 
  geom_bar(stat = "identity", color="black") +
  xlab("Feature") + 
  ylab("Units Sold")

#The number of units sold is greater for non-featured category than featured category. 


ggplot(data=prReducedSales, aes(reorder(PR,-UnitsSold),UnitsSold)) + 
  geom_bar(stat = "identity", color="black") +
  xlab("Reduced Price") + 
  ylab("Units Sold")

#The number of units sold is equivalent for both reduced price and regular price category


ggplot(data=dispSales, aes(reorder(disp,-UnitsSold),UnitsSold)) + 
  geom_bar(stat = "identity", color="black") +
  xlab("Display") + 
  ylab("Units Sold")

#The number of units sold is greater for non-display category than display category.

# Sales distribution by Flavor/Scent
flavor <- grocMaxwell %>%
  group_by(`FLAVOR/SCENT`) %>%
  summarize(DollarSales = sum(DOLLARS/UNITS))
ggplot(data=flavor, aes(reorder(`FLAVOR/SCENT`,-DollarSales),DollarSales)) + 
  geom_bar(stat = "identity", color="black") +
  xlab("Flavor") + 
  ylab("Dollar sales") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Sales distribution by Package

packaging <- grocMaxwell %>%
  group_by(PACKAGE) %>%
  summarize(DollarSales = sum(DOLLARS/UNITS))
ggplot(data=packaging, aes(reorder(PACKAGE,-DollarSales),DollarSales)) + 
  geom_bar(stat = "identity", color="black") +
  xlab("Packaging Type") + 
  ylab("Dollar sales") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Sales distribution by Brewing Method
bw <- grocMaxwell %>%
  group_by(`BREWING METHOD`) %>%
  summarize(DollarSales = sum(DOLLARS/UNITS))
ggplot(data=bw, aes(reorder(`BREWING METHOD`,-DollarSales),DollarSales)) + 
  geom_bar(stat = "identity", color="black") +
  xlab("Brewing Method") + 
  ylab("Dollar sales") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# ANOVA on Flavor, Package and Brewing Method
anovaData <- grocMaxwell %>%
  mutate(flavor = ifelse(`FLAVOR/SCENT`=="REGULAR",1,0),
         package = ifelse(PACKAGE=="CAN",1,0),
         brew = ifelse(`BREWING METHOD`=="ALL PURPOSE",1,0))


# Test if dollar sales of Regular flavor is significantly different from other flavors using ANOVA

flvTest <- aov(anovaData$DOLLARS/anovaData$UNITS ~ anovaData$flavor)
summary(flvTest)

#We can statistically conclude that dollar sales of Regular flavor coffee is significantly higher than other flavours. 
# Test if dollar sales of Canned Packaging is significantly different from other packaging using ANOVA

pkgTest <- aov(anovaData$DOLLARS/anovaData$UNITS ~ anovaData$package)
summary(pkgTest)

#We can statistically conclude that dollar sales of Canned coffee is significantly higher than other packaging 

# Test if dollar sales of All Purpose brewing method is significantly different from other brewing methods using ANOVA
bwTest <- aov(anovaData$DOLLARS/anovaData$UNITS ~ anovaData$brew)
summary(bwTest)
#We can statistically conclude that dollar sales of all purpose coffee is significantly higher than other brewing methods. 

#Linear regression
regData <- wtGrocMaxwell %>% 
  group_by(WEEK) %>%
  summarise(sales = mean(DOLLARS/UNITS),
            ppoz = weighted.mean((DOLLARS)/(UNITS*VOL_EQ*16), wt),
            display = mean(D),
            featured = mean(feature))
mod <- glm(sales~ppoz+display+featured, data=regData)
summary(mod)

# weekly sales= -0.547+ 23.018 ppoz + 2.07 * display at 90% confidence.
#The model interpretation is biased because the data is skewed,(i.e the no. of observations of the product featuring in the ad are very less compared to the observations where the product was not featured in advertisement)
table(wtGrocMaxwell$feature)
57183/(540977+57183)
#Only 9% of the data has been featured in the ad.
#Suggested Solution to avoid this problem:
#Maybe perform SMOTE Analysis to balance the data and then run a linear regression model to predict the sales 
#Weekly sales of coffee increased when the product was displayed in the store(when there were deals on it), while the ad was not that effective in this case
#head(groceryStores,1)
head(regData,1)
summary(grocMaxwell)
head(wtGrocMaxwell,1)
str(wtGrocMaxwell)
write.csv(wtGrocMaxwell,'wtGrocMaxwell.csv')



