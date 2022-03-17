### A retail store is trying to figure out weak points in its business.
### Explore the store's data in order to gain insight on possible trends
### hidden in the data and derive data-driven solutions.

library(dplyr)
library(ggplot2)
library(gridExtra)
library(corrplot)

setwd(choose.dir())
dataset <- read.csv('SampleSuperstore.csv')

### Explore the data of the retail store
glimpse(dataset)
View(dataset)

# Count the number of missing data if any
lapply(lapply(dataset, is.na), sum)
cat('There are', count(dataset[duplicated(dataset), ])[1,1], 'duplicate values to be removed. Done.')
dataset <- dataset[!duplicated(dataset), ]



### Which states are most profitable?
profit_by_state <- dataset %>% 
  group_by(State) %>% 
  select(State, Profit) %>% 
  summarise(Profit = sum(Profit)) %>% 
  arrange(desc(Profit))

# Simple Bar Plot comparing the Total profit of the store per state
ggplot(data=profit_by_state, aes(x=State, y=Profit, fill=time)) +
  geom_bar(colour="black", fill="#DD8888", width=.8, stat="identity") +
  guides(fill='none') +
  xlab("States") + ylab("Profit") +
  ggtitle("Total Profit per State") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# Retails in the state of California and New York are the most profitable respectively at around 75k$ per month.
# The state suffering the highest loss is Texas with over 25k$ in losses. 
# This is followed by Ohio, Pennsylvania and Illinois respectively.



### How are the features correlated?
corrplot(cor(dataset[, c(10,11,12,13)]), method = "color", outline = T, cl.pos = 'n', rect.col = "black",  
         tl.col = "indianred4", addCoef.col = "black", 
         number.digits = 2, number.cex = 0.60, tl.cex = 0.7, cl.cex = 1, 
         col = colorRampPalette(c("green4","white","red"))(100))

# The heat map indicates that sales and profits are positively correlated as expected.
# On the other hand, quantity affects sales, but doesn't necessary indicate profit.
# Finally, discounts appear to be negatively correlated to profit.



### Is there a certain threshold before discounts affect profit negatively?
profit_by_discount <- dataset %>% 
  group_by(Discount) %>% 
  select(Discount, Profit) %>% 
  summarise(Profit = sum(Profit)) %>% 
  arrange(Discount)

# Bar Plot comparing the total profit according to the discount given
barplot(profit_by_discount$Profit ~ profit_by_discount$Discount,
        main="Relationship Between Profit and Discount",
        xlab="Discount margin",
        ylab="Profit")

# Overall, offering a discount of over 20% results in losses.
# Hence, high discounts should only be offered in a strategic manner such as attracting customers.
# However, this leads to the question of whether the retails located in states that offer a variety 
# of high discounts, result in a negative net profit for retailers in those states.

table(dataset[dataset$Discount > 0.2,][,5])
# The table clearly shows that Texas offers the most number of discounted prices with over 0.2 profit.
# That is followed closely by Pennsylvania, Illinois and Ohio.
# Interestingly, the mentioned states have also displayed the highest losses as seen previously.



### Which Category has the highest sales? Profits?
sales_by_category <- dataset %>% 
  group_by(Category) %>% 
  select(Category, Sales, Profit) %>% 
  summarise(Sales = sum(Sales), Profit = sum(Profit)) %>% 
  arrange(desc(Sales))

# Simple Bar Plot
ggplot(data=sales_by_category, aes(x=Category, y=Sales, fill=time)) +
  geom_bar(colour="black", fill="#DD8888", width=.8, stat="identity") +
  guides(fill='none') +
  xlab("States") + ylab("Profit") +
  ggtitle("Total Profit per State") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# The technological contributes to the highest amount of profit,
# where as office supplies offer the least amount of profit.
# Thus, further insight on the distribution of the categories is needed in order
# to derive a reasonable conclusion to the reason behind the shape of the current profit distribution.



### Whats the overall discount distribution for each category? Its impact on sales and profit?
unique(dataset$Category)
data_furniture <- dataset[,c(8,10,11,12,13)]
data_furniture$Category <- factor(data_furniture$Category)

# Two scatter plots are illustrated to understand the sales and profit of a category with respect 
# to the discount.

grid.arrange(ggplot(data_furniture, aes(x=Discount, y=Sales, shape=Category, color=Category)) +
  geom_point() + ggtitle('Discount vs Sales for Each Category'),
  ggplot(data_furniture, aes(x=Discount, y=Profit, shape=Category, color=Category)) +
  geom_point() + ggtitle('Discount vs Profit for Each Category'),nrow=2) 

# The number of items in each category 
table(data_furniture$Category)

# At first glance, the technology category has the most and furthest outliers and thus the highest variance.
# Office supplies are the most abundant category and the category with the highest discount and overall losses.
# Furniture have the highest variety of discounts and shows low variance.
# As previously seen, discount above 20% should be used strategically. 
# However, it is clear that office supplies are the items with the highest discount.
# The store should consider limiting the maximum discount up to 60% and compare the performance later on.



### The details should be explored further.
# A bar plot is used to illustrate the quantity difference between the categories.
ggplot(data=dataset, aes(x=Category, y=Quantity, fill=time)) +
  geom_bar(colour="black", fill="#DD8888", width=.8, stat="identity") +
  guides(fill='none') +
  xlab("Category") + ylab("Quantity") +
  ggtitle("Quantity of Items Sold per Category") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
# The office supplies category is by far the most sold item, due to its nature.
# Interestingly, the quantity of furniture sold is slightly higher than that of technology.

# This leaves the question of how discounts ranges affect the overall supply profitability.
dev.off()
Office_sup_df <- data.frame(Quantity_low_Discount = sum(data_furniture$Quantity[data_furniture$Discount<=0.6 & data_furniture$Category=='Office Supplies']),
Profit_low_Discount = sum(data_furniture$Profit[data_furniture$Discount<=0.6 & data_furniture$Category=='Office Supplies']),
Quantity_high_Discount = sum(data_furniture$Quantity[data_furniture$Discount>0.6 & data_furniture$Category=='Office Supplies']),
Profit_high_Discount = sum(data_furniture$Profit[data_furniture$Discount>0.6 & data_furniture$Category=='Office Supplies']))
grid.table(Office_sup_df, rows='')
paste0('The profit from highly discounted supplies is ', round((Office_sup_df[,4]/Office_sup_df[,2])*100,2), 
       '% compared to low discounted ones')



### What are the key takeaways?

# Discounts should be applied strategically and be limited to 60%.
# Consequently, if further discount is necessary for a competitive edge,
# a higher discount based on loyalty points would be feasible. This ensures 
# that customers would be enticed to purchase other more profitable products as well.

# Additionally, retails in non-profitable stats such as Texas, Ohio and so on, are required 
# to lower the total amount of discounts that exceed 50%.

# Finally, office supplies are to be placed further away from the cashier,
# while introducing customers to more technological items.
# This would entice shoppers to buy further
