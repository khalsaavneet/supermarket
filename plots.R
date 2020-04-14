install.packages('tinytex')
tinytex::install_tinytex()
library("ggplot2")
library('dplyr')

#Dataset for supermarket inventory
data<-read.csv("/home/khalsa/Desktop/Supermarket/Dataset.csv")
#smallData<-read.csv("/home/khalsa/Desktop/temporary.csv")

#Cleansing data 
cleanedData <- data %>% filter((!is.na(Quantity) & Quantity != "" & Quantity>0) & 
(!is.na(UnitPrice) & UnitPrice != "" & UnitPrice>0) & (!is.na(InvoiceNo) & InvoiceNo != "")
& (!is.na(StockCode) & StockCode != ""))

#finding sales by quantity* unitprice
salesData <- cleanedData %>% mutate(Sales = Quantity * UnitPrice)

#group data by 'StockCode'
data2 <- cleanedData %>% group_by(StockCode) %>% summarise(Quantity = sum(Quantity))

H <- data2 %>% pull(Quantity)
M <- data2 %>% pull(StockCode)
# Give the chart file a name
png(file = "barchat_total_sale.png")

# Plot the bar chart 
barplot(H,names.arg=M,xlab="Stockcode",ylab="Quantity",col="blue",
        main="Sale of a product chart",border="red")

#group data by 'Country'
data3 <- salesData %>% group_by(Country)  %>% summarise(Sales = sum(Sales)) 
data3 <- salesData %>% group_by(Country) %>% filter(Country!="United Kingdom") %>% summarise(Sales = sum(Sales))

H1 <- data3 %>% pull(Sales)
M1 <- data3 %>% pull(Country)
# Give the chart file a name
png(file = "country_vs_sale.png")

# Plot the bar chart 
barplot(H1,names.arg=M1,xlab="Country",ylab="Sales",col="blue",
        main="country wise sale",border="red")

#group data by 'CustomerID'
data4 <- salesData %>% group_by(CustomerID) %>% summarise(Sales = sum(Sales)) %>% filter(Sales<7000)

H2 <- data4 %>% pull(Sales)
M2 <- data4 %>% pull(CustomerID)
# Give the chart file a name
png(file = "Customer_wise_sale.png")

# Plot the bar chart 
barplot(H2,names.arg=M2,xlab="CustomerID",ylab="Sales",col="red",
        main="CustomerID vs Sale",border="blue")


#group data by 'Country'
data5<- cleanedData  %>% group_by(Country, StockCode)  %>% summarise(NewQuantity = sum(Quantity)) 
png(file = "testLinechart.png")
ggplot(data=data5, aes(x=StockCode, y=NewQuantity, group=Country)) +geom_line(aes(linetype=Country))+ geom_point()


#correlation
f<-salesData %>% group_by(StockCode) %>% 
  summarise(cor = cor(UnitPrice, Sales, use="everything", method="pearson"))

#Predictive Analysis: Multiple Linear Regression
model<-lm(formula = Sales~Quantity+UnitPrice, data = salesData)
a <- coef(model)[1]
print(a)

XQuantity <- coef(model)[2]
XUnitPrice  <- coef(model)[3]

#MULTIPLE LINEAR REGESSION EQUATION FORMED
#y=a+XQuantity*x1+XUnitPrice*x2 where x1 and x2 are arbitrary values


# Save the file
dev.off()