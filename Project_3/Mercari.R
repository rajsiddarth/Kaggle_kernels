rm(list=ls(all=T))

setwd("C:/Users/sid/Desktop/Data_Science/Github_Codes/Kaggle_kernels/Project_3")

#Reading data
train=read.csv("train.tsv",sep = "\t")
test=read.csv("test.tsv",sep="\t")

#Exploration

library(tibble)
train=as_tibble(train)
test=as_tibble(test)

str(train)
str(test)

#Removing train_id and test_id
library(dplyr)
train_data=select(train,-train_id)
test_data=select(test,-test_id)

#Checking for NA's
sum(is.na(train_data))
sum(is.na(test_data))

#But NA's are mentioned as blanks in train and test
#First separating out blanks and filling them with NA's

train_data[train_data==" "]=NA
train_data[train_data==""]=NA
test_data[test_data==" "]=NA
test_data[test_data==""]=NA

train_nas=sapply(train_data,function(x){sum(is.na(x))})
test_nas=sapply(test_data, function(x){sum(is.na(x))})
train_nas
test_nas



#NA's belong to category names and Brand names

#Separating out the target price variable and combining train and test

price=select(train_data,price)

train_Data=select(train_data,-price)

all_data=rbind(train_Data,test_data)

rm(train,test)

all_nas=sapply(all_data, function(x){sum(is.na(x))})
all_nas

#Almost half of the products do not have brand names.It may be because they are
#hand made items
#Category_name has sub categories included separated by " /" .
#We can separate out the sub categories to analyze the distribution of products 
#among categories

str(all_data)
all_data$category_name=as.character(all_data$category_name)

library(tidyr)

all_data=all_data%>%separate(category_name,into=c("category1","category2",
                "category3","category4"),sep="/")

# Finding number of categories
sapply(all_data[3:6],function(x){length(unique(x))})

#11 unique category 1 and 114 unique category 2

#Mapping category 1 and category 2 together

options(repr.plot.width=7, repr.plot.height=7)
library(ggplot2)
library(treemapify)
temp=all_data %>%
    group_by(category1, category2) %>%
    count() %>%
    ungroup() 
str(temp)

temp%>%ggplot(aes(area=n,fill=category1,label=category2))+
    geom_treemap()+
        geom_treemap_text(colour = "white", place = "topleft", reflow = T)
    theme(legend.position = "null")+
    ggtitle("1st and 2nd Hierarchical Category Levels")
    
#Delving deeper into categories
    
#For 1st level categories    
all_data%>%group_by(category1)%>%summarise(count=n())%>%
    ggplot(aes(x=reorder(category1,-count),y=count))+
    geom_bar(stat="identity")+
    ggtitle("1st level categories")+xlab("Categories")+ylab("count")+
    theme(axis.text.x=element_text(angle=45, hjust=1))

#For second level categories
#Since there are aBOUT 114 Category 2 we will consider categories with count 
#greate than 20000
all_data%>%group_by(category2)%>%summarise(count=n())%>%
    filter(count>20000)%>%
    ggplot(aes(x=reorder(category2,-count),y=count))+geom_bar(stat="identity")+
    ggtitle("2nd level categories")+xlab("Categories")+ylab("count")+
    theme(axis.text.x=element_text(angle=45, hjust=1))

#Similarly for 3rd and 4th level categories
#greater than 10000
all_data%>%group_by(category3)%>%summarise(count=n())%>%
    filter(count>10000)%>%
    ggplot(aes(x=reorder(category3,-count),y=count))+geom_bar(stat="identity")+
    ggtitle("3rd level categories")+xlab("Categories")+ylab("count")+
    theme(axis.text.x=element_text(angle=45, hjust=1))

all_data%>%group_by(category4)%>%summarise(count=n())%>%
    filter(!is.na(category4))%>%
    ggplot(aes(x=reorder(category4,-count),y=count))+geom_bar(stat="identity")+
    ggtitle("4th level categories")+xlab("Categories")+ylab("count")+
    theme(axis.text.x=element_text(angle=45, hjust=1))

#From category 1 plot, it is evident that women primary category are sold
#the most,We further check for sub categories 2nd level in women

all_data%>%filter(category1=="Women")%>%group_by(category2)%>%
    summarise(count=n())%>%
    ggplot(aes(x=reorder(category2,-count),y=count))+geom_bar(stat="identity")+
    ggtitle("2nd level categories in woman")+xlab("Categories")+ylab("count")+
    theme(axis.text.x=element_text(angle=45, hjust=1))
#We can now observe the top selling categories in women.
#We do the similar analysis in Beauty and kids category

all_data%>%filter(category1=="Beauty")%>%group_by(category2)%>%
    summarise(count=n())%>%
    ggplot(aes(x=reorder(category2,-count),y=count,fill=category2))+geom_bar(stat="identity")+
    ggtitle("2nd level categories in Beauty")+xlab("Categories")+ylab("count")+
    theme(axis.text.x=element_text(angle=45, hjust=1))+theme(legend.position = "null")

all_data%>%filter(category1=="Kids")%>%group_by(category2)%>%
    summarise(count=n())%>%
    ggplot(aes(x=reorder(category2,-count),y=count,fill=category2))+geom_bar(stat="identity")+
    ggtitle("2nd level categories in Kids")+xlab("Categories")+ylab("count")+
    theme(axis.text.x=element_text(angle=45, hjust=1))+theme(legend.position = "null")

#We ll plot a stacked bar chart to show  primary categories and the 
#items with and without brands in them

all_data%>%mutate(brand=as.factor(ifelse(is.na(brand_name),"No Brand","Has Brand")))%>%
    ggplot(aes(x=category1,fill=brand))+geom_bar(position = "fill")+
    theme(axis.text.x=element_text(angle=45, hjust=1))+
    ggtitle("Brands in categories")

#As expected Handmade items  doesnt have brands
#We ll check for top 10 brands and their categories except NA's

top_brands=all_data%>%filter(!is.na(brand_name))%>%group_by(brand_name)%>%
    summarise(count=n())%>%arrange(desc(count))%>%
    top_n(n=10)

all_data%>% filter(brand_name %in% top_brands$brand_name) %>%
    ggplot(aes(x=factor(brand_name, levels=rev(top_brands$brand_name)), fill=category1)) +
    geom_bar(width=0.5) +
    coord_flip() +
    xlab('brand') +
    labs(fill='1st Category') +
    ggtitle('Top 10 Brands and Their Categories')   

#ggplot(aes(x=reorder(brand_name,-count),y=count,fill=category1))

#Item condition id vs price
unique(all_data$item_condition_id)
all_data$item_condition_id=as.factor(all_data$item_condition_id)


all_data%>%group_by(item_condition_id)%>%summarise(count=n())%>%
    ggplot(aes(x=reorder(item_condition_id,-count),y=count,fill=item_condition_id))+
    geom_bar(stat="identity")+
    theme(legend.position = "null")+
    ggtitle("Items per item condition")+xlab("item_condition_id")

train_data$item_condition_id=as.factor(train_data$item_condition_id)

train_data%>%ggplot(aes(x=item_condition_id,y=price))+
    geom_boxplot()+
    labs(title="Conditon id vs price")+
    xlab("Item_conditon_id")
#Many outliers.Lets just take log of price+1

logprice=train_data%>%mutate(logprice=log1p(price))
logprice$item_condition_id=as.factor(logprice$item_condition_id)
logprice%>%ggplot(aes(x=item_condition_id,y=logprice,fill=item_condition_id))+
    geom_boxplot()+
    labs(title="Conditon id vs logprice")+
    theme(legend.position = "null")+
    xlab("Item_conditon_id")

#Which brand has the highest price?
temp=train_data%>%filter(!is.na(brand_name))%>%group_by(brand_name)%>%
    summarise(count=n())%>%arrange(desc(count))%>%top_n(20)

train_data%>%filter(brand_name%in%temp$brand_name)%>%group_by(brand_name)%>%
    summarise(avgprice=mean(price))%>%
    ggplot(aes(x=reorder(brand_name,avgprice),y=avgprice))+
    geom_point(color="green")+
    scale_y_continuous(labels = scales::dollar) +
    coord_flip()+
    labs(x = '', y = 'Average price', title = 'Top 20 most expensive brands') 

train_data%>%filter(brand_name%in%temp$brand_name)%>%group_by(brand_name)%>%
    summarise(medianprice=median(price))%>%
    ggplot(aes(x=reorder(brand_name,medianprice),y=medianprice))+
    geom_point(color="red")+
    scale_y_continuous(labels = scales::dollar) +
    coord_flip()+
    labs(x = '', y = 'Median price', title = 'Top 20 most expensive brands') 
#We need to set a filter on the number of items to find the important brands
#I took top 20 

#Price vs category
length(unique(train_data$category_name))
str(train_data$category_name)
train_data%>%filter(!is.na(category_name))%>%group_by(category_name)%>%
    summarise(count=n())%>%top_n(20)%>%
    ggplot(aes(x=reorder(category_name,-count),y=count))+geom_bar(stat="identity")+
    coord_flip()+labs(x="Category Name",y="Number of items")+
    title("Category Name vs number of items")

train_data%>%filter(!is.na(category_name))%>%group_by(category_name)%>%
    summarise(avgprice=mean(price))%>%top_n(20)%>%
    ggplot(aes(x=reorder(category_name,-avgprice),y=avgprice))+geom_point()+
    coord_flip()+labs(x="Category Name",y="Avg Price")+
    title("Category Name vs Avg price")

temp=train_data%>%filter(!is.na(category_name))%>%group_by(category_name)%>%
    summarise(count=n())%>%top_n(20)

train_data%>%filter(category_name%in%temp$category_name)%>%
    ggplot(aes(x=category_name,y=log1p(price)))+geom_boxplot()+
    coord_flip()+labs(x="Category Name",y="Log Price")+
    title("Category Name vs Log price")

#Further analysis on price can be done for each primary category,second level
#categories or brands

#Analysis on shipping conditions
str(train_data$shipping)
#shipping is 1 when paid by seller/ 0 otherwise
train_data$shipping=as.factor(train_data$shipping)
all_data$shipping=as.factor(all_data$shipping)
table(all_data$shipping)

train_data%>%ggplot(aes(x=log1p(price),fill=shipping))+
    geom_density(alpha = 0.7)+
    title("Distribution of price vs Shipping")

#We can see that when shipping is paid by the seller the cost is lower

#Now we get into descriptions

#First we find the most common descripton single length words
#Second we find the most common descripton two length words
#Similarly for 3 length words
#Check is there a relationship between description length and price
#Items which do not have a descripton are mentioned as No description yet

str(train_data$item_description)
train_data$item_description=as.character(train_data$item_description)
train_data$item_description[1]

#Removing spaces
#Length of characters
#Whenever there is no description replace nchar with NA
train_data$description_length=sub("No description yet",NA,train_data$item_description)
train_data$description_length=gsub(" ","",train_data$description_length)
train_data$description_length=nchar(train_data$description_length)

#Correlation between length and price
cor(train_data$description_length,train_data$price, use = 'complete.obs')

#No correlaton is observed

#We use tidytext package for qunatitative text analysis

library(tidytext)

train_data$item_description=gsub("No description yet",NA,train_data$item_description)

#Single word
oneword_frequency=train_data["item_description"]%>%
    unnest_tokens(word,item_description)%>%group_by(word)%>%
    summarise(count=n())%>%arrange(desc(count))


head(oneword_frequency,10)

#We need to remove stop words,numbers and NA's
data("stop_words")
head(stop_words,10)
library(stringr)

oneword_frequency=oneword_frequency%>%anti_join(stop_words,by="word")%>%
       filter(!is.na(word))%>%filter(str_detect(word,"[a-z]"))

library(wordcloud)
oneword_frequency %>%filter(count>10000)%>%
    with(wordcloud(word,count,max.words = 100,rot.per = 0.35,
                   colors=brewer.pal(8,"Dark2")))

rm(train_nas,test_nas,all_nas,temp,twoword_frequency)
#Similarly for two gram words
# twoword_frequency=train_data["item_description"]%>%
#     filter(is.na(item_description))%>%
#     unnest_tokens(bigram,item_description,token="ngrams",n=2)
# 
# temp=austen_books()
# austen_bigrams=unnest_tokens(temp,bigram, text, token = "ngrams", n = 2)
# 

