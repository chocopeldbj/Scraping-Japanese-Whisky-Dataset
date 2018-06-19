# packages
library(rvest)
library(tidyverse)

# links
yamazaki.link <- "https://www.masterofmalt.com/distilleries/yamazaki-whisky-distillery/"
hibiki.link <- "https://www.masterofmalt.com/distilleries/hibiki/"
hakushu.link <- "https://www.masterofmalt.com/distilleries/hakushu-whisky-distillery/"
nikka.link1 <- "https://www.masterofmalt.com/distilleries/nikka-branded-whisky/"
nikka.link2 <- "https://www.masterofmalt.com/distilleries/nikka-branded-whisky/2/"

# read html
yamazaki.page <- read_html(yamazaki.link)
hibiki.page <- read_html(hibiki.link)
hakushu.page <- read_html(hakushu.link)
nikka.page1 <- read_html(nikka.link1)
nikka.page2 <- read_html(nikka.link2)

# Bottle list
## Yamazaki
yamazaki.list <- yamazaki.page %>% 
  html_nodes("div.col-md-12") %>% 
  html_nodes("div.boxBgr")

yamazaki.bottle.list <- yamazaki.list[1:25] %>% 
  html_nodes("h3") %>% 
  html_text()

## Hibiki
hibiki.list <- hibiki.page %>% 
  html_nodes("div.col-md-12") %>% 
  html_nodes("div.boxBgr")

hibiki.bottle.list <- hibiki.list[1:10] %>% 
  html_nodes("h3") %>% 
  html_nodes("a") %>% 
  html_text()

## Hakushu
hakushu.list <- hakushu.page %>% 
  html_nodes("div.col-md-12") %>% 
  html_nodes("div.boxBgr")

hakushu.bottle.list <- hakushu.list[1:13] %>% 
  html_nodes("h3") %>% 
  html_nodes("a") %>% 
  html_text()

## Nikka
nikka.list1 <- nikka.page1 %>% 
  html_nodes("div.col-md-12") %>% 
  html_nodes("div.boxBgr")

nikka.bottle.list1 <- nikka.list1[1:25] %>% 
  html_nodes("h3") %>% 
  html_nodes("a") %>% 
  html_text()

nikka.list2 <- nikka.page2 %>% 
  html_nodes("div.col-md-12") %>% 
  html_nodes("div.boxBgr")

nikka.bottle.list2 <- nikka.list2[1:18] %>% 
  html_nodes("h3") %>% 
  html_nodes("a") %>% 
  html_text()


# Link List
## Yamazaki
yamazaki.link.list <- 
  yamazaki.list[1:25] %>% 
  html_nodes("h3") %>% 
  html_nodes("a") %>% 
  html_attr("href")

## Hibiki
hibiki.link.list <- 
  hibiki.list[1:10] %>% 
  html_nodes("h3") %>% 
  html_nodes("a") %>% 
  html_attr("href")

## Hakushu
hakushu.link.list <- 
  hakushu.list[1:13] %>% 
  html_nodes("h3") %>% 
  html_nodes("a") %>% 
  html_attr("href")

## Nikka
nikka.link.list1 <- 
  nikka.list1[1:25] %>% 
  html_nodes("h3") %>% 
  html_nodes("a") %>% 
  html_attr("href")

nikka.link.list2 <-
  nikka.list2[1:18] %>% 
  html_nodes("h3") %>% 
  html_nodes("a") %>% 
  html_attr("href")



# Create data frame
yamazaki.df <- data.frame(yamazaki.bottle.list, yamazaki.link.list)
yamazaki.df$Distillery <- "Yamazaki"

hibiki.df <- data.frame(hibiki.bottle.list, hibiki.link.list)
hibiki.df$Distillery <- "Hibiki"

hakushu.df <- data.frame(hakushu.bottle.list, hakushu.link.list)
hakushu.df$Distillery <- "Hakushu"

nikka1.df <- data.frame(nikka.bottle.list1, nikka.link.list1)
nikka1.df$Distillery <- "Nikka"

nikka2.df <- data.frame(nikka.bottle.list2, nikka.link.list2)
nikka2.df$Distillery <- "Nikka"


names(yamazaki.df) <- c("Name", "Link", "Distillery")
names(hibiki.df) <- c("Name", "Link", "Distillery")
names(hakushu.df) <- c("Name", "Link", "Distillery")
names(nikka1.df) <- c("Name", "Link", "Distillery")
names(nikka2.df) <- c("Name", "Link", "Distillery")

japanese.whisky <- rbind(yamazaki.df, hibiki.df, hakushu.df, nikka1.df, nikka2.df)

japanese.whisky <- 
  japanese.whisky[-c(8:9, 11, 15, 17:18, 20:21, 23:24, 28, 35, 
                     38, 43:46, 48, 59:61, 64:65, 67, 70:72, 76:80, 
                     82,84, 86:87, 89:91), ]

names(japanese.whisky)[3] <- "Brand"

japanese.whisky <- 
  japanese.whisky %>% 
  select(Name, Brand, Link)

japanese.whisky$Link<- as.character(japanese.whisky$Link)

# Create function for review title 
title_scrape <- function (url){
  reviewlist.container <- 
    url %>% 
    read_html() %>% 
    html_nodes("div.col-sm-12") %>% 
    html_nodes("div.col-md-12") %>% 
    html_nodes("div#reviewslist") %>% 
    html_nodes("div.row")
  
  k <- reviewlist.container %>% 
    html_attrs() %>% 
    length()
  
  reviewlist <- reviewlist.container %>% 
    html_nodes("div.userReviewBlock") %>% 
    html_nodes("[itemprop=name]")
  
  reviewlist[1:k] %>% 
    html_text()
}


# Create function for review content 
content_scrape <- function(url){
  reviewlist.container <- 
    url %>% 
    read_html() %>% 
    html_nodes("div.col-sm-12") %>% 
    html_nodes("div.col-md-12") %>% 
    html_nodes("div#reviewslist") %>% 
    html_nodes("div.row")
  
  k <- reviewlist.container %>% 
    html_attrs() %>% 
    length()
  
  review.body.list <- reviewlist.container %>% 
    html_nodes("div.userReviewBlock") %>% 
    html_nodes("[itemprop=reviewBody]")
  
  review.body.list[1:k] %>% 
    html_text()
}

# Create function for creating a dataframe
review.dataframe <- function(df, nth) {
  url <- df$Link[nth]
  review.df <- data.frame(
    title_scrape(url), 
    content_scrape(url)
  )
  names(review.df) <- c("Title", "Review_Content")
  review.df <- 
    review.df %>% 
    mutate(Brand = df$Brand[nth])
  review.df <- 
    review.df %>% 
    mutate(Bottle_name = df$Name[nth])
  review.df
}


# Create for loop
whisky1 <- review.dataframe(japanese.whisky, nth = 1)
whisky2 <- review.dataframe(japanese.whisky, nth = 2)
whisky3 <- review.dataframe(japanese.whisky, nth = 3)
whisky4 <- review.dataframe(japanese.whisky, nth = 4)
whisky5 <- review.dataframe(japanese.whisky, nth = 5)
whisky6 <- review.dataframe(japanese.whisky, nth = 6)
whisky7 <- review.dataframe(japanese.whisky, nth = 7)
whisky8 <- review.dataframe(japanese.whisky, nth = 8)
whisky9 <- review.dataframe(japanese.whisky, nth = 9)
whisky10 <- review.dataframe(japanese.whisky, nth = 10)
whisky11 <- review.dataframe(japanese.whisky, nth = 11)
whisky12 <- review.dataframe(japanese.whisky, nth = 12)
whisky13 <- review.dataframe(japanese.whisky, nth = 13)
whisky14 <- review.dataframe(japanese.whisky, nth = 14)
whisky15 <- review.dataframe(japanese.whisky, nth = 15)
whisky16 <- review.dataframe(japanese.whisky, nth = 16)
whisky17 <- review.dataframe(japanese.whisky, nth = 17)
whisky18 <- review.dataframe(japanese.whisky, nth = 18)
whisky19 <- review.dataframe(japanese.whisky, nth = 19)
whisky20 <- review.dataframe(japanese.whisky, nth = 20)
whisky21 <- review.dataframe(japanese.whisky, nth = 21)
whisky22 <- review.dataframe(japanese.whisky, nth = 22)
whisky23 <- review.dataframe(japanese.whisky, nth = 23)
whisky24 <- review.dataframe(japanese.whisky, nth = 24)
whisky25 <- review.dataframe(japanese.whisky, nth = 25)
whisky26 <- review.dataframe(japanese.whisky, nth = 26)
whisky27 <- review.dataframe(japanese.whisky, nth = 27)
whisky28 <- review.dataframe(japanese.whisky, nth = 28)
whisky29 <- review.dataframe(japanese.whisky, nth = 29)
whisky30 <- review.dataframe(japanese.whisky, nth = 30)
whisky31 <- review.dataframe(japanese.whisky, nth = 31)
whisky32 <- review.dataframe(japanese.whisky, nth = 32)
whisky33 <- review.dataframe(japanese.whisky, nth = 33)
whisky34 <- review.dataframe(japanese.whisky, nth = 34)
whisky35 <- review.dataframe(japanese.whisky, nth = 35)
whisky36 <- review.dataframe(japanese.whisky, nth = 36)
whisky37 <- review.dataframe(japanese.whisky, nth = 37)
whisky38 <- review.dataframe(japanese.whisky, nth = 38)
whisky39 <- review.dataframe(japanese.whisky, nth = 39)
whisky40 <- review.dataframe(japanese.whisky, nth = 40)
whisky41 <- review.dataframe(japanese.whisky, nth = 41)
whisky42 <- review.dataframe(japanese.whisky, nth = 42)
whisky43 <- review.dataframe(japanese.whisky, nth = 43)
whisky44 <- review.dataframe(japanese.whisky, nth = 44)
whisky45 <- review.dataframe(japanese.whisky, nth = 45)
whisky46 <- review.dataframe(japanese.whisky, nth = 46)
whisky47 <- review.dataframe(japanese.whisky, nth = 47)
whisky48 <- review.dataframe(japanese.whisky, nth = 48)
whisky49 <- review.dataframe(japanese.whisky, nth = 49)
whisky50 <- review.dataframe(japanese.whisky, nth = 50)
whisky51 <- review.dataframe(japanese.whisky, nth = 51)
whisky52 <- review.dataframe(japanese.whisky, nth = 52)


# Merge Data frames
japanese.whisky.review <- rbind(whisky1, whisky2, whisky3, whisky4, whisky5, whisky6, whisky7, whisky8, whisky9, whisky10, 
                                whisky11, whisky12, whisky13, whisky14, whisky15, whisky16, whisky17, whisky18, whisky19, whisky20, 
                                whisky21, whisky22, whisky23, whisky24, whisky25, whisky26, whisky27, whisky28, whisky29, whisky30, 
                                whisky31, whisky32, whisky33, whisky34, whisky35, whisky36, whisky37, whisky38, whisky39, whisky40, 
                                whisky41, whisky42, whisky43, whisky44, whisky45, whisky46, whisky47, whisky48, whisky49, whisky50, 
                                whisky51, whisky52)

japanese.whisky.review <- 
  japanese.whisky.review %>% select(Bottle_name, Brand, Title, Review_Content)

# Export Dataset as csv file
write.csv(japanese.whisky.review, file = "japanese_whisky_review.csv")
