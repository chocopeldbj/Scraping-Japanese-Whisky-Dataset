# packages
library(rvest)
library(tidyverse)
library(pforeach)

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

# Create data frame
yamazaki.df <- data.frame(yamazaki.bottle.list)
yamazaki.df$Distillery <- "Yamazaki"

hibiki.df <- data.frame(hibiki.bottle.list)
hibiki.df$Distillery <- "Hibiki"

hakushu.df <- data.frame(hakushu.bottle.list)
hakushu.df$Distillery <- "Hakushu"

nikka1.df <- data.frame(nikka.bottle.list1)
nikka1.df$Distillery <- "Nikka"

nikka2.df <- data.frame(nikka.bottle.list2)
nikka2.df$Distillery <- "Nikka"

names(yamazaki.df) <- c("Name", "Distillery")
names(hibiki.df) <- c("Name", "Distillery")
names(hakushu.df) <- c("Name", "Distillery")
names(nikka1.df) <- c("Name", "Distillery")
names(nikka2.df) <- c("Name", "Distillery")

japanese.whisky <- rbind(yamazaki.df, hibiki.df, hakushu.df, nikka1.df, nikka2.df)

# Create Link

japanese.whisky$Link <- 
  japanese.whisky$Name %>% 
  str_to_lower()

japanese.whisky$Link <- 
  japanese.whisky$Link %>% 
  str_replace(pattern = "%", replacement = "")

japanese.whisky$Link <- japanese.whisky$Link %>% 
  str_replace(pattern = "\\(", replacement = "")
japanese.whisky$Link <- japanese.whisky$Link %>% 
  str_replace(pattern = "\\)", replacement = "")
japanese.whisky$Link <- japanese.whisky$Link %>% 
  str_replace_all(pattern = "\\’", replacement = "")
japanese.whisky$Link <- japanese.whisky$Link %>% 
  str_replace_all(pattern = "\\- ", replacement = "")
japanese.whisky$Link <- japanese.whisky$Link %>% 
  str_replace_all(pattern = "\\.", replacement = "-")
japanese.whisky$Link <- japanese.whisky$Link %>% 
  str_replace_all(pattern = " ", replacement = "-")

japanese.whisky$Link <- 
  paste0("https://www.masterofmalt.com/whiskies/", 
         str_to_lower(japanese.whisky$Distillery), "/",
         japanese.whisky$Link, "-whisky/")

japanese.whisky$Link <- japanese.whisky$Link %>% 
  str_replace(pattern = "--", replacement = "-")


japanese.whisky <- 
  japanese.whisky[-c(8:9, 11, 15, 17:18, 20:21, 23:24, 28, 35, 
                     38, 43:46, 48, 59:61, 64:65, 67, 70:72, 76:80, 
                     82,84, 86:87, 89:91), ]

names(japanese.whisky)[2] <- "Brand"


# Create loop function
row_num <- japanese.whisky %>% nrow()

review.parent.container <- paste0("reviewlist", 1:row_num)


pforeach (i = 1:row_num){
  review.parent.container[i] <- 
    japanese.whisky$Link[i] %>% 
    read_html() %>% 
    html_nodes("div.col-sm-12") %>% 
    html_nodes("div.col-md-12") %>% 
    html_nodes("div#reviewslist") %>% 
    html_nodes("div.row")
}





## Scraping
reviewlist1 <- japanese.whisky$Link[1] %>% 
  read_html() %>% 
  html_nodes("div.col-sm-12") %>% 
  html_nodes("div.col-md-12") %>% 
  html_nodes("div#reviewslist") %>% 
  html_nodes("div.row")

k <- reviewlist1 %>% 
  html_attrs() %>% 
  length()

## Title
review.title <- reviewlist1 %>% 
  html_nodes("div.userReviewBlock") %>% 
  html_nodes("[itemprop=name]")

titles1 <- review.title[1:k] %>% 
  html_text()

## Content
review.body1 <- reviewlist1 %>% 
  html_nodes("div.userReviewBlock") %>% 
  html_nodes("[itemprop=reviewBody]")

review.content1 <- review.body1[1:k] %>% 
  html_text()












