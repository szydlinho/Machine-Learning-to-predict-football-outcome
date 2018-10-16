library(RSelenium)
library(rvest)
url <- paste0("https://www.transfermarkt.com/laliga/startseite/wettbewerb/ES1/plus/?saison_id=20", 10:18)

for (i in 1:length(url)){
  
  rD <- rsDriver()
  remDr <- rD[["client"]]
  
  remDr$navigate(url[i])
  web <- read_html(remDr$getPageSource()[[1]])
  Sys.sleep(10)
  
  css2 <- ".even .hide-for-pad , .odd .hide-for-pad , .show-for-pad+ .zentriert , .show-for-pad+ .zentriert a , .hide-for-pad .tooltipstered"
  table <- html_nodes(web, css2)
  table <- html_text(table)
  table <- table[table!=""]
  table <- as.data.frame(matrix(table, ncol = 8, byrow = TRUE))
  table$V9 <- as.numeric(substr(url[i], nchar(url[i])-3, nchar(url[i])))
  
  if (i == 1){
    final <- table
  }
  
  final <- rbind(final, table)
  
  Sys.sleep(1)
  
}


final <- read.csv("C:/Users/pszydlik/Desktop/p/budget_la_liga.csv")
final <- final[,c(2,6,7,8,9,10)]
names(final) <- c("Club", "Age", "Foreign", "Total_value", "Market_value", "Sezon")

final$Club <- as.character(final$Club)
final$Age <- gsub(",", ".", final$Age)
final$Age <- as.numeric(final$Age)
final$Foreign <- as.numeric(final$Foreign)
final$Total_value <- gsub(",", ".", final$Total_value)
final$Market_value <- gsub(",", ".", final$Market_value)

final$Total_value_a <-  gsub("Mill. €", "", final$Total_value)
final$Market_value_a <-  gsub("Mill. €", "", final$Market_value)
final$Market_value_a <-  ifelse(grepl(pattern = "Th.", final$Market_value_a), 
                                paste0("0.", substr(final$Market_value, 1, 3)), 
                                final$Market_value_a)
final$Total_value_a <-  ifelse(grepl(pattern = "Bill.", final$Total_value_a), 
                                as.numeric(substr(final$Total_value_a, 1, 3))*1000, 
                                final$Total_value_a)
final$Total_value_a <- as.numeric(final$Total_value_a)

final$Market_value_a <- as.numeric(final$Market_value_a)
final <- final[,c(1,2,3,6,7,8)]
names(final) <- c("Club", "Age", "Foreign", "Sezon", "Total_value", "Market_value")

write.csv(final, "C:/Users/pszydlik/Desktop/p/budget_la_liga_final.csv")



final_draw <- final[,c(1,4,5)]
clubs_2018 <- final$Club[final$Sezon==2018]
final_draw <- final[final$Club %in% clubs_2018,]
final_draw %>% ggplot() + geom_line(aes(x=Sezon, y=Total_value, colour = Club) ) + geom_point(aes(x=Sezon, y=Total_value, colour = Club) ) +
  theme(axis.text.x = element_text(angle = 30, vjust=0.5, size = 8),  # rotate x axis text
        panel.grid.minor = element_blank()) 

