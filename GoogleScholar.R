library(dplyr)
library(rvest)

URL <- "https://scholar.google.com/citations?user=PhDDPiUAAAAJ&hl=en"
res <- read_html(URL)

pattern <- "#gsc_a_b > tr > td.gsc_a_t > a"
title <- res %>% 
  html_nodes(pattern) %>% 
  html_text()
title

pattern <- "#gsc_a_b > tr > td.gsc_a_c"
citation <- res %>% 
  html_nodes(pattern) %>% 
  html_text()
citation

pattern <- "#gsc_a_b > tr > td.gsc_a_y"
year <- res %>% 
  html_nodes(pattern) %>% 
  html_text()
year

library(stringr)

pattern <- "#gsc_a_b > tr > td.gsc_a_t > a"
link <- res %>% 
  html_nodes(pattern) %>% 
  html_attr("href") %>%
  str_c("https://scholar.google.com", .)
link

tab <- cbind(title, citation, year, link) %>% as_tibble()
tab
tab$link[1]

tibble(tab)

#table 로 한번에 가져오기 

pattern <- "#gsc_a_t"
tab <- res %>% 
  html_table() %>%
  .[[2]]
tab

str(tab)
dim(tab)
view(tab)

#table head가 데이터로 받아졌기 때문에 20줄이 아닌 21줄.. 
names(tab) <- c("title", "citation", "year")
view(tab)
tab <- tab %>% slice(-1)
view(tab)

### TedTalks ###

URL <- "https://www.ted.com/talks"
res <- read_html(URL)

pattern <- "#browse-results > div.row.row-sm-4up.row-lg-6up.row-skinny > div:nth-child(1) > div > div > div > div.media__message > h4.h12.talk-link__speaker"
speaker <- res %>% 
  html_nodes(pattern) %>% 
  html_text()
speaker

# '\'가 있는 경우에는 이걸 두개로 붙여줘야 한다. 
pattern <- ".m5 .ga-link"
title <- res %>% 
  html_nodes(pattern) %>% 
  html_text()
title

pattern <- ".meta__val"
date <- res %>% 
  html_nodes(pattern) %>% 
  html_text()
date

pattern <- ".thumb__duration"
time <- res %>% 
  html_nodes(pattern) %>% 
  html_text()
time

library(stringr)

pattern <- ".m5 .ga-link"
link <- res %>% 
  html_nodes(pattern) %>% 
  html_attr("href")
link


for (i in 1:5) {
  URL <- str_c("https://www.ted.com/talks?page=",i)
  print()
}


