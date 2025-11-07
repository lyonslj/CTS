library(rvest)
lego_movie <- read_html("http://www.imdb.com/title/tt1490017/")
#to exract the rating we start with selectorgadget to figure out which css selector matches the data we want: strong span.
#html_node() to find the first node that matches that selector, extract its contents with html_text(), and convert it to numeric with as.numeric():
lego_movie %>% 
        html_node(".ratingValue span") %>%
        html_text() %>%
        as.numeric()
#We use a similar process to extract the cast, using html_nodes() to find all nodes that match the selector:
lego_movie %>%
        html_nodes("#titleCast .itemprop span") %>%
        html_text()
#The titles and authors of recent message board postings are stored in a the third table on the page. 
#We can use html_node() and [[ to find it, then coerce it to a data frame with html_table():
lego_movie %>%
        html_nodes("table") %>%
        .[[3]] %>%
        html_table()
