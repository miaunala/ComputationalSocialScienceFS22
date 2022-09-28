setwd("C:/Users/rolan/OneDrive - Universität Zürich UZH/Documents")


library(tidyr)
library(plyr)
library(jsonlite)
library(dplyr)
library(stringr)
# read in your API key
NYT_KEY <- readLines(con = "~/NYT_KEY.txt")


# set query parameters
begin_date = "20200201"
end_date = "20200630"
term = "Coronavirus+hoax"

# build the base URL
baseurl <- paste0(
  "http://api.nytimes.com/svc/search/v2/articlesearch.json?q=",term,
  "&begin_date=",begin_date,
  "&end_date=",end_date,
  "&facet_filter=true", # restricts descriptives of the results to the search criteria
  "&api-key=",NYT_KEY, 
  sep="")

baseurl

# first query to get total number of records found
initialQuery <- fromJSON(baseurl)
( maxPages <- ceiling((initialQuery$response$meta$hits[1] / 10)-1) )

# create an empty list fitting results
result_list <- vector("list",length=maxPages)

# cycle through the pages using offset
for(i in 0:maxPages){
  print(i)
  nytSearch <- 
    fromJSON(paste0(baseurl, "&page=", i), flatten = TRUE) %>% 
    data.frame(.,stringsAsFactors = FALSE) 
  result_list[[i+1]] <- nytSearch 
  Sys.sleep(6) #build in sleep to not upset the server / internet connection
}

# combine results
nyt_covid <- rbind_pages(result_list)

save(nyt_covid, file = "nyt_covid0.RData")

library(tibble)
glimpse(nyt_covid) 

covid <-
  nyt_covid %>% 
  select(url = response.docs.web_url, # rename vars on the fly
         text = response.docs.lead_paragraph,
         type = response.docs.type_of_material,
         date = response.docs.pub_date)

str(covid)


table(covid$type)


covid <- 
  covid %>% 
  filter(type == "News") %>% 
  select(-type) # we drop a column we don't need any longer


covid <-
  covid %>% 
  filter(text != "" & text != " ")


covid$text_length <- nchar(covid$text)


covid <-
  covid %>% 
  filter(!text %in% c("", " ","___")) 


head(covid$date)


# get rid of the minutes
covid$date_cut <- 
  gsub("T.*",
       "",
       covid$date) 

covid$month <- 
  covid$date_cut %>% 
  gsub("[0-9]{4}-([0-9]{2}).*","\\1", .) %>% 
  as.numeric


covid$day <- 
  covid$date_cut %>% 
  gsub("[0-9]{4}-[0-9]{2}-([0-9]{2})","\\1", .) %>% 
  as.numeric

# extract city

covid$has.city <- grepl("^[A-Z]{2,20}( -|,)", covid$text)
covid$has.city
covid$city <- ifelse(covid$has.city, gsub("(^[A-Z]{2,20})( -|,).*", "\\1", covid$text), NA)
table(covid$city)
covid$text_wth_city <- ifelse(covid$has.city, gsub("^[A-Z]{2,20}( -|,)(.*)", "\\2", covid$text), covid$text) %>%
  trimws()


library(ggplot2)

covid %>% 
  group_by(month) %>% 
  summarize(n = n()) %>% 
  ggplot(., 
         aes(x = month, 
             y = n)) +
  geom_line(size=1) + 
  scale_x_continuous(name="Month",
                     labels = c("February", "March", "April", "May", "June"),
                     breaks = c(2, 3, 4, 5, 6)) + 
  scale_y_continuous(name="Total") + 
  ggtitle("Number of documents per month") 


#install.packages("quanteda")
library(quanteda)

covid_corpus <- 
  corpus(covid, 
           text_field = "text")

covid_corpus <-
  corpus(select(covid, 
                text, 
                url, 
                month, 
                day))


summary(covid_corpus) %>% 
  head

covid_corpus[1:3]

docvars(covid_corpus) %>% 
  head

docvars(covid_corpus, "added") <- "test"
docvars(covid_corpus) %>% head
docvars(covid_corpus, "added") <- NULL
docvars(covid_corpus) %>% head

??docvars



sum_corp <- 
  summary(covid_corpus, 
          n=ndoc(covid_corpus)) %>% # needed to overwrite default limits
  as.data.frame() %>%
  select(-Text)

sum_corp_grp <- 
  sum_corp %>%
  group_by(month) %>%
  summarise(sum_docs = n()) 


mydfm <- 
  dfm(covid_corpus,
      tolower = FALSE, 
      remove_numbers = FALSE)

#schauen
mode(mydfm)
str(mydfm)


toks <- tokens(covid_corpus,
               what = c("word"),
               remove_separators = TRUE,
               include_docvars = TRUE,
               ngrams = 1L,
               remove_numbers = FALSE, 
               remove_punct = FALSE,
               remove_symbols = FALSE, 
               remove_hyphens = FALSE
)
toks %>% head

# install.packages("quanteda.textstats")
library(quanteda.textstats) 

col3 <- 
  toks %>% 
  tokens_select(pattern = "^[A-Z]", 
                valuetype = "regex", 
                case_insensitive = FALSE,
                padding = TRUE) %>% 
  textstat_collocations(min_count = 5, 
                        size = 3,
                        tolower = FALSE)
head(col3)


toks <- 
  toks %>%
  tokens_remove(pattern = "^[[:punct:]]+$", # regex for toks consisting solely of punct class chars
                valuetype = "regex",
                padding = TRUE) 
toks[1:2]

toks <- 
  toks %>% 
  tokens_tolower

toks[1:2]


stopwords("english")
toks <- 
  toks %>% 
  tokens_remove(stopwords("english"), 
                padding = TRUE) 
toks[1:2]


SnowballC::wordStem(c("corona","cov","hoax","pandemic"))

toks <- 
  toks %>% 
  tokens_wordstem(language = "english")


toks[c(1,5)]


toks <-
  toks %>% 
  tokens_remove("")

toks[c(1,2,5)]

kwic(covid_corpus, "covid-19", 3) %>% head(40)
kwic(toks, "covid-19", 3) %>% head(40)


mydfm <- dfm(toks)

as.matrix(mydfm)[80:80,1:80] 
??as.matrix
#frequency 

library(ggplot2)

topfeatures(mydfm, 50)

top_words <-
  topfeatures(mydfm, 20) %>% 
  data.frame(word = names(.),
             freq = .,
             row.names = c())

freq_tokens <-
  ggplot(top_words, 
         aes(x=reorder(word, freq), y=freq)) +
  geom_col() + 
  coord_flip() +
  theme_minimal() +
  ggtitle("Most frequent tokens") 

freq_tokens

#plot tokens

# install.packages("quanteda.textplots")
library(quanteda.textplots) # since quanteda 3.x

textplot_wordcloud(mydfm, 
                   min_count = 2, 
                   random_order = FALSE,
                   rotation = .3,
                   color = RColorBrewer::brewer.pal(8, "Dark2"))

data_dictionary_LSD2015 %>% 
  glimpse()

lengths(data_dictionary_LSD2015)

data_dictionary_LSD2015@.Data %>% 
  head %>% 
  glimpse


covid_toks_sent <- 
  tokens_lookup(toks, 
                dictionary =  data_dictionary_LSD2015[1:2]) # we only look for negative and positive
head(covid_toks_sent, 5)


mydfm_sent <- dfm(covid_toks_sent)
head(mydfm_sent, 10)

library(dplyr)
mydfm_sent %>% 
  convert(.,to = "data.frame") %>% 
  mutate(pos_to_neg = positive / (positive + negative)) 




mydfm_sent_forplot <- 
  mydfm_sent %>% 
  dfm(., 
      dictionary = data_dictionary_LSD2015[1:2])

mydfm_sent_forplot
mydfm_sent_forplot %>% 
  as.matrix %>% 
  head


mydfm_sent_forplot_group <- 
  mydfm_sent_forplot %>% 
  dfm_group(., 
            group = month, 
            fill = TRUE) 

  # New Syntax for quanteda 3.x
# dfmat_tm_lsd_grp <- 
#   dfmat_tm_lsd %>% 
#   dfm_group(., 
#             groups = mth_sc_16, 
#             fill = TRUE) 

matplot(mydfm_sent_forplot_group, 
        type = 'l', 
        xaxt = 'n',
        lty = 1, 
        ylab = 'Frequency')
grid()
legend('topleft', 
       col = 1:2, 
       legend = c('Negative', 'Positive'), 
       lty = 1, 
       bg = 'white')
axis(1, seq_len(ndoc(mydfm_sent_forplot_group)), 
     lubridate::ymd("2020-02-01") + months(seq_len(ndoc(mydfm_sent_forplot_group)) - 1))
title("Pos/Neg of Coronavirus between 1/2/20 and 30/6/20")

