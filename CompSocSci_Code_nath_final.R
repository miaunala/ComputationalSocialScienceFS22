# Guardian API

#install packages

install.packages("guardianapi")
install.packages("usethis")
install.packages("devtools")
devtools::install_github("evanodell/guardianapi")
# sometimes necessary
#force = TRUE

library(guardianapi) 
library(usethis)
library(devtools)



# API KEY
  
gu_api_key(check_env = TRUE)



# hoax scraping

coronavirus_hoax <- gu_content(query = "coronavirus+hoax", from_date = '2020-02-01', to_date = '2020-06-30')
save(coronavirus_hoax, file = "coronavirus_hoax.RData")



# Plotting news

# installing & loading packages

install.packages("ggplot2")
library(ggplot2)
install.packages("dplyr")
library(dplyr)


# Plots

# Coronavirus Hoax
coronavirus_hoax %>%
  mutate(publish_date = as.POSIXct(coronavirus_hoax$web_publication_date,
                                   format = ,'%Y-%m-%d')) %>%
  ggplot(aes(x=publish_date))+
  geom_histogram(fill="#003A3E")+
  theme_minimal()+
  xlab("Months")+ylab("Nr. published articles")+theme(axis.text.x=element_text(angle=45))+
  labs(caption = "Source: The Guardian",
       title = "Published Articles 'Coronavirus Hoax' (1.2.20-30.6.20)") +
  theme(panel.border = element_blank(),
        legend.position = "bottom") +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.border = element_blank(),
        axis.ticks = element_blank(),
        text = element_text(family = "Times", size = 10.00),
        plot.title = element_text(hjust = 0.5, size = 10.00, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10.00))



#Packages for cleaning
library(tidyr)
library(plyr)
library(jsonlite)
library(dplyr)
library(stringr)
library(tibble)


glimpse(coronavirus_hoax) 

#structuring scraped results

coronavirus <-
  coronavirus_hoax %>%
  select(url = web_url,
         text = body_text,
         type = pillar_name,
         date = first_publication_date)

str(coronavirus)

table(coronavirus$type) 

#filtering 

coronavirus <-
  coronavirus %>%
  filter(type == "News") %>%
  select(-type)

coronavirus <- 
  coronavirus %>%
  filter(text != "" & text != " ")

coronavirus$text_length <- nchar(coronavirus$text)

coronavirus <-
  coronavirus %>%
  filter(!text %in% c("", " ", "___"))

head(coronavirus$date)

coronavirus$date_cut <-
  gsub(" .*",
       "",
       coronavirus$date)

coronavirus$month <- 
  coronavirus$date_cut %>% 
  gsub("[0-9]{4}-([0-9]{2}).*","\\1", .) %>% 
  as.numeric

coronavirus <-
  coronavirus %>%
  filter(month != 1)

coronavirus$day <- 
  coronavirus$date_cut %>% 
  gsub("[0-9]{4}-[0-9]{2}-([0-9]{2})","\\1", .) %>% 
  as.numeric


install.packages("ggplot2")
library(ggplot2)


#plotting 

coronavirus %>% 
  group_by(month) %>% 
  dplyr::summarize(n = n()) %>% 
  ggplot(., 
         aes(x = month, 
             y = n)) +
  geom_line(size=1) + 
  scale_x_continuous(name="Month",
                     labels = c("February", "March", "April", "May", "June"),
                     breaks = c(2, 3, 4, 5, 6)) + 
  scale_y_continuous(name="Total") + 
  ggtitle("Number of documents per month") 

# text analysis with quanteda

install.packages("quanteda")
library(quanteda)

coronavirus_corpus <- 
  corpus(coronavirus, 
         text_field = "text")

coronavirus_corpus <-
  corpus(select(coronavirus, 
                text, 
                url, 
                month, 
                day))

summary(coronavirus_corpus) %>% 
  head

coronavirus_corpus[1:3]

docvars(coronavirus_corpus) %>% 
  head

#testing output(will not result in any differences in our corpus)
docvars(coronavirus_corpus, "added") <- "test"
docvars(coronavirus_corpus) %>% head
docvars(coronavirus_corpus, "added") <- NULL
docvars(coronavirus_corpus) %>% head

# saving information in our corpus and transform it into a dfm

sum_corp_nath <- 
  summary(coronavirus_corpus, 
          n=ndoc(coronavirus_corpus)) %>% # needed to overwrite default limits
  as.data.frame() %>%
  select(-Text)

sum_corp_grp <- 
  sum_corp_nath %>%
  group_by(month) %>%
  dplyr::summarise(sum_docs = n()) 

mydfm_nath <- 
  dfm(coronavirus_corpus,
      tolower = FALSE, 
      remove_numbers = FALSE)

#checking output
mode(mydfm_nath)
str(mydfm_nath)

#creating tokens

toks_nath <- tokens(coronavirus_corpus,
               what = c("word"),
               remove_separators = TRUE,
               include_docvars = TRUE,
               ngrams = 1L,
               remove_numbers = FALSE, 
               remove_punct = FALSE,
               remove_symbols = FALSE, 
               remove_hyphens = FALSE
)

toks_nath %>% head

# textual statistics with quanteda textstats

install.packages("quanteda.textstats")
library(quanteda.textstats) 


col_nath <- 
  toks_nath %>% 
  tokens_select(pattern = "^[A-Z]", 
                valuetype = "regex", 
                case_insensitive = FALSE,
                padding = TRUE) %>% 
  textstat_collocations(min_count = 5, 
                        size = 3,
                        tolower = FALSE)

head(col_nath)

# cleaning tokens

toks_nath <- 
  toks_nath %>%
  tokens_remove(pattern = "^[[:punct:]]+$", # regex for toks consisting solely of punct class chars
                valuetype = "regex",
                padding = TRUE) 

toks_nath[1:2]

toks_nath <- 
  toks_nath %>% 
  tokens_tolower

toks_nath[1:2]

# identifying and removing stopwords

stopwords("english")

toks_nath <- 
  toks_nath %>% 
  tokens_remove(stopwords("english"),
                padding = TRUE) 
toks_nath[1:2]

# analysing the word stem of different contextual words

SnowballC::wordStem(c("coronavirus","cov","hoax","pandemic"))

toks_nath <- 
  toks_nath %>% 
  tokens_wordstem(language = "english")

toks_nath[c(1,5)]

toks_nath <-
  toks_nath %>% 
  tokens_remove("")

toks_nath[c(1,2,5)]

# keywords in context surrounding coronavirus

kwic(coronavirus_corpus, "coronavirus", 3) %>% head(40)
kwic(toks_nath, "coronavirus", 3) %>% head(40)


dfm_nath <- dfm(toks_nath)
dfmtrimmed_nath <- dfm_trim(dfm_nath, min_docfreq = 10, min_termfreq = 20, verbose = TRUE)
as.matrix(dfmtrimmed_nath)[1:100,1:15] 


#using topfeatures for most frequent tokens and plotting them

topfeatures(dfmtrimmed_nath, 1000)

top_words_nath <-
  topfeatures(dfmtrimmed_nath, 20) %>% 
  data.frame(word = names(.),
             freq = .,
             row.names = c())

freq_tokens_nath <-
  ggplot(top_words_nath, 
         aes(x=reorder(word, freq), y=freq)) +
  geom_col() + 
  coord_flip() +
  theme_minimal() +
  ggtitle("Most frequent tokens") 

freq_tokens_nath

#plot tokens

install.packages("quanteda.textplots")
library(quanteda.textplots) # since quanteda 3.x

textplot_wordcloud(dfmtrimmed_nath, 
                   min_count = 2, 
                   random_order = FALSE,
                   rotation = .3,
                   color = RColorBrewer::brewer.pal(8, "Dark2"))

# comparing sentiment (positive and negative) words to our data and creating a sentiment analysis

data_dictionary_LSD2015 %>% 
  glimpse()

lengths(data_dictionary_LSD2015)

data_dictionary_LSD2015@.Data %>% 
  head %>% 
  glimpse


covid_toks_sent_nath <- 
  tokens_lookup(toks_nath, 
                dictionary =  data_dictionary_LSD2015[1:2]) # we only look for negative and positive
head(covid_toks_sent_nath, 5)


mydfm_sent_nath <- dfm(covid_toks_sent_nath)
head(mydfm_sent_nath, 10)


library(dplyr)
mydfm_sent_nath %>% 
  convert(.,to = "data.frame") %>% 
  mutate(pos_to_neg = positive / (positive + negative)) 

mydfm_sent_forplot_nath <- 
  mydfm_sent_nath %>% 
  dfm(., 
      dictionary = data_dictionary_LSD2015[1:2])

mydfm_sent_forplot_nath
mydfm_sent_forplot_nath %>% 
  as.matrix %>% 
  head


mydfm_sent_forplot_group_nath <- 
  mydfm_sent_forplot_nath %>% 
  dfm_group(., 
            group = month, 
            fill = TRUE) 

#plotting positive and negative sentiments

matplot(mydfm_sent_forplot_group_nath, 
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
axis(1, seq_len(ndoc(mydfm_sent_forplot_group_nath)), 
     lubridate::ymd("2020-02-01") + months(seq_len(ndoc(mydfm_sent_forplot_group_nath)) - 1))
title("Pos/Neg of Coronavirus between 1/2/20 and 30/6/20")


