Twitter Analysis
================

## Loading in Data and functions to help

``` r
load("C:/Users/Bryce/Desktop/RStudio/DSCI 415/Wendys.Rdata")
load("C:/Users/Bryce/Desktop/RStudio/DSCI 415/TextFunctions.Rdata")
```

## Libraries Needed

``` r
library(twitteR)
library(rtweet)
```

    ## 
    ## Attaching package: 'rtweet'

    ## The following object is masked from 'package:twitteR':
    ## 
    ##     lookup_statuses

``` r
library(lubridate)
```

    ## 
    ## Attaching package: 'lubridate'

    ## The following objects are masked from 'package:base':
    ## 
    ##     date, intersect, setdiff, union

``` r
library(stringr)
library(wordcloud)
```

    ## Loading required package: RColorBrewer

``` r
library(tm)
```

    ## Loading required package: NLP

``` r
library(arules)
```

    ## Loading required package: Matrix

    ## 
    ## Attaching package: 'arules'

    ## The following object is masked from 'package:tm':
    ## 
    ##     inspect

    ## The following objects are masked from 'package:base':
    ## 
    ##     abbreviate, write

``` r
library(arulesViz)
```

    ## Loading required package: grid

## Looking at data and picking what I want to minimize computing needed

``` r
Wendys.df = Wendys[,c(1,3,4,5,7,13,14,78,79,82)]
Wendys.df
```

    ## # A tibble: 3,199 x 10
    ##    user_id created_at          screen_name text  display_text_wi~ favorite_count
    ##    <chr>   <dttm>              <chr>       <chr>            <dbl>          <int>
    ##  1 595535~ 2019-11-19 19:25:47 Wendys      "@th~              112              2
    ##  2 595535~ 2019-11-19 18:54:57 Wendys      "@ja~               87              1
    ##  3 595535~ 2019-11-19 18:47:07 Wendys      "@Cy~              110              0
    ##  4 595535~ 2019-11-19 18:39:35 Wendys      "@No~              211              0
    ##  5 595535~ 2019-11-19 18:32:33 Wendys      "@Lb~               53             12
    ##  6 595535~ 2019-11-19 18:28:53 Wendys      "@km~              157              0
    ##  7 595535~ 2019-11-19 17:52:26 Wendys      "@mo~              104              0
    ##  8 595535~ 2019-11-19 17:12:54 Wendys      "@_b~              109              0
    ##  9 595535~ 2019-11-19 16:45:11 Wendys      "@le~               95              0
    ## 10 595535~ 2019-11-19 16:40:36 Wendys      "@sh~              119              0
    ## # ... with 3,189 more rows, and 4 more variables: retweet_count <int>,
    ## #   followers_count <int>, friends_count <int>, favourites_count <int>

``` r
names(Wendys.df)
```

    ##  [1] "user_id"            "created_at"         "screen_name"       
    ##  [4] "text"               "display_text_width" "favorite_count"    
    ##  [7] "retweet_count"      "followers_count"    "friends_count"     
    ## [10] "favourites_count"

## Getting Day and Hour of when the tweets were done

``` r
Wendys.df$date = ymd_hms(Wendys.df$created_at)
Wendys.df$hour = hour(Wendys.df$date) + minute(Wendys.df$date)/60
Wendys.df$day = weekdays(as.Date(Wendys.df$date))

Wendys.df$text[1:10]
```

    ##  [1] "@thecbdvirgo Oh no! Can you DM us the address of the Wendy's you visited and your phone number? We'd love to make this right."                                                                                                  
    ##  [2] "@jaygremillion Hey there! Please DM us the exact restaurant location so we can look into this. Thanks!"                                                                                                                         
    ##  [3] "@CyrusCarlos1 That's not okay! Please DM us the restaurant location and your phone number so we can make this right. Thanks!"                                                                                                   
    ##  [4] "@NoCapMatt Hi there! Hi there-\nWe'd be happy to connect you with the Human Resources point of contact for the restaurant you worked. DM us the address of the restaurant, your full name and phone number so HR can reach out."
    ##  [5] "@LbbhBigCrybaby You aren’t simply a clown. You are the entire circus."                                                                                                                                                          
    ##  [6] "@kmichaelprince That's a smart kid, which we credit to good parenting. Please DM us his name and mailing address so we can send some Wendy's swag his way!  #KidsLoveWendys <U+0001F600>"                                       
    ##  [7] "@mommaSallyAnn That's not okay! Please DM us the restaurant location and your phone # so we can look into this. Thanks!"                                                                                                        
    ##  [8] "@_breeezayy That's not okay! Please DM us the restaurant location and your phone number so we can look into this. Thanks!"                                                                                                      
    ##  [9] "@leadinsideout Oh no! Please DM us the restaurant location and your phone # so we can make this right. Thanks!"                                                                                                                 
    ## [10] "@shannonkarnygm1 We're disappointed to hear this. Please DM us with the location and your phone number so we can look into this further."

## Using Functions to clean the tweets text

``` r
clean1 <- textScrubber(Wendys.df)
clean1[1:10]
```

    ## # A tibble: 3,199 x 10
    ##    user_id created_at          screen_name text  display_text_wi~ favorite_count
    ##    <chr>   <dttm>              <chr>       <chr>            <dbl>          <int>
    ##  1 595535~ 2019-11-19 19:25:47 Wendys      "the~              112              2
    ##  2 595535~ 2019-11-19 18:54:57 Wendys      "jay~               87              1
    ##  3 595535~ 2019-11-19 18:47:07 Wendys      "cyr~              110              0
    ##  4 595535~ 2019-11-19 18:39:35 Wendys      "noc~              211              0
    ##  5 595535~ 2019-11-19 18:32:33 Wendys      "lbb~               53             12
    ##  6 595535~ 2019-11-19 18:28:53 Wendys      "kmi~              157              0
    ##  7 595535~ 2019-11-19 17:52:26 Wendys      "mom~              104              0
    ##  8 595535~ 2019-11-19 17:12:54 Wendys      "bre~              109              0
    ##  9 595535~ 2019-11-19 16:45:11 Wendys      "lea~               95              0
    ## 10 595535~ 2019-11-19 16:40:36 Wendys      "sha~              119              0
    ## # ... with 3,189 more rows, and 4 more variables: retweet_count <int>,
    ## #   followers_count <int>, friends_count <int>, favourites_count <int>

``` r
clean2<-clean.text(Wendys.df$text)
clean2[1:10]
```

    ##  [1] "oh no can you dm us the address of the wendys you visited and your phone number wed love to make this right"                                                                                                   
    ##  [2] "hey there please dm us the exact restaurant location so we can look into this thanks"                                                                                                                          
    ##  [3] "thats not okay please dm us the restaurant location and your phone number so we can make this right thanks"                                                                                                    
    ##  [4] "hi there hi there\nwed be happy to connect you with the human resources point of contact for the restaurant you worked dm us the address of the restaurant your full name and phone number so hr can reach out"
    ##  [5] "you aren’t simply a clown you are the entire circus"                                                                                                                                                           
    ##  [6] "thats a smart kid which we credit to good parenting please dm us his name and mailing address so we can send some wendys swag his waykidslovewendys <U+0001F600>"                                              
    ##  [7] "thats not okay please dm us the restaurant location and your phoneso we can look into this thanks"                                                                                                             
    ##  [8] "thats not okay please dm us the restaurant location and your phone number so we can look into this thanks"                                                                                                     
    ##  [9] "oh no please dm us the restaurant location and your phoneso we can make this right thanks"                                                                                                                     
    ## [10] "were disappointed to hear this please dm us with the location and your phone number so we can look into this further"

``` r
clean3<- CleanTweets(clean2)
clean3[1:20]
```

    ##  [1] "oh no can you dm us the address of the wendys you visited and your phone number wed love to make this right"                                                                                                 
    ##  [2] "hey there please dm us the exact restaurant location so we can look into this thanks"                                                                                                                        
    ##  [3] "thats not okay please dm us the restaurant location and your phone number so we can make this right thanks"                                                                                                  
    ##  [4] "hi there hi therewed be happy to connect you with the human resources point of contact for the restaurant you worked dm us the address of the restaurant your full name and phone number so hr can reach out"
    ##  [5] "you aren’t simply a clown you are the entire circus"                                                                                                                                                         
    ##  [6] "thats a smart kid which we credit to good parenting please dm us his name and mailing address so we can send some wendys swag his waykidslovewendys <U+0001F600>"                                            
    ##  [7] "thats not okay please dm us the restaurant location and your phoneso we can look into this thanks"                                                                                                           
    ##  [8] "thats not okay please dm us the restaurant location and your phone number so we can look into this thanks"                                                                                                   
    ##  [9] "oh no please dm us the restaurant location and your phoneso we can make this right thanks"                                                                                                                   
    ## [10] "were disappointed to hear this please dm us with the location and your phone number so we can look into this further"                                                                                        
    ## [11] "this isnt the service we expect please dm us with the location and your phone number so we can look into this further and make this right"                                                                   
    ## [12] "yo these are awesome"                                                                                                                                                                                        
    ## [13] "hi there please dm us your contact information and we will look into this further for you"                                                                                                                   
    ## [14] "this isnt the service we expect please dm us with the location and your contact info so we can look into this further"                                                                                       
    ## [15] "this isn’t the quality we’d expect please dm us with info on this location along with your number and well make it up to you"                                                                                
    ## [16] "were sorry you left disappointed can you dm us the address of your visit and your phone number we’d love to make things up to you"                                                                           
    ## [17] "were sorry to hear that please dm us the restaurant location and your phoneso we can make this right thanks"                                                                                                 
    ## [18] "thats not okay please dm us the restaurant location and your phoneso we can look into this thanks"                                                                                                           
    ## [19] "meme would be better if you took the machines broke off and let people get the joke from the hat maybe try a nonimpact font as well"                                                                         
    ## [20] "thank you for bringing this to our attention can you dm us the address of this location we will forward along your feedback and make sure this is addressed"

## Replacing words not needed with blank spaces or fixing words

``` r
clean4 <- str_replace_all(clean3,"wendys"," ")
clean4 <- str_replace_all(clean4,"isn't","isnt")
clean4 <- str_replace_all(clean4,"'re"," ")
clean4 <- str_replace_all(clean4,"'s"," ")
clean4 <- str_replace_all(clean4,"'d"," ")
clean4 <- str_replace_all(clean4,"'re"," ")
```

## Clean Corpus function

``` r
CleanedCorpus = function(tweets) {
  tweetCorpus = Corpus(VectorSource(tweets))
  tweetCorpus = tm_map(tweetCorpus, content_transformer(tolower))
  tweetCorpus = tm_map(tweetCorpus, removePunctuation)
  tweetCorpus = tm_map(tweetCorpus, removeNumbers)
  tweetCorpus = tm_map(tweetCorpus, removeWords,stopwords("english"))
  return(tweetCorpus)
}
```

## Making a Corpus of tweets (cleaned), formatting, and turning it into a document matrix

``` r
WendysCorpus = CleanedCorpus(clean4)
```

    ## Warning in tm_map.SimpleCorpus(tweetCorpus, content_transformer(tolower)):
    ## transformation drops documents

    ## Warning in tm_map.SimpleCorpus(tweetCorpus, removePunctuation): transformation
    ## drops documents

    ## Warning in tm_map.SimpleCorpus(tweetCorpus, removeNumbers): transformation drops
    ## documents

    ## Warning in tm_map.SimpleCorpus(tweetCorpus, removeWords, stopwords("english")):
    ## transformation drops documents

``` r
temp = sapply(WendysCorpus,function(row) iconv(row, "latin1", "ASCII", sub = ""))
WendyCorpus2 =  Corpus(VectorSource(temp))

WendysTDM = TermDocumentMatrix(WendysCorpus)
WendysCorp.mat = as.matrix(WendysTDM)
```

``` r
word.freq = sort(rowSums(WendysCorp.mat), decreasing = T)
word.freq[1:40]
```

    ##          can         make       please     location        right       thanks 
    ##         1308         1124         1107         1034          825          680 
    ##   restaurant       number      phoneso      address         info         look 
    ##          670          543          493          387          376          356 
    ##        phone        along        sorry         hear         well        thats 
    ##          352          341          323          280          280          265 
    ##      contact        visit       expect         okay         love           ’d 
    ##          264          260          256          236          184          178 
    ##        email         like      service         isnt disappointed      quality 
    ##          168          145          135          131          129          128 
    ##         will       things          ’re      someone        isn’t      phoneor 
    ##          117          115          109          105           98           91 
    ##           ’s       doesnt        reach   management 
    ##           91           86           84           80

``` r
word.freq1 = word.freq[1:20]
word.freq2 = word.freq[21:40]
```

## Making barplots, wordclouds, and histograms of the words found

``` r
barplot(word.freq1,xlab = "1-20 Words", cex.names = .6)
```

![](Twitter_Analysis_PCA_files/figure-gfm/plotting%20wordclouds%20and%20barplots-1.png)<!-- -->

``` r
barplot(word.freq2,xlab = "21-40 Words", cex.names = .6)
```

![](Twitter_Analysis_PCA_files/figure-gfm/plotting%20wordclouds%20and%20barplots-2.png)<!-- -->

``` r
wordcloud(words = names(word.freq),freq = word.freq, random.order = F, col = rainbow(1000),min.freq = 5,max.words = 30)
```

![](Twitter_Analysis_PCA_files/figure-gfm/plotting%20wordclouds%20and%20barplots-3.png)<!-- -->

``` r
wordcloud(words = names(word.freq),freq = word.freq, random.order = F, col = rainbow(1000),min.freq = 10,max.words = 50)
```

![](Twitter_Analysis_PCA_files/figure-gfm/plotting%20wordclouds%20and%20barplots-4.png)<!-- -->

``` r
wordcloud(words = names(word.freq1),freq = word.freq1, random.order = F, col = rainbow(1000),min.freq = 5,max.words = 30)
```

![](Twitter_Analysis_PCA_files/figure-gfm/plotting%20wordclouds%20and%20barplots-5.png)<!-- -->

``` r
wordcloud(words = names(word.freq1),freq = word.freq2, random.order = F, col = rainbow(1000),min.freq = 10,max.words = 50)
```

![](Twitter_Analysis_PCA_files/figure-gfm/plotting%20wordclouds%20and%20barplots-6.png)<!-- -->

``` r
hist(Wendys.df$hour, main = " Wendys Tweets 24 Hours", xlab = "Hours", col="Blue")
```

![](Twitter_Analysis_PCA_files/figure-gfm/plotting%20wordclouds%20and%20barplots-7.png)<!-- -->

``` r
barplot(table(Wendys.df$day), main = " Tweets Per Day Wendys", xlab = "Weekdays", col="Blue")
```

![](Twitter_Analysis_PCA_files/figure-gfm/plotting%20wordclouds%20and%20barplots-8.png)<!-- -->

## Making a matrix off of the corpus matrix

``` r
wendys.mat = as.matrix(WendysCorp.mat)

temp = t(WendysCorp.mat)
temp [temp>1] = 1
```

## Doing Arules (apriori)

``` r
wendys.trans <- as(temp,"transactions")
wendys.rules = apriori(wendys.trans) 
```

    ## Apriori
    ## 
    ## Parameter specification:
    ##  confidence minval smax arem  aval originalSupport maxtime support minlen
    ##         0.8    0.1    1 none FALSE            TRUE       5     0.1      1
    ##  maxlen target  ext
    ##      10  rules TRUE
    ## 
    ## Algorithmic control:
    ##  filter tree heap memopt load sort verbose
    ##     0.1 TRUE TRUE  FALSE TRUE    2    TRUE
    ## 
    ## Absolute minimum support count: 311 
    ## 
    ## set item appearances ...[0 item(s)] done [0.00s].
    ## set transactions ...[1867 item(s), 3112 transaction(s)] done [0.00s].
    ## sorting and recoding items ... [15 item(s)] done [0.00s].
    ## creating transaction tree ... done [0.00s].
    ## checking subsets of size 1 2 3 4 5 6 7 8 done [0.00s].
    ## writing ... [717 rule(s)] done [0.00s].
    ## creating S4 object  ... done [0.00s].

## Plotting the Association rules

``` r
wendys.rules = apriori(wendys.trans, parameter = list(support = .22, conf = .85)) 
```

    ## Apriori
    ## 
    ## Parameter specification:
    ##  confidence minval smax arem  aval originalSupport maxtime support minlen
    ##        0.85    0.1    1 none FALSE            TRUE       5    0.22      1
    ##  maxlen target  ext
    ##      10  rules TRUE
    ## 
    ## Algorithmic control:
    ##  filter tree heap memopt load sort verbose
    ##     0.1 TRUE TRUE  FALSE TRUE    2    TRUE
    ## 
    ## Absolute minimum support count: 684 
    ## 
    ## set item appearances ...[0 item(s)] done [0.00s].
    ## set transactions ...[1867 item(s), 3112 transaction(s)] done [0.00s].
    ## sorting and recoding items ... [5 item(s)] done [0.00s].
    ## creating transaction tree ... done [0.00s].
    ## checking subsets of size 1 2 3 done [0.00s].
    ## writing ... [6 rule(s)] done [0.00s].
    ## creating S4 object  ... done [0.00s].

``` r
plot(wendys.rules, "graph")
```

![](Twitter_Analysis_PCA_files/figure-gfm/plotting%20arules-1.png)<!-- -->

``` r
wendys.rules2 = apriori(wendys.trans, parameter = list(support = .22, conf = .8)) 
```

    ## Apriori
    ## 
    ## Parameter specification:
    ##  confidence minval smax arem  aval originalSupport maxtime support minlen
    ##         0.8    0.1    1 none FALSE            TRUE       5    0.22      1
    ##  maxlen target  ext
    ##      10  rules TRUE
    ## 
    ## Algorithmic control:
    ##  filter tree heap memopt load sort verbose
    ##     0.1 TRUE TRUE  FALSE TRUE    2    TRUE
    ## 
    ## Absolute minimum support count: 684 
    ## 
    ## set item appearances ...[0 item(s)] done [0.00s].
    ## set transactions ...[1867 item(s), 3112 transaction(s)] done [0.00s].
    ## sorting and recoding items ... [5 item(s)] done [0.00s].
    ## creating transaction tree ... done [0.00s].
    ## checking subsets of size 1 2 3 done [0.00s].
    ## writing ... [10 rule(s)] done [0.00s].
    ## creating S4 object  ... done [0.00s].

``` r
plot(wendys.rules2, "graph")
```

![](Twitter_Analysis_PCA_files/figure-gfm/plotting%20arules-2.png)<!-- -->

``` r
wendys.rules3 = apriori(wendys.trans, parameter = list(support = .20, conf = .8)) 
```

    ## Apriori
    ## 
    ## Parameter specification:
    ##  confidence minval smax arem  aval originalSupport maxtime support minlen
    ##         0.8    0.1    1 none FALSE            TRUE       5     0.2      1
    ##  maxlen target  ext
    ##      10  rules TRUE
    ## 
    ## Algorithmic control:
    ##  filter tree heap memopt load sort verbose
    ##     0.1 TRUE TRUE  FALSE TRUE    2    TRUE
    ## 
    ## Absolute minimum support count: 622 
    ## 
    ## set item appearances ...[0 item(s)] done [0.00s].
    ## set transactions ...[1867 item(s), 3112 transaction(s)] done [0.00s].
    ## sorting and recoding items ... [7 item(s)] done [0.00s].
    ## creating transaction tree ... done [0.00s].
    ## checking subsets of size 1 2 3 done [0.00s].
    ## writing ... [16 rule(s)] done [0.00s].
    ## creating S4 object  ... done [0.00s].

``` r
plot(wendys.rules3, "graph")
```

![](Twitter_Analysis_PCA_files/figure-gfm/plotting%20arules-3.png)<!-- -->

``` r
wendys.rules4 = apriori(wendys.trans, parameter = list(support = .18, conf = .6)) 
```

    ## Apriori
    ## 
    ## Parameter specification:
    ##  confidence minval smax arem  aval originalSupport maxtime support minlen
    ##         0.6    0.1    1 none FALSE            TRUE       5    0.18      1
    ##  maxlen target  ext
    ##      10  rules TRUE
    ## 
    ## Algorithmic control:
    ##  filter tree heap memopt load sort verbose
    ##     0.1 TRUE TRUE  FALSE TRUE    2    TRUE
    ## 
    ## Absolute minimum support count: 560 
    ## 
    ## set item appearances ...[0 item(s)] done [0.00s].
    ## set transactions ...[1867 item(s), 3112 transaction(s)] done [0.00s].
    ## sorting and recoding items ... [7 item(s)] done [0.00s].
    ## creating transaction tree ... done [0.00s].
    ## checking subsets of size 1 2 3 4 done [0.00s].
    ## writing ... [72 rule(s)] done [0.00s].
    ## creating S4 object  ... done [0.00s].

``` r
plot(wendys.rules4, "graph")
```

![](Twitter_Analysis_PCA_files/figure-gfm/plotting%20arules-4.png)<!-- -->

## Sorting the association rules by lift

``` r
wendys.sort = sort(wendys.rules, by = "lift")
wendys.sort2 = sort(wendys.rules2, by = "lift")
wendys.sort3 = sort(wendys.rules3, by = "lift")
wendys.sort4 = sort(wendys.rules4, by = "lift")
```

## Looking at what words have highest lift

``` r
inspect(wendys.sort)
```

    ##     lhs                rhs        support   confidence coverage  lift     count
    ## [1] {right}         => {make}     0.2336118 0.9745308  0.2397172 2.808093 727  
    ## [2] {can,location}  => {please}   0.2419666 0.9567980  0.2528920 2.694620 753  
    ## [3] {make,please}   => {location} 0.2217224 0.8868895  0.2500000 2.674419 690  
    ## [4] {location}      => {please}   0.3004499 0.9060078  0.3316195 2.551580 935  
    ## [5] {make,location} => {please}   0.2217224 0.9007833  0.2461440 2.536867 690  
    ## [6] {right}         => {can}      0.2207584 0.9209115  0.2397172 2.428709 687

``` r
inspect(wendys.sort2)
```

    ##      lhs                  rhs        support   confidence coverage  lift    
    ## [1]  {right}           => {make}     0.2336118 0.9745308  0.2397172 2.808093
    ## [2]  {can,location}    => {please}   0.2419666 0.9567980  0.2528920 2.694620
    ## [3]  {make,please}     => {location} 0.2217224 0.8868895  0.2500000 2.674419
    ## [4]  {can,please}      => {location} 0.2419666 0.8470191  0.2856684 2.554189
    ## [5]  {location}        => {please}   0.3004499 0.9060078  0.3316195 2.551580
    ## [6]  {please}          => {location} 0.3004499 0.8461538  0.3550771 2.551580
    ## [7]  {make,location}   => {please}   0.2217224 0.9007833  0.2461440 2.536867
    ## [8]  {right}           => {can}      0.2207584 0.9209115  0.2397172 2.428709
    ## [9]  {location,please} => {can}      0.2419666 0.8053476  0.3004499 2.123934
    ## [10] {please}          => {can}      0.2856684 0.8045249  0.3550771 2.121764
    ##      count
    ## [1]  727  
    ## [2]  753  
    ## [3]  690  
    ## [4]  753  
    ## [5]  935  
    ## [6]  935  
    ## [7]  690  
    ## [8]  687  
    ## [9]  753  
    ## [10] 889

``` r
inspect(wendys.sort3)
```

    ##      lhs                  rhs        support   confidence coverage  lift    
    ## [1]  {can,make}        => {right}    0.2188303 0.8576826  0.2551414 3.577893
    ## [2]  {can,right}       => {make}     0.2188303 0.9912664  0.2207584 2.856316
    ## [3]  {right}           => {make}     0.2336118 0.9745308  0.2397172 2.808093
    ## [4]  {restaurant}      => {please}   0.2043702 0.9578313  0.2133676 2.697530
    ## [5]  {can,location}    => {please}   0.2419666 0.9567980  0.2528920 2.694620
    ## [6]  {make,please}     => {location} 0.2217224 0.8868895  0.2500000 2.674419
    ## [7]  {can,please}      => {location} 0.2419666 0.8470191  0.2856684 2.554189
    ## [8]  {location}        => {please}   0.3004499 0.9060078  0.3316195 2.551580
    ## [9]  {please}          => {location} 0.3004499 0.8461538  0.3550771 2.551580
    ## [10] {make,location}   => {please}   0.2217224 0.9007833  0.2461440 2.536867
    ## [11] {restaurant}      => {can}      0.2046915 0.9593373  0.2133676 2.530049
    ## [12] {make,right}      => {can}      0.2188303 0.9367263  0.2336118 2.470417
    ## [13] {right}           => {can}      0.2207584 0.9209115  0.2397172 2.428709
    ## [14] {thanks}          => {can}      0.2001928 0.9161765  0.2185090 2.416221
    ## [15] {location,please} => {can}      0.2419666 0.8053476  0.3004499 2.123934
    ## [16] {please}          => {can}      0.2856684 0.8045249  0.3550771 2.121764
    ##      count
    ## [1]  681  
    ## [2]  681  
    ## [3]  727  
    ## [4]  636  
    ## [5]  753  
    ## [6]  690  
    ## [7]  753  
    ## [8]  935  
    ## [9]  935  
    ## [10] 690  
    ## [11] 637  
    ## [12] 681  
    ## [13] 687  
    ## [14] 623  
    ## [15] 753  
    ## [16] 889

``` r
inspect(wendys.sort4)
```

    ##      lhs                             rhs          support   confidence
    ## [1]  {can,make,please}            => {right}      0.1921594 0.9933555 
    ## [2]  {thanks}                     => {restaurant} 0.1809126 0.8279412 
    ## [3]  {restaurant}                 => {thanks}     0.1809126 0.8478916 
    ## [4]  {can,location,please}        => {restaurant} 0.1953728 0.8074369 
    ## [5]  {can,location}               => {restaurant} 0.1956941 0.7738247 
    ## [6]  {can,make}                   => {right}      0.2188303 0.8576826 
    ## [7]  {can,location}               => {thanks}     0.1802699 0.7128335 
    ## [8]  {can,please}                 => {restaurant} 0.1985861 0.6951631 
    ## [9]  {make,please}                => {right}      0.1931234 0.7724936 
    ## [10] {can,please}                 => {thanks}     0.1960154 0.6861642 
    ## [11] {make,location}              => {right}      0.1825193 0.7415144 
    ## [12] {location,please}            => {restaurant} 0.1963368 0.6534759 
    ## [13] {can,please,restaurant}      => {location}   0.1953728 0.9838188 
    ## [14] {please,restaurant}          => {location}   0.1963368 0.9606918 
    ## [15] {can,restaurant}             => {location}   0.1956941 0.9560440 
    ## [16] {right,please}               => {make}       0.1931234 0.9917492 
    ## [17] {can,right,please}           => {make}       0.1921594 0.9917081 
    ## [18] {right,location}             => {make}       0.1825193 0.9912740 
    ## [19] {can,right}                  => {make}       0.2188303 0.9912664 
    ## [20] {can,please}                 => {right}      0.1937661 0.6782902 
    ## [21] {location,restaurant}        => {please}     0.1963368 0.9983660 
    ## [22] {can,location,restaurant}    => {please}     0.1953728 0.9983580 
    ## [23] {make}                       => {right}      0.2336118 0.6731481 
    ## [24] {right}                      => {make}       0.2336118 0.9745308 
    ## [25] {location,thanks}            => {please}     0.1818766 0.9929825 
    ## [26] {restaurant}                 => {location}   0.1966581 0.9216867 
    ## [27] {location,please}            => {thanks}     0.1818766 0.6053476 
    ## [28] {can,thanks}                 => {please}     0.1960154 0.9791332 
    ## [29] {please,thanks}              => {location}   0.1818766 0.9114332 
    ## [30] {can,restaurant}             => {please}     0.1985861 0.9701727 
    ## [31] {can,thanks}                 => {location}   0.1802699 0.9004815 
    ## [32] {restaurant}                 => {please}     0.2043702 0.9578313 
    ## [33] {can,location}               => {please}     0.2419666 0.9567980 
    ## [34] {make,please}                => {location}   0.2217224 0.8868895 
    ## [35] {location,restaurant}        => {can}        0.1956941 0.9950980 
    ## [36] {location,please,restaurant} => {can}        0.1953728 0.9950900 
    ## [37] {right,please}               => {can}        0.1937661 0.9950495 
    ## [38] {make,right,please}          => {can}        0.1921594 0.9950083 
    ## [39] {location,thanks}            => {can}        0.1802699 0.9842105 
    ## [40] {please,thanks}              => {can}        0.1960154 0.9822866 
    ## [41] {thanks}                     => {please}     0.1995501 0.9132353 
    ## [42] {please,restaurant}          => {can}        0.1985861 0.9716981 
    ## [43] {can,please}                 => {location}   0.2419666 0.8470191 
    ## [44] {location}                   => {please}     0.3004499 0.9060078 
    ## [45] {please}                     => {location}   0.3004499 0.8461538 
    ## [46] {make,location}              => {please}     0.2217224 0.9007833 
    ## [47] {restaurant}                 => {can}        0.2046915 0.9593373 
    ## [48] {thanks}                     => {location}   0.1831620 0.8382353 
    ## [49] {can,make,right}             => {please}     0.1921594 0.8781204 
    ## [50] {can,right}                  => {please}     0.1937661 0.8777293 
    ## [51] {make,right}                 => {can}        0.2188303 0.9367263 
    ## [52] {right}                      => {can}        0.2207584 0.9209115 
    ## [53] {thanks}                     => {can}        0.2001928 0.9161765 
    ## [54] {make,right}                 => {location}   0.1825193 0.7812930 
    ## [55] {make,right}                 => {please}     0.1931234 0.8266850 
    ## [56] {right}                      => {location}   0.1841260 0.7680965 
    ## [57] {right}                      => {please}     0.1947301 0.8123324 
    ## [58] {location}                   => {make}       0.2461440 0.7422481 
    ## [59] {make}                       => {location}   0.2461440 0.7092593 
    ## [60] {can,make}                   => {please}     0.1934447 0.7581864 
    ## [61] {location,please}            => {make}       0.2217224 0.7379679 
    ## [62] {location,please}            => {can}        0.2419666 0.8053476 
    ## [63] {please}                     => {can}        0.2856684 0.8045249 
    ## [64] {can}                        => {please}     0.2856684 0.7533898 
    ## [65] {make,please}                => {can}        0.1934447 0.7737789 
    ## [66] {please}                     => {make}       0.2500000 0.7040724 
    ## [67] {make}                       => {please}     0.2500000 0.7203704 
    ## [68] {location}                   => {can}        0.2528920 0.7625969 
    ## [69] {can}                        => {location}   0.2528920 0.6669492 
    ## [70] {can,please}                 => {make}       0.1934447 0.6771654 
    ## [71] {make}                       => {can}        0.2551414 0.7351852 
    ## [72] {can}                        => {make}       0.2551414 0.6728814 
    ##      coverage  lift     count
    ## [1]  0.1934447 4.143864 598  
    ## [2]  0.2185090 3.880351 563  
    ## [3]  0.2133676 3.880351 563  
    ## [4]  0.2419666 3.784253 608  
    ## [5]  0.2528920 3.626720 609  
    ## [6]  0.2551414 3.577893 681  
    ## [7]  0.2528920 3.262262 561  
    ## [8]  0.2856684 3.258054 618  
    ## [9]  0.2500000 3.222520 601  
    ## [10] 0.2856684 3.140210 610  
    ## [11] 0.2461440 3.093288 568  
    ## [12] 0.3004499 3.062676 611  
    ## [13] 0.1985861 2.966709 608  
    ## [14] 0.2043702 2.896970 611  
    ## [15] 0.2046915 2.882954 609  
    ## [16] 0.1947301 2.857707 601  
    ## [17] 0.1937661 2.857589 598  
    ## [18] 0.1841260 2.856338 568  
    ## [19] 0.2207584 2.856316 681  
    ## [20] 0.2856684 2.829543 603  
    ## [21] 0.1966581 2.811688 611  
    ## [22] 0.1956941 2.811665 608  
    ## [23] 0.3470437 2.808093 727  
    ## [24] 0.2397172 2.808093 727  
    ## [25] 0.1831620 2.796526 566  
    ## [26] 0.2133676 2.779350 612  
    ## [27] 0.3004499 2.770355 566  
    ## [28] 0.2001928 2.757523 610  
    ## [29] 0.1995501 2.748430 566  
    ## [30] 0.2046915 2.732287 618  
    ## [31] 0.2001928 2.715406 561  
    ## [32] 0.2133676 2.697530 636  
    ## [33] 0.2528920 2.694620 753  
    ## [34] 0.2500000 2.674419 690  
    ## [35] 0.1966581 2.624360 609  
    ## [36] 0.1963368 2.624339 608  
    ## [37] 0.1947301 2.624232 603  
    ## [38] 0.1931234 2.624124 598  
    ## [39] 0.1831620 2.595647 561  
    ## [40] 0.1995501 2.590573 610  
    ## [41] 0.2185090 2.571935 621  
    ## [42] 0.2043702 2.562648 618  
    ## [43] 0.2856684 2.554189 753  
    ## [44] 0.3316195 2.551580 935  
    ## [45] 0.3550771 2.551580 935  
    ## [46] 0.2461440 2.536867 690  
    ## [47] 0.2133676 2.530049 637  
    ## [48] 0.2185090 2.527702 570  
    ## [49] 0.2188303 2.473041 598  
    ## [50] 0.2207584 2.471940 603  
    ## [51] 0.2336118 2.470417 681  
    ## [52] 0.2397172 2.428709 687  
    ## [53] 0.2185090 2.416221 623  
    ## [54] 0.2336118 2.355992 568  
    ## [55] 0.2336118 2.328184 601  
    ## [56] 0.2397172 2.316198 573  
    ## [57] 0.2397172 2.287763 606  
    ## [58] 0.3316195 2.138774 766  
    ## [59] 0.3470437 2.138774 766  
    ## [60] 0.2551414 2.135272 602  
    ## [61] 0.3004499 2.126441 690  
    ## [62] 0.3004499 2.123934 753  
    ## [63] 0.3550771 2.121764 889  
    ## [64] 0.3791774 2.121764 889  
    ## [65] 0.2500000 2.040678 602  
    ## [66] 0.3550771 2.028772 778  
    ## [67] 0.3470437 2.028772 778  
    ## [68] 0.3316195 2.011188 787  
    ## [69] 0.3791774 2.011188 787  
    ## [70] 0.2856684 1.951239 602  
    ## [71] 0.3470437 1.938895 794  
    ## [72] 0.3791774 1.938895 794
