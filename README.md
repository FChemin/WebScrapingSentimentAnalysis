# Cinéma et opinion mining: Twitter comme critique de film
FChemin  
October 18, 2016  



Ce post présente une utilisation de [l'opinion mining](https://fr.wikipedia.org/wiki/Opinion_mining) ou analyse de sentiments en l'appliquant sur des tweets concernant un film.

L'objectif est de créer un système présentant le ressenti des personnes s'étant exprimées sur Twitter à propos d'un film. L'implémentation d'un tel système se fait en 5 étapes:

1. Obtenir les tweets de Twitter
2. Nettoyer les données
3. Créer une nuage de mots-clés
4. Créer un dictionnaire
5. Donner un score à chaque tweet

Nous prendrons comme exemple le film [Doctor Strange](http://www.imdb.com/title/tt1211837/?ref_=nv_sr_1) qui sort sur les écrans le 26 octobre 2016. Ceci nous permettra de faire une comparaison avant/après sa sortie en salle.

## Chargement des paquets nécessaires


```r
library(ROAuth)
library(twitteR)
library(tm)
```

```
## Loading required package: NLP
```

```r
library(wordcloud)
```

```
## Loading required package: RColorBrewer
```

```r
library(stringr)
library(plyr)
```

```
## 
## Attaching package: 'plyr'
```

```
## The following object is masked from 'package:twitteR':
## 
##     id
```



## Obtention des tweets de Twitter

La première étape est donc de récupérer des données à l'aide de l'API Twitter. Pour cela, nous faisons appel au paquet TwitteR de R. Chaque donnée de tweet récupéré contient les informations suivantes:

* Text
* Is re-tweeted
* Re-tweet count
* Tweeted User name
* Latitude/Longitude
* Replied to, etc.

Il nous faut nous identifier à l'aide des informations fournies sur notre compte développeur sur Twitter:


```r
consumer_key = "key"
consumer_secret = "secret"
token_secret = "token secret"
access_token = "token"
authenticate <- OAuthFactory$new(consumerKey = consumer_key,
                                 consumerSecret = consumer_secret,
                                 requestURL="https://api.twitter.com/oauth/request_token",
                                 accessURL="https://api.twitter.com/oauth/access_token",
                                 authURL="https://api.twitter.com/oauth/authorize")

setup_twitter_oauth(consumer_key, consumer_secret, access_token, token_secret)
```

Nous récupérons ensuite 2000 tweets contenant le hashtag **#DoctorStrange** et en présentons un échantillon:


```r
N = 2000
tweets <- searchTwitter("#DoctorStrange", n = N, lang = "fr")
sample(tweets, 5)
```

```
## [[1]]
## [1] "S3raph1: RT @FanactuCom: Sublime affiche fan art de #DoctorStrange par @Cakes_Comics. https://t.co/rTlqchrLsu"
## 
## [[2]]
## [1] "B_Mortreux: RT @Honor_FR: [CONCOURS] Follow @Honor_FR &amp; @MarvelFR + RT pour tenter de gagner l'un des 5 packs #DoctorStrange mis en jeu ! Le 26 octobre…"
## 
## [[3]]
## [1] "MelNaessens: RT @maxp26: Moi en sortant de #DoctorStrange https://t.co/H68aaEfIpc"
## 
## [[4]]
## [1] "H3DOJr: @lemecde_C @ColliotF @Charlieliveshow @bergamelli moi ce sont les #Kitkatball (*^.^*) ! Et la prochaine fois... #DoctorStrange !"
## 
## [[5]]
## [1] "vulcan_fangirl: Hey, quelqu'un ayant vu #DoctorStrange peut me dire EN DM si on voit Loki/Skurge dans la scène post-gen ?"
```

Nous ne nous intéresserons ici pour le moment qu'à la valeur Text des tweets.

## Nettoyage des données

Cette étape consiste à nettoyer les données afin que nous puissions les utiliser pour notre analyse. Il s'agit ici de supprimer les url, la ponctuation, les symboles, etc., afin d'avoir des données propres pour la suite du travail:


```r
tweets_txt <- sapply(tweets, function(x) x$getText())

tweets_clean <- gsub("(RT|via)((?:\\b\\W*@\\w+)+)","",tweets_txt)
tweets_clean <- gsub("http[^[:blank:]]+", "", tweets_clean)
tweets_clean <- gsub("@\\w+", "", tweets_clean)
tweets_clean <- gsub("[ \t]{2,}", "", tweets_clean)
tweets_clean <- gsub("^\\s+|\\s+$", "", tweets_clean)
tweets_clean <- gsub("[[:punct:]]", " ", tweets_clean)
tweets_clean <- gsub("[^[:alnum:]]", " ", tweets_clean)
tweets_clean <- gsub('\\d+', "", tweets_clean)
```

Cette étape de préparation des données [représente environ 80% du travail des Data Scientist](http://www.forbes.com/sites/gilpress/2016/03/23/data-preparation-most-time-consuming-least-enjoyable-data-science-task-survey-says/#10429127f758) selon une récente enquête.

## Création d'un nuage de mots-clés

Nous réalisons à cette étape un nuage de mots-clés afin d'obtenir une représentation visuelle des mots les plus fréquemment utilisés:


```r
col <- brewer.pal(6, "Dark2")
wordcloud(tweets_clean, min.freq=3, scale=c(3,.5),rot.per = 0.15, random.color=T, max.word=100, random.order=F,colors=col)
```

![](https://github.com/FChemin/WebScrapingSentimentAnalysis/blob/master/graph-unnamed-chunk-6-1.png)

Nous pouvons voir à partir du nuage que les mots les plus fréquents sont "concours", "jeu", "gagner", "packs", "octobre" et "tenter". Il s'agit possiblement d'un jeu-concours avant le lancement du film dans une semaine...

## Création d'un lexique

Pour cette étape, nous avons récupéré [une liste de mots français](https://github.com/johnthillaye/sentiment/blob/master/build/AFINN.txt) ayant chacun une note selon qu'ils soient plus ou moins positifs:


```r
afinn_list <- read.csv(file="AFINN-FR.txt", sep="", header=FALSE, stringsAsFactors=FALSE)
names(afinn_list) <- c("words", "score")
afinn_list$word <- tolower(afinn_list$word)
```

Nous les divisons ensuite en quatre catégories différentes qui nous permettront de classer chaque tweet:


```r
vNegTerms <- afinn_list$word[afinn_list$score==-5 | afinn_list$score==-4]
negTerms <- c(afinn_list$word[afinn_list$score==-3 | afinn_list$score==-2 | afinn_list$score==-1])
posTerms <- c(afinn_list$word[afinn_list$score==3 | afinn_list$score==2 | afinn_list$score==1])
vPosTerms <- c(afinn_list$word[afinn_list$score==5 | afinn_list$score==4])
```

## Notation des tweets

Nous créons maintenant une fonction nous permettant de calculer le score de chaque tweet:


```r
cleanTweets <- function(tweets)
{
        tweets_clean <- gsub("(RT|via)((?:\\b\\W*@\\w+)+)","",tweets)
        tweets_clean <- gsub("http[^[:blank:]]+", "", tweets_clean)
        tweets_clean <- gsub("@\\w+", "", tweets_clean)
        tweets_clean <- gsub("[ \t]{2,}", "", tweets_clean)
        tweets_clean <- gsub("^\\s+|\\s+$", "", tweets_clean)
        tweets_clean <- gsub("[[:punct:]]", " ", tweets_clean)
        tweets_clean <- gsub("[^[:alnum:]]", " ", tweets_clean)
        tweets_clean <- gsub('\\d+', '', tweets_clean)
        return(tweets_clean)
}

sentimentScore <- function(sentences, vNegTerms, negTerms, posTerms, vPosTerms){
        final_scores <- matrix('', 0, 5)
        scores <- laply(sentences, function(sentence, vNegTerms, negTerms, posTerms, vPosTerms){
                initial_sentence <- sentence
                #remove unnecessary characters and split up by word
                sentence = cleanTweets(sentence)
                sentence <- tolower(sentence)
                wordList <- str_split(sentence, '\\s+')
                words <- unlist(wordList)
                #build vector with matches between sentence and each category
                vPosMatches <- match(words, vPosTerms)
                posMatches <- match(words, posTerms)
                vNegMatches <- match(words, vNegTerms)
                negMatches <- match(words, negTerms)
                #sum up number of words in each category
                vPosMatches <- sum(!is.na(vPosMatches))
                posMatches <- sum(!is.na(posMatches))
                vNegMatches <- sum(!is.na(vNegMatches))
                negMatches <- sum(!is.na(negMatches))
                score <- c(vNegMatches, negMatches, posMatches, vPosMatches)
                #add row to scores table
                newrow <- c(initial_sentence, score)
                final_scores <- rbind(final_scores, newrow)
                return(final_scores)
        }, vNegTerms, negTerms, posTerms, vPosTerms)
        return(scores)
}
```

Puis nous calculons le score pour l'ensemble des tweets à l'aide de cette fonction:


```r
tweetResult <- as.data.frame(sentimentScore(tweets_txt, vNegTerms, negTerms, posTerms, vPosTerms))
tweetResult$'2' <- as.numeric(tweetResult$'2')
tweetResult$'3' <- as.numeric(tweetResult$'3')
tweetResult$'4' <- as.numeric(tweetResult$'4')
tweetResult$'5' <- as.numeric(tweetResult$'5')
counts <- c(sum(tweetResult$'2'),sum(tweetResult$'3'),sum(tweetResult$'4'),sum(tweetResult$'5'))
names <- c("NUL","MAUVAIS","BON","EXCELLENT")
mr <- list(counts,names)
```

Nous finissons en créant un graphique nous montrant la note obtenue dans chaque catégorie "NUL", "MAUVAIS", "BON" et "EXCELLENT":


```r
colors = c("red", "yellow", "green", "violet")
barplot(mr[[1]], main="Critique du Film", xlab="Nombre de votes",legend=mr[[2]],col=colors)
```

![](https://github.com/FChemin/WebScrapingSentimentAnalysis/blob/master/graph-unnamed-chunk-11-1.png)

Il semblerait donc qu'avant sa sortie en salle, ce film ait un accueil plutôt positif.

Le code complet ainsi que le lexique et les graphiques sont disponibles sur [mon Github](https://github.com/FChemin/WebScrapingSentimentAnalysis).
