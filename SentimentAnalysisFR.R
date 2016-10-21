# Auteur: Florian CHEMIN
# Date de création: 20 octobre 2016
#
# Ce code permet de récupérer des tweets via l'API Twitter
# Et de leurs donner un score permettant ainsi de les classer
# En quatre catégories distinctes et de présenter
# Le résultat sous forme graphique.

# Charger les librairies nécessaires à l'analyse
library(plyr)
library(twitteR)
library(stringr)

# Récupérer les tweets
tweets <- searchTwitter("#DoctorStrange", n=2000, lang="fr")
tweets_txt <- sapply(tweets,function(x) x$getText())

# Une fonction pour nettoyer les données
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

# Une fonction pour attribuer un score à chacun des mots d'un tweet
sentimentScore <- function(sentences, vNegTerms, negTerms, posTerms, vPosTerms){
        final_scores <- matrix('', 0, 5)
        scores <- laply(sentences, function(sentence, vNegTerms, negTerms, posTerms, vPosTerms){
                initial_sentence <- sentence
                # Enlever les caractères inutiles et diviser par mot
                sentence <- cleanTweets(sentence)
                sentence <- tolower(sentence)
                wordList <- str_split(sentence, '\\s+')
                words <- unlist(wordList)
                # Créer un vecteur faisant correspondre les mots avec les catégories
                vPosMatches <- match(words, vPosTerms)
                posMatches <- match(words, posTerms)
                vNegMatches <- match(words, vNegTerms)
                negMatches <- match(words, negTerms)
                # Additionner les mots dans chaque catégories
                vPosMatches <- sum(!is.na(vPosMatches))
                posMatches <- sum(!is.na(posMatches))
                vNegMatches <- sum(!is.na(vNegMatches))
                negMatches <- sum(!is.na(negMatches))
                score <- c(vNegMatches, negMatches, posMatches, vPosMatches)
                # Ajouter une ligne dans la table
                newrow <- c(initial_sentence, score)
                final_scores <- rbind(final_scores, newrow)
                return(final_scores)
        }, vNegTerms, negTerms, posTerms, vPosTerms)
        return(scores)
}


# Charger le lexique de mots avec leur score
afinn_list <- read.csv(file="AFINN-FR.txt", sep="", header=FALSE, stringsAsFactors=FALSE)
names(afinn_list) <- c("word", "score")
afinn_list$word <- tolower(afinn_list$word)

# Catégoriser les mots de très négatif à très positif
vNegTerms <- afinn_list$word[afinn_list$score==-5 | afinn_list$score==-4]
negTerms <- afinn_list$word[afinn_list$score==-3 | afinn_list$score==-2 | afinn_list$score==-1]
posTerms <- afinn_list$word[afinn_list$score==3 | afinn_list$score==2 | afinn_list$score==1]
vPosTerms <- afinn_list$word[afinn_list$score==5 | afinn_list$score==4]

# Calculer le score pour chaque tweet
tweetResult <- as.data.frame(sentimentScore(tweets_txt, vNegTerms, negTerms, posTerms, vPosTerms))
tweetResult$'2' = as.numeric(tweetResult$'2')
tweetResult$'3' = as.numeric(tweetResult$'3')
tweetResult$'4' = as.numeric(tweetResult$'4')
tweetResult$'5' = as.numeric(tweetResult$'5')
counts = c(sum(tweetResult$'2'),sum(tweetResult$'3'),sum(tweetResult$'4'),sum(tweetResult$'5'))
names = c("NUL","MAUVAIS","BON","EXCELLENT")
mr = list(counts,names)

# Créer le graphique présentant le résultat final
colors = c("red", "yellow", "green", "violet")
barplot(mr[[1]], main="Critique du Film", xlab="Nombre de votes",legend=mr[[2]],col=colors)
