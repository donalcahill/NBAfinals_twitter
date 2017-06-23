rm(list = ls())

library(twitteR)
library(plyr)
library(ggplot2)
library(cowplot)

data <- readRDS("NBAfinals_data.rds")

# clean duplicates
data <- data[!duplicated(data),] # remove harvesting duplicates
data[duplicated(data[,names(data)!="team"])|duplicated(data[,names(data)!="team"],fromLast = TRUE),"team"] <- "both" #identify tweets with tags for both teams
data <- data[!duplicated(data),] # since tweets with both tags are now assigned the same team name (i.e. "both"), they are duplicated. Here we remove the duplicates.

# convert timezones to EST
attr(data$created, "tzone") <- "America/New_York"

game_start_times <- as.POSIXct(c("2017-06-01 21:00:00",
                                 "2017-06-04 20:00:00",
                                 "2017-06-07 21:00:00",
                                 "2017-06-09 21:00:00",
                                 "2017-06-12 21:00:00"),
                               tz="America/New_York")
# find nearest game
data$closestGame <- sapply(data$created,function(x) which.min(abs(x-game_start_times)))
# calculate time to/from the start of the nearest game
data$timeToGame <- as.numeric(data$created - game_start_times[data$closestGame])

# reencode tweet text using iconv
data$text <- iconv(data$text,"latin1","ascii","byte")

emojiByCat <- readLines("emojiByCat.txt") # text file containing emoji unicode strings and classification
emojiRDict <- read.csv2("emojis.csv",stringsAsFactors = FALSE) # loading emoji dictionary

# function to scrape unicodes from text file
getCodes <- function(emojiTextFile,inds) {
  return(
    aaply(emojiTextFile[inds],1,function(x) {
      x <- strsplit(x,"\\s+")[[1]]
      x <- iconv(x, 'UTF-8', 'latin1', 'byte')
      x <- x[grep("^<",x)]
    }
    )
  )
}

# function to translate unicodes into search strings for searching through tweets.
createEmojiSearchStr <- function(codes) {
  dictInds <- grep(paste(codes,collapse="|"),emojiRDict$utf8)
  searchStr <- paste(emojiRDict$ftu8[dictInds],collapse="|")
  return(searchStr)
}

posInds <- (which(emojiByCat %in% "# subgroup: face-positive")+1):(which(emojiByCat %in% "# subgroup: face-neutral")-2)
negInds <- (which(emojiByCat %in% "# subgroup: face-negative")+1):(which(emojiByCat %in% "# subgroup: face-sick")-2)

posCodes <- getCodes(emojiByCat,posInds)
negCodes <- getCodes(emojiByCat,negInds)

posSearchStr <- createEmojiSearchStr(posCodes)
negSearchStr <- createEmojiSearchStr(negCodes)

# assign valence to each tweet
data$valence <- "neutral"
data$valence[grep(posSearchStr,data$text)] <- "pos"
data$valence[grep(negSearchStr,data$text)] <- "neg"

# for convenience
mins <- 60
hours <- 60*mins
days <- 24*hours
gameLength <- 2.5*hours
teamCols <- c("orangered3","blue")

game_end_times <- as.POSIXct(c("2017-06-01 23:31:00",
                               "2017-06-04 22:40:00",
                               "2017-06-07 23:48:00",
                               "2017-06-10 00:04:00",
                               "2017-06-12 23:46:00"),
                             tz="America/New_York")
game_end_times_relative <- data.frame(closestGame=1:5,
                                      endTime=difftime(game_end_times,game_start_times,units="mins"))

# function to select a subset of tweets
selectTweetsRel <- function(data,timeStart,timeEnd,team=c("cavs","wrrs"),valence = c("pos","neg"),gamesToInclude=1:5) {
  return(data[data$timeToGame>timeStart &
                data$timeToGame<timeEnd &
                data$team %in% team &
                data$valence %in% valence &
                data$closestGame %in% gamesToInclude,])
}

# function to summarise
sumTweets <- function(data,splitVars,timeStart=-hours,timeEnd=hours+gameLength,team=c("cavs","wrrs"),valence = c("pos","neg"),gamesToInclude=1:5,returnValDiff=FALSE,useRetweet=TRUE,cutBreaks) {
  
  if (returnValDiff & missing("cutBreaks")) {stop('specify cutBreaks')}
  if (returnValDiff & ("valence" %in% splitVars)) {stop('cannot split on valence')}
  if (!useRetweet) data<-data[!data$isRetweet,]
  data <- selectTweetsRel(data,timeStart,timeEnd,team,valence,gamesToInclude)
  if (!missing("cutBreaks")) {
    bks <- seq(timeStart,timeEnd,cutBreaks)
    data$timeBins <- cut(data$timeToGame,breaks = bks)
    data$timeBins <- ((as.numeric(data$timeBins)*cutBreaks + timeStart - cutBreaks/2) /mins)  # convert to minutes
    splitVars <- c(splitVars,"timeBins")
  }
  if (returnValDiff) {
    summData <- ddply(data,splitVars,summarise,valDiff = sum(valence=="pos")-sum(valence=="neg"))
  } else {
    summData <- ddply(data,splitVars,summarise,count=nrow(data))
  }
  
  return(summData)
  
}

# before game
beforeGame <- sumTweets(data,c("team","closestGame"),timeStart=-12*hours,timeEnd=0,returnValDiff = TRUE,useRetweet=FALSE,cutBreaks= hours)
beforeGame$closestGame <- as.factor(beforeGame$closestGame)
ggplot(beforeGame,aes(x=timeBins,y=valDiff,group=closestGame,color=closestGame)) + geom_line(size=1) +
  facet_grid(team~.) +
  labs(x="Minutes relative to game start",y="Pos.-Neg. Tweets",color="Game") +
  theme(legend.position = "top")

# during the game
duringGame <- sumTweets(data,c("team","closestGame"),timeStart=0,timeEnd=gameLength,returnValDiff = TRUE,useRetweet=FALSE,cutBreaks= 10*mins)
ggplot(duringGame ,aes(x=timeBins,y=valDiff,group=team,color=team)) + geom_line(size=1) +
  facet_grid(closestGame~.) +
  labs(x="Minutes relative to game start",y="Pos.-Neg. Tweets") +
  scale_color_manual(values=teamCols,
                     labels=c("Cavaliers","Warriors"),
                     name="") +
  theme(legend.position = "top",
        strip.text = element_blank(),
        strip.background = element_blank())

# after the game
afterGame <- sumTweets(data,c("team","closestGame"),timeStart=gameLength-15*mins,timeEnd=gameLength+hours,returnValDiff = TRUE,useRetweet=FALSE,cutBreaks= 5*mins)
ggplot(afterGame ,aes(x=timeBins,y=valDiff,group=team,color=team)) + geom_line(size=1) +
  geom_vline(aes(xintercept = endTime),data=game_end_times_relative) +
  facet_grid(closestGame~.) +
  labs(x="Minutes relative to game start",y="Pos.-Neg. Tweets") +
  scale_color_manual(values=teamCols,
                     labels=c("Cavaliers","Warriors"),
                     name="") +
  theme(legend.position = "top",
        strip.text = element_blank(),
        strip.background = element_blank())





