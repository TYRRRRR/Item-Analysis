# Clear out all object from memory
rm( list = ls() )

library(    ggplot2 )
library(       xlsx )

outXLSFile <- 'Item Analysis.xlsx'

# Read in the file the first row contains columns names (header=TRUE)
# treat the responses as literals (stringsAsFactors=TRUE)
df_responses <- read.table("RW.dat", header=T, stringsAsFactors=F, sep='|' )

names( df_responses )[ -1] <- gsub( '[.]', ' ', names( df_responses )[ -1 ] )

# extract the answer key from the 1st row
(df_answer_key <- df_responses[ 1, ] )

# remove the answer key from the matrix
df_responses <- df_responses[ -1, ]

( nItems     <- ncol( df_responses ) - 1 ) # How many items
( nExaminees <- nrow( df_responses )     ) # How many respondents

# Transpose the data to a long format
df_responses_long <- reshape( df_responses, dir='long', idvar='ID', 
                              v.name='Response', timevar='Item',
                              varying=paste0( 'Item ', 1:20 ), 
                              times=paste0( 'Item ', 1:20 ) )

# For cosmetic (sorting purposes)
df_items <- data.frame( Item=unique( df_responses_long$Item ),
                        fItem=do.call( rbind, strsplit( unique( df_responses_long$Item ), '[ ]') )[,2] )

df_items$fItem <- as.integer( df_items$fItem )
df_items$fItem <- factor( df_items$fItem, labels=df_items$Item )
df_responses_long <- merge( df_items, df_responses_long, by='Item')

df_responses_long <- df_responses_long[ order( df_responses_long$fItem, df_responses_long$ID ), ]
df_responses_long$Item <- NULL
names( df_responses_long )[ 1 ] <- 'Item'

###########################
### Distractor Analysis ###
###     A&Y p. 123      ###
###########################

# get a counts of each response by each item and then sort the file
# the following line is code can be read as follows:
# 1. Remove the first column from the dt_Long object, this is the ID column
# 2. Get a count of the responses, by item, call the new count variable N
# 3. Sort the file by Item, then Response

df_distractor <- aggregate( . ~ Response + Item, data=df_responses_long, length )
names( df_distractor )[ 3 ] <- 'Frequency'

df_counts <- aggregate( Frequency ~ Item, data=df_distractor, sum )
names( df_counts )[ 2 ] <- 'N'

df_distractor <- merge( df_distractor, df_counts, by='Item' )
rm( df_counts )

# Compute the item-response response proportions
df_distractor$Proportion <- df_distractor$Frequency / df_distractor$N

# Put the counts and the proportions together in a manner suitable for presentation in a table
df_distractor$Presentation <- paste0( df_distractor$Frequency, ' (', sprintf( '%0.3f', df_distractor$Proportion ), ')' )

# Plot the item responses counts by item
ggplot( df_distractor, aes( Response, Frequency, fill=Response ) ) + geom_bar( stat='identity' ) + 
   labs( x='Item Response', title='Distractor Analysis', subtitle='Frequencies of Item Responses' ) + 
   facet_wrap( ~ Item ) + theme( legend.position='none' ) + ylab( 'Frequency' )

# ggsave( file = 'Distractor.pdf' )
# ggsave( file = 'Distractor.jpg' )

df_distractor <- reshape( df_distractor[ , c( 'Item', 'Response', 'Presentation' ) ],
                          idvar='Item',
                          dir='wide',
                          timevar='Response',
                          v.names='Presentation' )
df_distractor <- df_distractor[ order( df_distractor$Item ), ]
names( df_distractor )[ -1 ] <- unlist( strsplit( names( df_distractor )[ -1 ], '[.]' ) )[ seq( 2, 8, by=2 )]

df_distractor$Item <- as.character( df_distractor$Item )

for ( i in 1:20 ){
   df_distractor$Item[ i ] <- paste0( df_distractor$Item[ i ], ' (', df_answer_key[ 1, i + 1 ], ')' )
}
names( df_distractor )[ 1 ] <- 'Item (Key)'


write.xlsx2(df_distractor, file = outXLSFile, sheetName='Distractor', row.names=FALSE )
######################################################################################################

###################################
### Score the items right/wrong ###
###################################
# which items are Right (1) and which are Wrong (0)

# First let's reshape the dt_answer_key object from wide to long
df_key_long <- reshape( df_answer_key, dir='long', idvar='ID', 
                  v.name='Response', timevar='Item',
                  varying=paste0( 'Item ', 1:20 ), 
                  times=paste0( 'Item ', 1:20 ) )

df_key_long$ID <- NULL
names( df_key_long )[ 2 ] <- 'Key'

# Merge the keys and responses
df_responses_long <- merge( df_responses_long, df_key_long, by='Item' )

# Score the item (0/1)
df_responses_long$Score <- as.integer( df_responses_long$Response == df_responses_long$Key )

# Get the counts of 0/1 by item
df_rw <- aggregate( ID ~ Item + Score, data=df_responses_long[ , c( 'Item', 'Score', 'ID' ) ], length )


df_rw <- reshape( df_rw, dir='wide', idvar='Item', timevar='Score', v.names='ID'   )
names( df_rw )[ 2:3 ]  <- c( '0', '1' )


df_scores_wide <- reshape( df_responses_long[ , c( 'ID', 'Item', 'Score' ) ], dir='wide', 
                     idvar='ID', timevar='Item', v.names='Score' )

names( df_scores_wide )[ -1 ] <- do.call( rbind, strsplit( names( df_scores_wide )[ -1 ], '[.]' ) )[ , 2 ]

df_scores_wide <- df_scores_wide[ , c( 'ID', df_items$Item ) ]

# Compute the total scores
df_scores_wide$Total <- apply( df_scores_wide[ , -1 ], 1, sum )
######################################################################################################

#########################
### Item Difficulties ###
###    A&Y p. 123     ###
#########################

# Compute the item difficulties, p
df_rw$p <- df_rw$`1` / ( df_rw$`0` + df_rw$`1` )

# Item difficulties adjusted for guessing

# m is the number of potential responses
m <- 4

df_rw$p_adj <- df_rw$p + df_rw$p/m 
######################################################################################################

############################
###     Reliability      ###
### Internal Consistency ###
############################



# Spearman-Brown assumes parallel measures
tvSpearmanBrown <- function(Reliability, Parts) {
   retVal <- (Parts * Reliability)/(1 + (Parts - 1) * Reliability)
   return(retVal)
}

# Rulon's method is appropriate if variances are not equal
tvRulon <- function(Delta.Variance, Total.Variance) {
   retVal <- 1 - (Delta.Variance/Total.Variance)
   return(retVal)
}

tvAlpha <- function(Data) {
   K <- ncol(Data)
   Var.Tot <- var(apply(Data, 1, sum))
   Item.Vars <- apply(Data, 2, var)
   retVal <- (K/(K - 1)) * (1 - (sum(Item.Vars)/Var.Tot))
   return(retVal)
}

tvKR20 <- function(Data) {
   K <- ncol(Data)
   Var.Tot <- var(apply(Data, 1, sum))
   p <- apply(Data, 2, mean)
   q <- 1 - p
   Item.Vars <- p * q
   retVal <- (K/(K - 1)) * (1 - (sum(Item.Vars)/Var.Tot))
   return(retVal)
}


df_scores_wide[ , seq( 3, nItems+1, by = 2)]


Odd  <- apply( df_scores_wide[ , paste( 'Item', seq( 1, nItems, by=2 ) ) ], 1, sum )
Even <- apply( df_scores_wide[ , paste( 'Item', seq( 2, nItems, by=2 ) ) ], 1, sum)
Delta <- Odd - Even

( r_split.half <- cor( Odd, Even) )
tvSpearmanBrown(r_split.half, 2)

var(  Odd )
var( Even )
tvRulon(var(Delta), var( df_scores_wide$Total ) )

tvAlpha( df_scores_wide[ , paste( 'Item', 1:20 ) ] )
tvKR20(  df_scores_wide[ , paste( 'Item', 1:20 ) ] )

# Inter-item, and Item-Total Correlations
(IIC <- cor( df_scores_wide[ , c( paste( 'Item', 1:20 ), 'Total' ) ] ))

# Average inter-item correlation
(AIIC <- sapply(1:nItems, FUN = function(x) mean(IIC[  -c(x, nrow( IIC ) ), x] ) ) )

tvItemRest <- function(iCol, dfData) {
   Rest <- apply(dfData[, -iCol], 1, sum)
   retVal <- cor(Rest, dfData[, iCol])
   return(retVal)
}

(vItemRest <- sapply(1:nItems, tvItemRest, df_scores_wide[, 2:( nItems + 1 ) ] ) )

(vAlphaIfDeleted <- sapply( 2:( nItems + 1 ),
                            FUN=function(x) tvAlpha( df_scores_wide[, -c(1, x, 
                                            ncol( df_scores_wide ) ) ] ) ) ) 


# Item-Reliability Index
( iri <- apply( df_scores_wide[ , 2:( nItems + 1 ) ], 2, sd) * IIC[1:nItems, nItems + 1])


tv_pbis <- function( y_bar_0, y_bar_1, s_y, p_i ){
   pbis <- ( y_bar_1 - y_bar_0 ) * sqrt( p_i * ( 1 - p_i ) ) / s_y
   return( pbis )
}

# Point-Biserial p. 38, Allen & Yen
PBis <- sapply( 1:nItems, FUN=function( i ){ p_i  =df_rw$p[ df_rw$Item==paste( 'Item', i ) ];
                                     s_y=sqrt( sum( ( df_scores_wide$Total - mean( df_scores_wide$Total ) )^2 ) / length( df_scores_wide$Total ) );
                                     y_bar_1=mean( df_scores_wide$Total[ df_scores_wide[ , paste( 'Item', i ) ] == 1 ] );
                                     y_bar_0=mean( df_scores_wide$Total[ df_scores_wide[ , paste( 'Item', i ) ] == 0 ] );
                                     tv_pbis( p_i=p_i, s_y=s_y, y_bar_0=y_bar_0, y_bar_1=y_bar_1) } )


# Biserial correlation Eq. 2.32, p. 39, Allen & Yen
tvBiserial <- function(contin, binary) {
   p <- mean(binary)
   q <- 1 - p
   
   Mean.1 <- mean(contin[binary == 1])
   Mean.0 <- mean(contin[binary == 0])
   
   sd.y <- sd(contin)
   
   h <- qnorm(q)
   
   u <- exp(-h * h/2)/sqrt(2 * acos(-1))
   retVal <- p * (1 - p) * (Mean.1 - Mean.0)/sd.y/u
   
   return(retVal)
}

(Bis <- sapply(1:nItems, FUN = function(Item) tvBiserial( contin=df_scores_wide$Total, binary=df_scores_wide[, paste( 'Item', Item ) ] ) ) )


( dfItemStatistics <- cbind(`Item Difficulties`=df_rw$p, p_adj=df_rw$p_adj, 
                           `Alpha if Deleted` = vAlphaIfDeleted, 
                           `Item-Rest Correlation` = vItemRest, 
                           `Item-Total Correlation` = IIC[1:nItems, nItems + 1], 
                           `Average Inter-Item Correlation` = AIIC, 
                           `Item-Reliability Index` = iri, 
                           Biserial = Bis))


tblScores <- table( df_scores_wide$Total )

tblScores <- cbind(Frequency = tblScores, 
                   `Cumulative Frequency` = cumsum(tblScores), 
                   Proportion = prop.table(tblScores), 
                   `Cumulative Proportion` = cumsum(prop.table(tblScores)))
(tblScores <- tblScores[nrow(tblScores):1, ])
write.xlsx(tblScores, file = outXLSFile, sheetName = "Total Scores", append = TRUE)


ggplot( df_scores_wide, aes( Total ) ) + geom_bar() + 
   labs(x = "Total Score", y='Frequency',
        title = "Distribution of Total Scores", 
        subtitle = "With a Normal Distribution") + 
   stat_function(fun = function(x) {
      length( df_scores_wide$Total ) * dnorm(x = x, mean = mean(  df_scores_wide$Total ), sd = sd( df_scores_wide$Total) )
   }, color = "red")
#ggsave(file = "Total Scores.pdf")
#ggsave(file = "Total Scores.jpg")

tvDescriptives <- function(x) {
   return(c(Mean = mean(x, na.rm = TRUE), 
            p_adj = mean(x, na.rm = TRUE) +  (mean(x, na.rm = TRUE)/4), 
            Variance = var(x, na.rm = TRUE), 
            SD = sd(x, na.rm = TRUE), 
            Median = median(x, na.rm = TRUE), 
            Minimum = min(x, na.rm = TRUE), 
            Maximum = max(x, na.rm = TRUE), 
            N = sum(!is.na(x)), 
            Missing = sum(is.na(x))))
}
(dfDescriptives <- data.frame(do.call(rbind, lapply( 2:ncol( df_scores_wide ), FUN = function(iCol) tvDescriptives( df_scores_wide[, iCol ] ) ) ) ) )
row.names(dfDescriptives)[1:nItems] <- paste("Item", 1:nItems)
row.names(dfDescriptives)[nrow(dfDescriptives)] <- "Score"

write.xlsx2(dfDescriptives, file = outXLSFile, append = TRUE, sheetName = "Descriptives")

######################################################################################################

###########################
### Item Discrimination ###
###########################


#As a rule of thumb, in terms of discrimination index,.40 and greater are very good items,.30 to.39 are reasonably good but possibly subject to improvement,.20 to .29 are marginal items and need some revision, below 19 are considered poor items and need major revisionor should beeliminated (Ebel & Frisbie, 1986).

#Ebel, R.L.,& Frisbie, D.A.(1986). Essentials of educational measurement. Englewood Cliffs, NJ:Prentice-Hall

# 0.27 is the threshold suggested by Kelly
(l27 <- floor(nExaminees * 0.27))
(u27 <- nExaminees - l27 + 1)

# Sort the data so we can identify the Lower and Upper groups
df_scores_wide <- df_scores_wide[ order( df_scores_wide$Total ), ]

df_scores_wide$Lower <- ( 1:nExaminees ) %in% 1:l27
df_scores_wide$Upper <- ( 1:nExaminees ) %in% u27:nExaminees

dfSubsample <- df_scores_wide[ df_scores_wide$Lower == 1 | df_scores_wide$Upper == 1, ]

table( Lower = df_scores_wide$Lower, Upper = df_scores_wide$Upper )

# These are the item-discriminations
( P.lower <- apply( dfSubsample[ dfSubsample$Lower == 1, paste( 'Item', 1:nItems ) ], 2, mean))
( P.upper <- apply( dfSubsample[ dfSubsample$Lower == 0, paste( 'Item', 1:nItems ) ], 2, mean))
( D <- P.upper - P.lower)
(P_for_Dmax <- apply(cbind(P.lower, P.upper), 1, mean))
(D.max <- ((2 * (1 - P_for_Dmax)) * (P_for_Dmax >= 0.5)) + ((2 * P_for_Dmax) * 
                                                               (P_for_Dmax < 0.5)))

( D_ratio <- D / D.max )

(dfItemStatistics <- cbind(dfItemStatistics, cbind(P.lower, P.upper, D, D.max, D_ratio, P_for_Dmax)))
write.xlsx(dfItemStatistics, file = outXLSFile, append = TRUE, sheetName = "Item Statistics")


dfSlopes <- do.call(rbind, lstSlopes <- lapply(1:nItems, FUN = function(Item) data.frame(Item, 
                                                                                         aggregate( df_scores_wide[, paste( 'Item', Item )] ~ df_scores_wide$Total, FUN = mean ) ) ) )
names(dfSlopes) <- c("Item", "Score", "Proportion")

ggplot( dfSlopes, aes(x = Score, y = Proportion ) ) + 
   geom_line() + 
   facet_wrap( ~ Item ) +
   ggtitle( 'Item-Characteristic Curves' )
# ggsave(file = "Item-Characteristic Curves.pdf")
# ggsave(file = "Item-Characteristic Curves.jpg")

Lower.IIC <- IIC
Lower.IIC[upper.tri(Lower.IIC, diag = TRUE)] <- NA
Lower.IIC[-c(1, nrow(Lower.IIC)), 1:(nItems - 1)]

write.xlsx(Lower.IIC[-c(1, nrow(Lower.IIC)), 1:(nItems - 1)], append = TRUE, 
           file = outXLSFile, sheetName = "Inter-Item Correlations", showNA = FALSE)

maxIC <- data.frame(SD = apply( df_scores_wide[, paste( 'Item', 1:nItems ) ], 2, sd), iri = iri)
maxIC$Item <- 1:nrow( maxIC )


ggplot(maxIC, aes(iri, SD, label = Item)) + 
   geom_point() + ylab( 'Standard Deviation' ) +
   xlab( 'Item-Reliability Index (IRI)' ) +
   geom_text(hjust = 1.4 )
