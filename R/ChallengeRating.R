
# The following were 'sourced' and emitted for the package, e.g. via
# devtools::use_data( monstrCRExp, internal=FALSE )
#
# What experience-point values (XP) are various challenge-rating monsters (CR).
# (These are approximate; the Monster Manual may show different values for the same CR)
#monsterCRExp <- data.frame( CR=c( 0.125, 0.25, 0.5, 1:24 ),
#                            XP=c(    25,    50,   100,
#                                    200,   450,   700,  1100,  1800,
#                                   2300,  2900,  3900,  5000,  5900,
#                                   7200,  8400, 10000, 11500, 13000,
#                                  15000, 18000, 20000, 22000, 25000,
#                                  27500, 30000, 32500, 36500 ) )

# What multiplicative-difficulty do multiple opponents make?
#monsterMultiCR <- data.frame( Baddies=1:15,
#                              Multiplier=c( 1, 1.5,
#                                            rep(2,4), rep(2.5,4), rep(3,4), 4 ) )

# What is a good target-experience value for an individual player at various levels.
# A party's target is the sum of targets for individuals.
# (As a heuristic, a player should end his day with a long-rest after ~3x 'Deadly' XP)
#xpThresholds <- data.frame( CharLevel=rep( 1:20, each=4 ),
#                            Difficulty=rep( c('Easy', 'Medium', 'Hard', 'Deadly'), 20 ),
#                            Threshold= c(
#                              c(   25,    50,    75,   100 ),  #  1
#                              c(   50,   100,   150,   200 ),  #  2
#                              c(   75,   150,   225,   400 ),  #  3
#                              c(  125,   250,   375,   500 ),  #  4
#                              c(  250,   500,   750,  1000 ),  #  5
#                              c(  300,   600,   900,  1400 ),  #  6
#                              c(  350,   750,  1100,  1700 ),  #  7
#                              c(  450,   900,  1400,  2100 ),  #  8
#                              c(  550,  1100,  1600,  2400 ),  #  9
#                              c(  600,  1200,  1900,  3600 ),  # 10
#                              c(  800,  1600,  2400,  3600 ),  # 11
#                              c( 1000,  2000,  3000,  4500 ),  # 12,
#                              c( 1100,  2200,  3400,  5100 ),  # 13,
#                              c( 1250,  2500,  3800,  5700 ),  # 14,
#                              c( 1400,  2800,  4300,  6400 ),  # 15,
#                              c( 1600,  3200,  4800,  7200 ),  # 16,
#                              c( 2000,  3900,  5900,  8800 ),  # 17,
#                              c( 2100,  4200,  6300,  9500 ),  # 18,
#                              c( 2400,  4900,  7300, 10900 ),  # 19,
#                              c( 2800,  5700,  8500, 12700 )   # 20
#                            ) )


#' @title partyEncounter target XP calculator
#'
#' @description
#' \code{partyEncounter} computes the target XP value for an encounter with foes
#'  given a party of adventurers at different levels.
#'
#' @param cts A scalar or list of adventurer counts
#' @param lvls scalar or list (depending on cts) of adventurer levels
#' @param difficulty The encounter's inherent stress:
#'        Easy, Medium, Hard, or Deadly (or All)
#' @return The experience-points the combined monsters can sum to
#'         or a named-list for difficulty='All'.
#' @examples
#'    partyEncounter( c(3,1), c(5,7), 'Hard' )
#'    partyEncounter( 4, 3, 'All' )
#' @export
#' @seealso \code{\link{listEncounters}}
partyEncounter <- function( cts, lvls, difficulty='Medium' )
{
    if( difficulty == 'All' )
    {
      sapply( c('Easy','Medium','Hard','Deadly'),
              function(d) { partyEncounter( cts, lvls, d ) } )
    }
    else
    {
        xtsub <- with( xpThresholds,
                       xpThresholds[ (CharLevel %in% lvls) & Difficulty==difficulty, ] )
        psum  <- mapply( function(c,lv) { with( xtsub,
                              sum( c * xtsub[ CharLevel==lv, "Threshold" ] ) ) },
                         cts, lvls )
        sum( psum )
    }
}


closestCr <- function( targetXp )
{
  xpErr <- abs( monsterCRExp[ , "XP" ] - targetXp )
  idx   <- which.min( xpErr )
  monsterCRExp[ idx, ]
}


closestCrAtCt <- function( targetXp, baddieCt )
{
  mltp <- monsterMultiCR[ monsterMultiCR$Baddies==baddieCt, 'Multiplier' ]
  ccr <- closestCr( targetXp / (mltp * baddieCt) )[['CR']]
  res  <- mltp * baddieCt * monsterCRExp[ monsterCRExp$CR==ccr, 'XP' ]
  data.frame( Baddies=baddieCt, CR=ccr, XP=res )
}


# Enumerates the combinations of monster counts and challenge-ratings
# that could be used to achieve an encounter of a targetXp amount.
# (See partyEncounter(...) for targetXp)
# Example:
#    For a boss-battle with minions of 6800xp (deadly for 4 7th level players),
#  the DM decides to split the XP to ~4500 and ~2300
#    listEncounters( 4500 )
#  from which he picks 2 CR 5s (5400) and
#    listEncounters( 1400 )
#  from which 6 CR 1/2 round things out nicely.
#' @title listEncounters matching a targetXp
#'
#' @description
#' \code{listEncounters} Enumerates the combinations of foe counts and
#'  challenge-ratings that yield approximately the given  target XP value.
#'
#' @param targetXp A scalar experience-point target for the encounter
#' @return A dataframe of Baddies, CR, XP from 1 to 15 Baddies.
#' @examples
#'    # A deadly encounter for 4 7th-level players
#'    listEncounters( 6800 )
#'
#'    # A boss-and-minions battle of 4500(boss) and 2300(minions)
#'    # (This example won't compensate for $Boss$Baddies + $Minions$Baddies
#'    #  posing more difficulty than either $Baddies count alone, so be kind,
#'       pick rows that sum to 66-75% targetXp, but still award targetXp)
#'    sapply( c(Boss=4500, Minions=2300),
#'            function(x) { listEncounters(x) },
#'            simplify=FALSE )
#' @export
#' @seealso \code{\link{partyEncounter}}
listEncounters <- function( targetXp )
{
   # REVIEW: There has to be a better way to make a dataframe of this!
   encs <-
     rbind( data.frame(),
            vapply( monsterMultiCR$Baddies,
                    function(bct)  {  closestCrAtCt( targetXp, bct ) },
                    FUN.VALUE=data.frame(Baddies=1, CR=1, XP=1),
                    USE.NAMES = FALSE )
     )

   #transpose to rows (which you'd think rbind would do)
   enct <- t(encs)

   # filter to possibilities within 133% of nominal
   encsane <- enct[ enct[,3] < targetXp * 1.33, ]

   # Re-apply names to the columns (as FUN.VALUE should have done)
   data.frame( Baddies = unlist(encsane[,1]),
               CR      = unlist(encsane[,2]),
               XP      = unlist(encsane[,3]) )
}

