# ---------------------------------------------
#  Basic die mechanics functions
#
#  All ...Roll() functions return already-parameterized 'functors'
#  with which you can re-roll the die combination just with f().
#  e.g. rapierSneak <- makeRoll( c(1,2), c(8,6), 2 )
#       will roll 1D8 + 2D6 + 2 ability-modifier
#
# ---------------------------------------------

# Performs a single roll of various dice
rollDice <- function( dieCt = c(1), dieType = c(6), adj= 0, ... )
{
  total <- mapply( function(c,t) { sum( round( runif( c, 0.5, t + 0.5 ) ) ) },
                   dieCt,
                   dieType )
  sum(total) + adj
}


#' @title makeRoll die-roll closure function
#'
#' @description
#' \code{makeRoll} creates a parameterless function that sums predefined
#'  sets of dice along with a constant adjustment.
#'
#' @param dieCt a list of counts of dice
#' @param dieType a list of dice sizes, e.g. 8 for a 1D8 die
#' @param adj a constant to also add to the sum of die-rolls
#' @return a parameterless function that will make the roll when called
#'
#' @examples
#'   threeD8    <- makeRoll( 3, 8 )
#'   \code{#} To your D20, add an extra 1D4 and your 6 proficiency + ability bonus
#'   blessedAtk <- makeRoll( c(1,1), c(20,4), 6 )
#'
#' @export
#' @seealso \code{\link{multiRoll}}
makeRoll <- function( dieCt, dieType, adj=0, ... )
{
  stopifnot( NROW(dieCt) == NROW(dieType) )

  # The force() function tells 'R' not to lazy-evaluate values,
  # which would otherwise repeat the final-value of a loop, say,
  # or otherwise skip expected executions.
  force( rollDice )

  # Return a function that 'just knows what to do'
  # in a self-contained operation.
  function(...) {  rollDice( dieCt, dieType, adj, ... ) }
}


#' @title constant-value closure function
#' @description
#' \code{constRoll} is a roll that always returns a predefined constant value.
#' This is useful in composing other roll functors.
#' @param adj the constant the resulting functor should return
#' @return a parameterless function that yields the adj constant
#' @export
constRoll <- function( adj )
{
  function(...) { adj }
}


#' @title composite die-roll closure function
#'
#' @description
#' \code{multiRoll} creates a parameterless functor that sums a list of
#' other rolls, e.g. composite effects from weapons and magic.
#' @param rollFuncList of die-roll functors to combine
#' @param evalFunc optional combining operation (default is sum)
#' @return a parameterless function that will make the combined roll
#'
#' @examples
#'   # Short-sword two-weapon fighting with for 3rd level rogue
#'   shortSwordTwFRoll <- multiRoll(  c(makeRoll(1,6,2), makeRoll(1,6,0)) )
#'   advantageAttack   <- multiRoll(  rep(
#'
#' @export
#' @seealso \code{\link{multiRoll}}
multiRoll <- function( rollFuncList, evalFunc = sum )
{
  # First lapply evaluates the list of rolls
  # unlist is needed to flatten lapply's list of single-element lists
  # evalfunc does something to that list of values, e.g. max or sum.
  function(...) { evalFunc( unlist( lapply( rollFuncList, function(f) {f()} ) ) ) }
}


#' @title Difficulty-Check roll
#'
#' @description
#' \code{dcCheck} makes a D20 roll against a difficulty-value.
#' @param dc a difficulty-check value to compare against
#' @param mod an optional modifier value (e.g. 10 for divine-guidance)
#' @param checkRoll an optional roll functor, defaults to 1D20
#' @return Boolean success or failure roll
#'
#' @examples
#'   saveVsPoisonCon13 <- dcCheck( 15, 1 )
#'   dwarfVsPoison <- dcCheck( 15, 3, withAdvantage() )
#'
#' @export
dcCheck <- function( dc, mod=0, checkRoll=NULL )
{
  if( is.null(checkRoll) )
  {
    checkRoll = makeRoll( 1, 20 )
  }
  function(...) { checkRoll() + mod >= dc }
}


# Allow differing, independent success- and fail-outcomes, e.g. attack and dmg rolls
ifelseRoll <- function( checkFunc, successFunc, failFunc=constRoll(0) )
{
  function(...) { if( checkFunc() ) { successFunc() } else { failFunc() } }
}

#' @title Roll, validate, and re-roll
#'
#' @description
#' \code{rollCheckElse} examines the success of a first roll and either
#' uses the first value or makes an alternative roll.
#' @param rollFunc is the initial roll
#' @param checkFunc tests acceptance of the initial roll's value
#' @param altFunc is used if the checkFunc returns FALSE
#' @return Either the rollFunc value or altFunc value
#'
#' @examples
#'   # Use the D20 roll but re-roll on 1-2, and add 6 attack modifiers
#'   greatWeapFight <- rollCheckElse( makeRoll(1,20),
#'                                    function(r) { r > 2 } )
#'   myAttack <- multiRoll( c( greatWeapFight(), constRoll( 6 ) ) )
#'
#' @export
rollCheckElse  <- function( rollFunc, checkFunc, altFunc=NULL )
{
  if( is.null( altFunc ) )
  {
    altFunc = rollFunc
  }

  function(...) {
    x <- rollFunc()
    if(checkFunc(x) ) { x }
    else              { altFunc() } }
}

# Allow differing, dependent success- and fail-outcomes, e.g. half-dmg DC
# The reduceFunc must accept an argument (the nominalFunc output)
ifelseReduce  <- function( checkFunc,
                           nominalFunc,
                           reduceFunc=NULL )
{
  if( is.null( reduceFunc ) )
  {
    reduceFunc <- function(v) { round( v * 0.5 ) }
  }

  force( reduceFunc )

  function(...)
  { n <- nominalFunc()
    if( checkFunc() ) { n <- reduceFunc(n) }
    n
  }
}


# Create a sequence of n samples of the given sampFunc
genSamples <- function(n, sampFunc )
{
  unlist( lapply( 1:n, function(n) { sampFunc() } ) )
}


# ---------------------------------------------
#  Evaluate statistics of various game choices.
#
# ---------------------------------------------
appendDf <- function( nameList, samplesList )
{
  rbind(
    mapply( function(n,d) { data.frame( Exposure=n, Data=d ) },
            nameList, samplesList,
            SIMPLIFY = FALSE, USE.NAMES = FALSE ) )
}


abSamples <- function( n, rollA, rollB )
{
  dfA <- data.frame( Exposure="Control",   Data=genSamples( n, rollA ) )
  dfB <- data.frame( Exposure="Treatment", Data=genSamples( n, rollB ) )
  df  <- rbind( dfA, dfB );
  #df$ControlMedian   <- median( df$Control )
  #df$TreatmentMedian <- median( df$Treatment )
  df
}




showExp <- function( df, title="AB Densities" )
{
    requireNamespace("ggplot2")
    cplt <- ggplot2::ggplot( data=df, ggplot2::aes(x=Data, group=Exposure) ) +
      ggplot2::scale_x_discrete()

    if( requireNamespace("ggthemes", quietly = TRUE) )
    {
        cplt <- cplt + ggthemes::theme_fivethirtyeight()
    }

    cplt <- cplt +
      ggplot2::geom_histogram( binwidth=1, alpha=0.5, color="black",
                               position=ggplot2::position_identity(),
                               ggplot2::aes(fill=Exposure, y=..density..) ) +
        # geom_density(   aes(color=Exposure, linetype=Exposure) ) +
      ggplot2::geom_vline( data=aggregate( Data ~ Exposure, data=df, mean),
                           ggplot2::aes(xintercept=Data, group=Exposure, color=Exposure),
                           linetype="longdash", size=1.5 ) +
      ggplot2::labs( title=title, x="Values" )

    print(cplt)
}


# ---------------------------------------------
#  Typical game- and combination-rolls
#
# ---------------------------------------------

#' Calculates the probability you will hit a targetAC
#' given proficiency-bonus, ability-modifier, and misc adjustments
#' (e.g. magic-bonus)
hitOdds <- function( targetAC, prof, ability, misc = 0 )
{
  p <- (21 - targetAC + prof + ability + misc) / 20
  max( 0.05, min( 0.95, p ) )
}

#' Calculates the effective average damage of your attacks
#' given a nominal damange, hitOdds, and critical-odds.
#' Be sure to add ability and misc (magic) bonuses to avgDmg,
#' e.g. effectiveHitDmg( mean(1:6)+3, hitodds( 17, 4, 3 ) )
#' for a proficiency 4 rogue with DEX mod 3 dmg and attack bonuses.
effectiveHitDmg <- function( avgDmg, odds, critOdds=0.05 )
{
  (avgDmg * odds) + (avgDmg * critOdds)
}

toHit <- function( ac, proficiency, attribMod )
{
  modRoll = makeRoll( 1, 20, (proficiency + attribMod) )
  function(...)
  {
    modRoll() >= ac
  }
}

attribRoll <- function()
{
   vals <- sort( genSamples( 4, makeRoll( 1,6,0 ) ), decreasing=TRUE )
   sum( vals[ 1:3 ] )
}


chargenRoll <- function()
{
  genSamples( 6, attribRoll )
}


#' @title Advantage or disadvantage rolls
#'
#' @description
#' \code{withAdvantage} picks the maximum of two rolls of the rollFunc,
#' or if disAdv=TRUE the minimum.
#' @param rollFunc optionally is roll to function to repeat (default D20)
#' @param disAdv set TRUE picks the min of the two rolls (default FALSE)
#' @return The best (or worst) or two calls to rollFunc
#'
#' @examples
#'   rogueHiddenAttack <- withAdvantage( makeRoll(1,20,5) )
#'   blindAttack       <- withAdvantage( makeRoll(1,20,5), disAdv=TRUE )
#'
#' @export
withAdvantage <- function( rollFunc=NULL, disAdv=FALSE )
{
  if( is.null(rollFunc) )
  {
    rollFunc = makeRoll( 1, 20 )
  }
  evalFunc <- if( disAdv ) { min }
              else         { max }

  function() { multiRoll( c(rollFunc, rollFunc), evalFunc )() }
}



# ---------------------------------------------
#  Specific examples (add to a Vignette?)
# ---------------------------------------------

# The Savage Attack feat re-rolls damage and takes the highest
savageRoll <- function( rollFunc, proficiency )
{
  # Composing two multiRolls means we have a functor returning a functor, not a value,
  # hence we invoke it once to get the expected value-returning functor behavior.
  function()
  {
    multiRoll(
        c(withAdvantage(rollFunc), constRoll(proficiency) ), sum )()
  }
}


# Treat any < 10 skill-check roll as if it were a 10.
reliableTalentRoll <- function( proficiency = 0, rollFunc=makeRoll(1,20,0), minRoll=10 )
{
  # Composing two multiRolls means we have a functor returning a functor, not a value,
  # hence we invoke it once to get the expected value-returning functor behavior.
  function()
  { multiRoll(
        c( multiRoll( c(makeRoll(1,20,0), function(){minRoll}), max ),
           constRoll(proficiency) ),
        sum )()
  }
}

shortSwordSneak   <- makeRoll( 3, 6, 2 )

rapierSneak       <- makeRoll( c(1,2), c(8,6), 2 )

savageRapierSneak <- savageRoll( makeRoll( c(1,2), c(8,6), 0 ), 2 )



# Sanity check the code
d20 <- makeRoll( 1, 20, 0 )
adv <- withAdvantage( d20 )
d20AdvDf <- abSamples( 10000, d20, adv )
showExp( d20AdvDf, "Advantage vs. Straight ToHit" )


longBowAt5  <- ifelseRoll( dcCheck( 12, 5 ), makeRoll( 1, 8, 2 ) )
longBowSS   <- ifelseRoll( dcCheck( 12, 0 ), makeRoll( 1, 8, 12 ) )
shouldI <- abSamples( 2000, longBowAt5, longBowSS )
showExp( shouldI, "Normal v SharpShooter" )


