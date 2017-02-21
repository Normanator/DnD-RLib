#' Experience-points by Challenge-rating.
#'
#' Representative XP worth of monsters by challenge-rating.
#'
#' @format A data frame with 27 rows and 2 variables:
#' \describe{
#'   \item{CR}{challenge-rating}
#'   \item{XP}{experience-points}
#'   ...
#' }
#' @source Monster Manual 5th Edition
"monsterCRExp"

#' Experience-point multipliers for multiple-creature encounters.
#'
#' Difficulty increases super-linearly with the number of opponents,
#' at certain bands.
#'
#' @format A data frame with 15 rows and 2 variables:
#' \describe{
#'   \item{Baddies}{number of opponents}
#'   \item{Multiplier}{multiplicative factor of each opponent's XP}
#'   ...
#' }
#' @source Dungeon Master's Guide 5th Edition
"monsterMultiCR"


#' Target experience-point budgets per player.
#'
#' A dataset of character level experience-point targets for various difficulty regimes.
#'
#' @format A data frame with 80 rows and 3 variables:
#' \describe{
#'   \item{CharLevel}{character-level of player}
#'   \item{Difficulty}{encounter difficulty factor: Easy, Medium, Hard, Deadly}
#'   \item{Threshold}{budget-target this player can absorb, in XP}
#'   ...
#' }
#' @source Dungeon Master's Guide 5th Edition
"xpThresholds"