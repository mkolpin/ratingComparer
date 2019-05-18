get_rating_valid <- function(x, gold, raters, grouping_vars = c("id", "condition")){
  #load required packages if not already loaded
  if(!require(dplyr)) {
    print("Package dplyr is missing, please install")
    break
  }
  data = x
  
  col_names = c()
  #loop over all raters
  for (rater in raters){
    other_raters <- raters[!raters %in% rater]
    #create new columns and attach name to list
    col_name_new <- paste(rater,"reliability", sep="_")
    col_names <- c(col_names, col_name_new)
    data <- add_reliability_column(data, gold, rater, col_name_new, other_raters)
  }
  #check if gold has rated this set
  data <- add_gold_rated_column(data, gold)
  
  #select grouping and reliability columns
  data <- data %>% select(grouping_vars, col_names, "gold_has_rated")
  #group by grouping variables, averaging percentages
  grouped <- data %>% group_by(.dots = grouping_vars) %>% summarise_all(list(~mean(., na.rm = TRUE)))
  
  #after grouping, fields without anything to group are set to NaN, the next steps replace these with NA
  is.nan.data.frame <- function(x)
    do.call(cbind, lapply(x, is.nan))
  grouped[is.nan(grouped)] <- NA
  
  return(grouped)  
}


add_gold_rated_column <- function(data, goldName){
  require(dplyr)
  dataWithGoldRatedColumn <- data %>%
    rowwise %>%
    mutate("gold_has_rated" = !is.na(!!as.name(goldName)))
  return(dataWithGoldRatedColumn)
}


add_reliability_column <- function(data, goldName, raterName, newColumnName, ...){
  require(dplyr)
  otherRaters <- list(...)
  newArgs <- lapply(otherRaters, as.name)
  dataWithNewColumns <- data %>% 
    rowwise %>% 
    mutate(!!newColumnName := 
             do.call(function(...) 
               get_diff_to_gold_if_available(!!as.name(goldName),!!as.name(raterName),...), 
               newArgs)
  )
  return(dataWithNewColumns)
}



get_diff_to_gold_if_available <- function(goldRating, rating, ...){
  if(!is.na(goldRating)) 
    return(check_diff(rating, goldRating))
  else 
    return(get_mean_of_diffs(rating, ...))
}

check_diff <- function(rating1, rating2){
  if (is.na(rating1) | is.na(rating2)){
    return(NA)
  }
  else{
    if( abs(rating1-rating2)<=1 )
      return(100)
    else
      return(0)
  }
}


get_mean_of_diffs <- function(rating, ...){
  diffs <- c()
  other_ratings <- list(...)
  for (otherRating in other_ratings){
    diffs <- c(diffs, check_diff(rating, otherRating))
  }
  diffs <- diffs[!is.na(diffs)]
  result <- mean(diffs)
  return(ifelse(length(diffs)==0, NA, result))
}

