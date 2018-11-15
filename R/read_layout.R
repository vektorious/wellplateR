#' @title Reading and Processing of Layout Data
#'
#' @description This function reads data from a layout data frame and processes its content
#'
#' @param layout_df
#'
#' @return output
#'
#' @examples
#'
#' @export

read_layout <- function(inputlayout){
  if (is.null(inputlayout))
    return(NULL)

  plate_layout <- mutate(inputlayout,
                         row = as.numeric(match(toupper(substr(well, 1, 1)), LETTERS)), # convert letter into row identifier
                         column = as.numeric(substr(well, 2, 5))) # extract number of well

  empty_wells <- raw_plate_layout$well[is.na(raw_plate_layout$elicitor) & is.na(raw_plate_layout$genotype)]
  raw_plate_layout <- raw_plate_layout[complete.cases(raw_plate_layout),] #delete rows with NAs

  output <- list("empty_wells" = empty_wells,
                 "plate_layout" = plate_layout,
                 "raw_plate_layout" = raw_plate_layout)
  return(output)
}
