##############################################################################
#
# Function paste.col
#
# Converts a row to a string, "pasting" the columns together.
#
#############################################################################

paste.col <- function(x) {

  # If it's just a vector, return each value as a string.

  if (is.null(dim(x))) {

    return(paste(as.character(x)))

  } # End if

  # Otherwise, it's a matrix.

  # Get the value of the first column in character form

  out <- paste(as.character(x[, 1]))

  # Add on each of the rest of the columns

  for (i in 2:ncol(x)) {

   out <- paste(out, as.character(x[, i]))

  } # End for

  return(out)

} # End paste.col

#############################################################################
#
# Function find.matching.rows
#
#
#   AUTHOR:  John R. Wallace (John.Wallace@noaa.gov)
#   REVISED: Andi Stephens, 2010.
#
#   Takes two tables with a shared primary key, and
#   returns the rows of the second table for which the
#   keys match those of the first.
#
#   NOTE:  The way this is used assumes that the second table is a
#          superset of the first (i.e., that each value is matched).
#
#   Changes:
#
#        Changed name from original "match.f" to "find.matching.rows".
#
#        Removed sub-function 'paste.col' and made it standalone.
#
#        The matching function no longer modifies it's inputs, just
#        returns the values to be 'cbound' in the calling function.
#
#
# Using the primary keys in columns named 'findex' and 'tindex', finds the
# matching values for 'file' in 'table' and returns 'table' column(s) 'tcol'.
#
# Note that no test is made to determine if there are unmatched rows.
#
#############################################################################

find.matching.rows <- function(file, table, findex = 1, tindex = 1, tcol = 2, round. = T) {

  # Coerce a vector argument into a matrix

  if (is.null(dim(file))) {  dim(file) <- c(length(file), 1) }

  # If the primary keys are numeric, round them.

  if (round.) {

    if (is.numeric(file[, findex])) { file[, findex] <- round(file[, findex]) }

    if (is.numeric(table[, tindex])) { table[, tindex] <- round(table[, tindex]) }

  } # End if round.

  # Convert the indices to character strings for comparison, and get the
  # positions of the 'file' values in the 'table' values.

  matched.rows = match(paste.col(file[, findex]), paste.col(table[, tindex]))

  # Return the 'tcol' values in the rows of the 'table' that matched.

  return(table[matched.rows, tcol, drop = FALSE])

} # End function find.matching.rows


