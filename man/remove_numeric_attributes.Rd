% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/remove_numeric_attributes.R
\name{remove_numeric_attributes}
\alias{remove_numeric_attributes}
\title{Remove Attributes from Numeric Columns in a Data Frame}
\usage{
remove_numeric_attributes(df)
}
\arguments{
\item{df}{A `data.frame` object from which attributes of numeric columns will be removed.}
}
\value{
A `data.frame` with attributes removed from all numeric columns.
}
\description{
Iterates over each column in the provided data frame. If a column is numeric
and has attributes, this function removes those attributes by converting the column
to a basic numeric vector. This is particularly useful for cleaning data frames
after operations that may add undesired attributes to numeric columns, such as
aggregations or merges.
}
\examples{
df <- data.frame(a = I(1:3), b = c("x", "y", "z"), c = I(rnorm(3)))
cleaned_df <- remove_numeric_attributes(df)
str(cleaned_df)

}
