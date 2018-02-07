#' Caluclate RFM
#'
#' @param data - a data table
#' @param r - weight recency
#' @param f - weight frequency
#' @param m - weight monetary
#' @details
#' \code{data} contains ...
#' @return returns a data table with the weighted rfm scores for each customer

RFMfunction <- function(data, weight_recency=1, weight_frequency=1, weight_monetary=1){
  require(data.table)
  require(lubridate)
  require(Hmisc)

  # Ensure that the weights add up to one
  weight_recency2 <- weight_recency/sum(weight_recency, weight_frequency, weight_monetary)
  weight_frequency2 <- weight_frequency/sum(weight_recency, weight_frequency, weight_monetary)
  weight_monetary2 <- weight_monetary/sum(weight_recency, weight_frequency, weight_monetary)

  # RFM measures
  max.Date <- max(data$TransDate)
  temp <- data[,list(
    recency = as.numeric(max.Date - max(TransDate)),
    frequency = .N,
    monetary = sum(PurchAmount)/.N),
    by="Customer"
    ]

  # RFM scores
  temp <- temp[,list(Customer,
                     recency = as.numeric(cut2(-recency, g=3)),
                     frequency = as.numeric(cut2(frequency, g=3)),
                     monetary = as.numeric(cut2(monetary, g=3)))]

  # Overall RFM score
  temp[,finalscore:=weight_recency2*recency+weight_frequency2*frequency+weight_monetary2*monetary, by=Customer]

  # RFM group
  temp[,group:=round(finalscore)]

  # Return final table

  return(temp)
}
