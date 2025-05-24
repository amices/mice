# employee selection data - Enders, 2010, p. 2018

employee <- data.frame(
  IQ = c(
    78, 84, 84, 85, 87, 91, 92, 94, 94, 96, 99, 105, 105, 106, 108, 112,
    113, 115, 118, 134
  ),
  wbeing = c(
    13, 9, 10, 10, NA, 3, 12, 3, 13, NA, 6, 12, 14, 10, NA, 10,
    14, 14, 12, 11
  ),
  jobperf = c(rep(NA, 10), 7, 10, 11, 15, 10, 10, 12, 14, 16, 12)
)
devtools::use_data(employee)
