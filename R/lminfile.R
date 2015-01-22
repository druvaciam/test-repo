lminfile <- function(xcount = 2, yparams = c("Result"),
                     filename = "t10_mouth.txt") {
  #xparams <- c("KFK", "Kortizol", "LN", "Mochevina", "Testosteron",
  #             "Gemaglobin", "Alt", "Ast", "Glukoza", "Leicociti", "Tg")
  xparams <- c("ÄÀÒÀ", "ÀÏ1", "ÀÏ2", "ÏÀÍÎ1", "ÏÀÍÎ2", "ÏÀÍÎ3", "GB1", "GB2", "GB3", "ìî÷åâèíà", "ìî÷åâèíà.âå÷åð", "ÊÔÊ", "ãëþêîçà", "ÀÑÒ", "ÀËÒ", "ãåì.ò", "ãåì.í", "L.N", "TG")
  
  splitedname <- unlist(strsplit(filename, "[.]"))
  outfilename <- paste(splitedname[1], "_out", ".", splitedname[2], sep = "")
  
  data<-read.table(filename, header = T, sep = "\t")
  data$FIO <- as.character(data$FIO)
  
  xchooseindexes <- combn(length(xparams), xcount, simplify = FALSE)
  lms <- c()
  newlm <- NULL
  sbysex <- split(data, data$sex)
  for (s in sbysex) {
    for (yparam in yparams) {
      for (xindexes in xchooseindexes) {
        df <- data.frame(s[, c(yparam, xparams[xindexes])])
        #df <- data.frame(data[, c(yparam, xparams[xindexes])])
        df <- df[complete.cases(df), ]
        if (nrow(df) > 5) {
          newlm <- NULL
          if (xcount == 1) {
            newlm <- lm(formula = df[, yparam] ~ df[, 2])
          }
          if (xcount == 2) {
            newlm <- lm(formula = df[, yparam] ~ df[, 2] + df[, 3])
            #newlm <- lm(formula = df[, yparam] ~ sqrt(df[, 2]) + sqrt(df[, 3]))
                        #+ df[, 2] + df[, 3])
          }
          else if (xcount == 3) {
            newlm <- lm(formula = df[, yparam] ~ df[, 2] + df[, 3] + df[, 4])
            #newlm <- lm(formula = df[, yparam] ~ sqrt(df[, 2]) + sqrt(df[, 3]) + sqrt(df[, 4]))
                        #+ df[, 2] + df[, 3] + df[, 4])
          }
          else if (xcount == 4) {
            newlm <- lm(formula = df[, yparam] ~ df[, 2] + df[, 3] + df[, 4] + df[, 5])
          }
          outstr <- paste(s$sex[[1]], ":", yparam, "~")
          #outstr <- paste("m + f", ":", yparam, "~")
          eq <- paste("(", as.character(newlm$coef[1]), ")", "+",
                      #"(", as.character(newlm$coef[2]), ")", "*", "sqrt(x)", "+",
                      #"(", as.character(newlm$coef[3]), ")", "*", "sqrt(y)", "+",
                      "(", as.character(newlm$coef[2]), ")", "*", "x1", "+",
                      "(", as.character(newlm$coef[3]), ")", "*", "x2",
                      sep = "")
          rsqr <- paste("R-squared:", as.character(summary(newlm)$r.squared))
          for (x in xparams[xindexes]) {
            sign <- "+"
            if (x == xparams[xindexes[1]]) {sign <- character()}
            outstr <- paste(outstr, sign, x)
          }
          if (summary(newlm)$r.squared > 0.6
              ) {
            #print(outstr)
            #print(rsqr)
            #print(eq)
            print(c(outstr, rsqr, eq, ""))
            #print(summary(newlm))
          }
        }
      }
    }
  }
  
}
