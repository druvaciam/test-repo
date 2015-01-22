source("lminfile_v-2.R")

digits_sign = 4
inputdir <- "input"
outputdir <- "output"
preddir <- 'predict'
predfile <- 'predict.txt'

create_3d_from_rdata <- function() {
  pred <- read.table(paste(getwd(), inputdir, preddir, predfile, sep='/'), header=T, sep='\t')
  x_pred <- names(pred)[-1]
  pred_values <- pred[-1]
  if (as.character(pred[1,1]) != "all") {
    data_file_name_short <- paste0(as.character(pred[1,1]), '.RData')
    data_file_name <- paste(getwd(), outputdir, data_file_name_short, sep='/')
    load(data_file_name)
    write_interact_plot(fit_container, x_pred, pred_values)
  }
  else {
    data_file_names <- list.files(path=paste(getwd(), outputdir, sep='/'), pattern="*.RData", full.names=T, recursive=F)
    for (file_name in data_file_names) {
      load(file_name)
      write_interact_plot(fit_container, x_pred, pred_values)
    }
  }
}

set_wd_from_args <- function() {
  if (!is.na(getargs()[1])) {
    print(getargs()[1])
    newwd <- gsub("\\?", " ", as.character(getargs()[1]), ignore.case =FALSE, fixed=FALSE)
    setwd(newwd)
  }
}

set_wd_from_args()
create_3d_from_rdata()


#source("3d_interact.R")
