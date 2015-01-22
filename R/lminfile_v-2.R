#install.packages('ggplot2')
#install.packages('scatterplot3d')
#install.packages('lattice')
#install.packages('rgl')
library(ggplot2)
library(scatterplot3d)
library(lattice)
library(rgl)

digits_sign = 4
min_valid_count <- 5
inputdir <- 'input'
outputdir <- 'output'
modes <- c('simple', 'sqrt', 'ln', 'sqr', 'mult', 'mult+sqrt')
window3d_width=1000
experiment_points_col='blue'
shape_interact3d_col='blue'
predict_ellipse_col='red'
lines_to_axis_col <- 'chartreuse4'
shape_interact3d_alpha=0.2
conf <- .9

get_parent_dir <- function(filename) {
  temp <- unlist(strsplit(filename, '[/]'))
  dir <- paste(temp[c(-(length(temp)-1), -length(temp))], collapse='/')
  if (!grepl(':', dir)) {dir <- file.path(getwd(), dir)}
  dir
}

get_full_output_dir <- function(filename) {
  file.path(get_parent_dir(filename), outputdir)
}

create_output_dir <- function(filename) {
  dir.create(get_full_output_dir(filename), showWarnings = FALSE)
}

get_output_name_prec <- function(filename) {
  temp <- unlist(strsplit(filename, '[/]'))
  file.path(get_full_output_dir(filename), unlist(strsplit(temp[length(temp)], '[.]'))[1])
}

get_xparams <- function(data, yparams) {
  names(data)[2:(length(names(data))-length(yparams))]
}

get_fit_file_name <- function(filename, xcount, rsqr_border) {
  paste0(get_output_name_prec(filename), '_', 'eq', xcount, 'param', '_', rsqr_border, '_out', '.txt')
}

get_pred_file_name <- function(filename, xcount, rsqr_border) {
  paste0(get_output_name_prec(filename), '_', 'eq', xcount, 'param', '_', rsqr_border, '_pred', '.txt')
}

get_plot_name <- function(filename, formula_text, Radj_border) {
  paste0(get_output_name_prec(filename),'_', formula_text, '_', Radj_border, '_plot.png')
}

get_Radj_text <- function(lmfit) {
  paste('Adjusted R-squared:', as.character(signif(summary(lmfit)$adj.r.squared, digits=digits_sign)))
}

get_choosed_df <- function(data, y_choosed, x_choosed) {
  df <- data.frame(data[, c(y_choosed, x_choosed)])
  df <- df[complete.cases(df), ]
  df
}

create_png_plots <- function(df, plotname, mode, lmfit, y_choosed, x_choosed, xcount) {
  outformula <- get_outformula(y_choosed, x_choosed, mode=mode)
  if (xcount == 1)
  {
    #plot(df[,2], df[,'Result'])
    #abline(lmfit)
    qplot(df[,2], df[,'Result'], geom=c('point', 'smooth'), main=outformula,
          ylab=y_choosed, xlab=x_choosed[1],  method='lm', formula=y~x)
    ggsave(plotname)
  }
  if (xcount == 2 & mode == 'simple')
  {
    png(plotname)
    s3d <-scatterplot3d(df[,2], df[,3], df[, y_choosed], pch=16, highlight.3d=TRUE, lty.hplot=3,
                        type='h', main=outformula, zlab=y_choosed, xlab=x_choosed[1],
                        ylab=x_choosed[2])
    s3d$plane3d(lmfit)
    dev.off()
  }
  else if (xcount == 2)
  {
    xyz <- get_xyz(df=df, lmfit=lmfit, mode=mode)
    png(plotname)
    persp(xyz$x, xyz$y, xyz$z, col='grey', theta=135, phi=30, ltheta=-120, box=T, axes=T, ticktype='detailed',
          xlab=x_choosed[1], ylab=x_choosed[2], zlab=y_choosed, main=outformula)
    #wireframe(z~x*y, data=xyz, scales=list(arrows=F), screen=list(z=30, x=-60), colorkey=T, drape=T)
    dev.off()
  }
}

get_xyz <- function(df, lmfit, mode, x_count=50) {
  x <- seq(range(df[,2])[1], range(df[,2])[2], len=x_count)
  y <- seq(range(df[,3])[1], range(df[,3])[2], len=x_count)
  z <- outer(x, y, getfunc(mode, lmfit))
  xyz <- list(x=x, y=y, z=z)
  xyz
}

lminfile <- function(xcount = 2, yparams = c('Result'),
                     filename = file.path('input', '9-11.txt'), rsqr_border = .8) {
  create_output_dir(filename)
  data<-get_file_data(filename)
  xparams <- get_xparams(data, yparams)
  
  xchooseindexes <- combn(length(xparams), xcount, simplify = FALSE)
  
  fit_container_base <- list()
  for (yparam in yparams) {
    for (xindexes in xchooseindexes) {
      x_choosed <- xparams[xindexes]
      df <- get_choosed_df(data, yparam, x_choosed)
      cur_modes = NULL
      if (xcount == 2) { cur_modes = modes }
      else { cur_modes = modes[1]}
      for (mode in cur_modes) {
        if (nrow(df) > min_valid_count) {
          lmfit <- lm(formula = getformula(xcount, df, mode=mode, x_choosed, yparam), data=df)
          if (summary(lmfit)$adj.r.squared > rsqr_border) {
            fit_element <- list(lmfit=lmfit, mode=mode, xcount=xcount, x_choosed=x_choosed,
                                y_choosed=yparam, df=df, filename=filename, Radj_border=rsqr_border)
            fit_container_base[[length(fit_container_base)+1]] <- fit_element
          }
        }
      }
    }
  }
  
  write_pred_file(fit_container_base)
  write_fit_file(fit_container_base, digits_sign=digits_sign)
  save_fit(fit_container_base)
  write_plot_file(fit_container_base)
  if (xcount == 2) { write_interact_plot(fit_container_base) }
}

replase_terms <- function(lmfit, x_choosed) {
  lmfit$terms <- gsub('df[[], 2[]]', x_choosed[1], attr(lmfit$terms,"term.labels"))
  lmfit$terms <- gsub('df[[], 3[]]', x_choosed[2], attr(lmfit$terms,"term.labels"))
  lmfit
}

write_interact_plot <- function(fit_container, x_pred=NULL, pred_values=NULL) {
  x_count <- 30
  for (fitel in fit_container) {
    pred_in_choosed <- x_pred %in% fitel$x_choosed
    if (sum(pred_in_choosed) == 2 || is.null(x_pred)) {
      xyz <- get_xyz(df=fitel$df, lmfit=fitel$lmfit, mode=fitel$mode, x_count=x_count)
      
      open3d(useNULL=T)
      bbox3d(expand=1)
      #plot3d(f, slim=c(0,1), tlim=c(0,1), col='red', alpha = 0.8)
      #plot3d(fitel$df[,2], fitel$df[,3], fitel$df[,1], pch=19, col='blue',
      #       xlab=fitel$x_choosed[1], ylab=fitel$x_choosed[2], zlab=fitel$y_choosed,
      #       xlim=c(min(min(xyz$x), min(fitel$df[,2])), max(max(max(xyz$x), max(fitel$df[,2])))),
      #       ylim=c(min(min(xyz$y), min(fitel$df[,3])), max(max(max(xyz$y), max(fitel$df[,3])))),
      #       zlim=c(min(min(xyz$z), min(fitel$df[,1])), max(max(max(xyz$z), max(fitel$df[,1])))))
      
      xlim=c(min(min(xyz$x), min(fitel$df[,2])), max(max(max(xyz$x), max(fitel$df[,2]))))
      ylim=c(min(min(xyz$y), min(fitel$df[,3])), max(max(max(xyz$y), max(fitel$df[,3]))))
      zlim=c(min(min(xyz$z), min(fitel$df[,1])), max(max(max(xyz$z), max(fitel$df[,1]))))
      persp3d(xyz, col=shape_interact3d_col, alpha=shape_interact3d_alpha, aspect=c(1,1,1),
              xlab=fitel$x_choosed[1], ylab=fitel$x_choosed[2], zlab=fitel$y_choosed,
              xlim=xlim, ylim=ylim, zlim=zlim, expand=1)
      points3d(fitel$df[,2], fitel$df[,3], fitel$df[,1], col=experiment_points_col, size=6, expand=1)
      for(i in 1:length(xyz$x)){lines3d(rep(xyz$x[i], length(xyz$y)), xyz$y, xyz$z[i,], expand=1)}
      for(i in 1:length(xyz$y)){lines3d(xyz$x, rep(xyz$y[i], length(xyz$x)), xyz$z[,i], expand=1)}
      
      if (!is.null(pred_values)) {
        current_pred <- pred_values[pred_in_choosed]
        p2d <- c(current_pred[,fitel$x_choosed[1]], current_pred[,fitel$x_choosed[2]])
        p3d <- c(p2d, getfunc(fitel$mode, fitel$lmfit)(p2d[1], p2d[2]))
        scale <- 1/80
        radius <- c(diff(xlim)*scale,diff(ylim)*scale,diff(zlim)*scale)
        
        tval <- qt(.025, df=summary(fitel$lmfit)$fstatistic['dendf'], lower.tail=F)
        newdf <- data.frame(p2d[1], p2d[2])
        names(newdf) = fitel$x_choosed
        pr_conf <- predict(fitel$lmfit, newdf, se=T, interval = 'confidence', level=conf)
        pr_pred <- predict(fitel$lmfit, newdf, se=T, interval = 'prediction', level=conf)
        tval <- qt((1-conf)/2, df=pr_conf$df, lower.tail=F)
        title3d(main=paste0('Confidence interval: (', round(pr_conf$fit[2], 2), ', ', round(pr_conf$fit[3], 2), '); ', 
                            'Prediction interval: (', round(pr_pred$fit[2], 2), ', ', round(pr_pred$fit[3], 2), ')'))
        
        #spheres3d(p3d[1], p3d[2], p3d[3], radius=min(radius), col=rgb(red=40, green=80, blue=100, maxColorValue=255))
        #plot3d(ellipse3d(matrix(c(diff(xlim)*scale,0,0,0,diff(ylim)*scale,0,0,0,diff(zlim)*scale), 3, 3), centre=p3d), col='green', alpha=.4, add=T)
        pred_point <- get_ellipse(radius=radius, centre=p3d)
        points3d(pred_point[[1]], pred_point[[2]], pred_point[[3]], col=predict_ellipse_col, size=3, expand=1)
        coef <- 1.03
        lines3d(c(p3d[1], min(min(xyz$x), min(fitel$df[,2]))), c(p3d[2], p2d[2]),
                c(min(min(xyz$z),min(fitel$df[,1])), min(min(xyz$z),min(fitel$df[,1]))), col=lines_to_axis_col, expand=1)
        lines3d(c(p3d[1], p3d[1]), c(p3d[2], min(min(xyz$y), min(fitel$df[,3]))),
                c(min(min(xyz$z),min(fitel$df[,1])), min(min(xyz$z),min(fitel$df[,1]))), col=lines_to_axis_col, expand=1)
        lines3d(c(p3d[1], p3d[1]), c(p3d[2], p3d[2]), c(p3d[3], min(min(xyz$z),min(fitel$df[,1]))), col=lines_to_axis_col, expand=1)
        lines3d(c(p3d[1], min(min(xyz$x), min(fitel$df[,2]))),
                c(p3d[2], min(min(xyz$y), min(fitel$df[,3]))), c(p3d[3], p3d[3]), col=lines_to_axis_col, expand=1)
      }
      
      box3d(draw_front=T, expand=1)
      
      plot_dir <- paste(get_output_name_prec(fitel$filename),
                        get_outformula(fitel$y_choosed, fitel$x_choosed,
                                       mode=fitel$mode, withoutmultsign=T), fitel$Radj_border, sep='_')
      dir.create(plot_dir, showWarnings = FALSE)
      width = window3d_width
      par3d(windowRect=c(100,100,100+width,100+width))
      writeWebGL(dir=plot_dir, width=width)
      rgl.close()
    }
  }
}

get_ellipse <- function(radius, centre) {
  theta <- seq(0, 2*pi, len=180)
  a <- c(seq(0, .99*radius[1], len=40), seq(.99*radius[1], radius[1], len=25))
  b <- c(seq(0, .99*radius[2], len=40), seq(.99*radius[2], radius[2], len=25))
  x <- outer(theta, a, function(theta, a) {a * cos(theta)})
  y <- outer(theta, b, function(theta, b) {b * sin(theta)})
  zfun <- function(x, y) { z <- radius[3] * sqrt(abs(1 - (x/(radius[1]))^2 - (y/(radius[2]))^2)); z}
  z <- zfun(x, y)
  x <- c(x, x) + centre[1]
  y <- c(y, y) + centre[2]
  z <- c(z, -z) + centre[3]
  xyz <- list(x=x, y=y, z=z)
  xyz
}

write_plot_file <- function(fit_container) {
  for (fitel in fit_container) {
    
    plotname <- get_plot_name(fitel$filename, get_outformula(fitel$y_choosed, fitel$x_choosed,
                                                             mode=fitel$mode, withoutmultsign=T), fitel$Radj_border)
    create_png_plots(fitel$df, plotname=plotname, mode=fitel$mode, lmfit=fitel$lmfit, y_choosed=fitel$y_choosed,
                     x_choosed=fitel$x_choosed, xcount=fitel$xcount)
  }
}

save_fit <- function(fit_container) {
  if (length(fit_container) > 0) {
    save(fit_container, file=paste0(get_output_name_prec(fit_container[[1]]$filename), '.RData'))
  }
}

write_pred_file <- function(fit_container) {
  for (fitel in fit_container) {
    pred_file_name <- get_pred_file_name(fitel$filename, length(fitel$x_choosed), fitel$Radj_border)
    line_for_pred <- get_pred_line(xcount=length(fitel$x_choosed), linm=fitel$lmfit,
                                   cur_xparams=fitel$x_choosed, mode=fitel$mode)
    write(line_for_pred, file=pred_file_name, append=T)
  }
}

write_fit_file <- function(fit_container, digits_sign) {
  for (fitel in fit_container) {
    outfilename <- get_fit_file_name(fitel$filename, length(fitel$x_choosed), fitel$Radj_border)
    outformula <- get_outformula(fitel$y_choosed, fitel$x_choosed, mode=fitel$mode)
    Radj_text <- get_Radj_text(fitel$lmfit)
    eq <- getequation(length(fitel$x_choosed), fitel$lmfit, mode=fitel$mode)
    write(c(outformula, Radj_text, eq), file=outfilename, append=T)
    write.table(signif(summary(fitel$lmfit)$coefficients, digits=digits_sign),
                file=outfilename, append=T, sep='\t', row.names=T, col.names=T)
    write('', file=outfilename, append=T)
  }
}

get_file_data <- function(filename=file.path('input', '7-9.txt')) {
  data<-read.table(filename, header = T, sep = '\t')
  #convert '0's to 'NA's
  for (i in 1:length(data)){for (j in 1:length(data[,i])){if (!is.na(data[j,i])) {if (data[j,i]==0) {data[j,i]<- NA}}}}
  newdata <- data.frame(data[1])
  #delete bad cols, cols in wich count of valid elements >= min_valid_count
  x_ind_toremove <- c()
  for (i in 2:length(data)){if (sum(!is.na(data[,i])) >= min_valid_count) { newdata[names(data)[i]] <- data[i] }
                            else {x_ind_toremove <- c(x_ind_toremove, i-1)}}
  newdata
}

getfunc <- function(mode, lmfit)
{
  if (mode == 'simple') {
    return(function(x, y){z <- lmfit$coefficients[1] + lmfit$coefficients[2]*x + lmfit$coefficients[3]*y})
  }
  else if (mode == 'sqrt') {
    return(function(x, y){z <- lmfit$coefficients[1] + lmfit$coefficients[2]*x +
                          lmfit$coefficients[3] * sqrt(x) + lmfit$coefficients[4]*y +
                          lmfit$coefficients[5] * sqrt(y); z})
  }
  else if (mode == 'ln') {
    return(function(x, y){z <- lmfit$coefficients[1] + lmfit$coefficients[2]*x +
                          lmfit$coefficients[3] * log(x) + lmfit$coefficients[4]*y +
                          lmfit$coefficients[5] * log(y); z})
  }
  else if (mode == 'sqr') {
    return(function(x, y){z <- lmfit$coefficients[1] + lmfit$coefficients[2]*x +
                          lmfit$coefficients[3] * x**2 + lmfit$coefficients[4]*y +
                          lmfit$coefficients[5] * y**2; z})
  }
  else if (mode == 'mult') {
    return(function(x, y){z <- lmfit$coefficients[1] + lmfit$coefficients[2]*x +
                          lmfit$coefficients[3]*y + lmfit$coefficients[4] * x * y; z})
  }
  else if (mode == 'mult+sqrt') {
    return(function(x, y){z <- lmfit$coefficients[1] + lmfit$coefficients[2]*x +
                          lmfit$coefficients[3] * sqrt(x) + lmfit$coefficients[4]*y +
                          lmfit$coefficients[5] * sqrt(y) + lmfit$coefficients[6] * x * y; z})
  }
  else {return(NULL)}
}

getformula <- function(xcount, df, mode='simple', x_choosed, yparam)
{
  res_f <- NULL
  res_str <- paste0(yparam, ' ~ ')
  if (mode == 'simple') {
    if (xcount > 1) {
      for (i in 1:(xcount-1)) {
        res_str <- paste0(res_str, x_choosed[i], ' + ')
      }
    }
    res_str <- paste0(res_str, x_choosed[xcount])
  }
  else if (mode == 'sqrt' & xcount == 2) {
    res_str <- paste0(res_str, paste(x_choosed[1], paste0('sqrt(', x_choosed[1], ')'),
                                     x_choosed[2], paste0('sqrt(', x_choosed[2], ')'), sep='+'))
  }
  else if (mode == 'ln' & xcount == 2) {
    res_str <- paste0(res_str, paste(x_choosed[1], paste0('log(', x_choosed[1], ')'),
                                     x_choosed[2], paste0('log(', x_choosed[2], ')'), sep='+'))
  }
  else if (mode == 'sqr' & xcount == 2) {
    res_str <- paste0(res_str, paste(x_choosed[1], paste0('I(', x_choosed[1], '^2)'),
                                     x_choosed[2], paste0('I(', x_choosed[2], '^2)'), sep='+'))
  }
  else if (mode == 'mult' & xcount == 2) {
    res_str <- paste0(res_str, paste(x_choosed[1], x_choosed[2], 
                                     paste0('I(', x_choosed[1], '*', x_choosed[2], ')'), sep='+'))
  }
  else if (mode == 'mult+sqrt' & xcount == 2) {
    res_str <- paste0(res_str, paste(x_choosed[1], paste0('sqrt(', x_choosed[1], ')'),
                                     x_choosed[2], paste0('sqrt(', x_choosed[2], ')'),
                                     paste0('I(', x_choosed[1], '*', x_choosed[2], ')'), sep='+'))
  }
  res_f <- as.formula(res_str)
  res_f
}

getequation <- function(xcount, linm, mode='simple')
{
  eq <- paste0('(', as.character(signif(linm$coef[1], digits=digits_sign)), ')')
  if (mode == 'simple') {
    for (i in 1:xcount)
    {
      eq <- paste0(eq, '+', '(', as.character(signif(linm$coef[i+1], digits=digits_sign)), ')', '*', 'x', as.character(i))
    }
  }
  else if (mode == 'sqrt') {
    for (i in 1:xcount)
    {
      eq <- paste0(eq, '+', '(', as.character(signif(linm$coef[2*i], digits=digits_sign)), ')', '*', 'x', as.character(i),
                   '+', '(', as.character(signif(linm$coef[2*i+1], digits=digits_sign)), ')', '*', 'sqrt(', 'x', as.character(i), ')')
    }
  }
  else if (mode == 'ln') {
    for (i in 1:xcount)
    {
      eq <- paste0(eq, '+', '(', as.character(signif(linm$coef[2*i], digits=digits_sign)), ')', '*', 'x', as.character(i),
                   '+', '(', as.character(signif(linm$coef[2*i+1], digits=digits_sign)), ')', '*', 'ln(', 'x', as.character(i), ')')
    }
  }
  else if (mode == 'sqr') {
    for (i in 1:xcount)
    {
      eq <- paste0(eq, '+', '(', as.character(signif(linm$coef[2*i], digits=digits_sign)), ')', '*', 'x', as.character(i),
                   '+', '(', as.character(signif(linm$coef[2*i+1], digits=digits_sign)), ')', '*', 'x', as.character(i), '^2')
    }
  }
  else if (mode == 'mult' & xcount == 2) {
    for (i in 1:xcount)
    {
      eq <- paste0(eq, '+', '(', as.character(signif(linm$coef[i+1], digits=digits_sign)), ')', '*', 'x', as.character(i))
    }
    eq <- paste(eq, '+', '(', as.character(signif(linm$coef[4], digits=digits_sign)), ')*x1*x2')
  }
  else if (mode == 'mult+sqrt' & xcount == 2) {
    for (i in 1:xcount)
    {
      eq <- paste0(eq, '+', '(', as.character(signif(linm$coef[2*i], digits=digits_sign)), ')', '*', 'x', as.character(i),
                   '+', '(', as.character(signif(linm$coef[2*i+1], digits=digits_sign)), ')', '*', 'sqrt(', 'x', as.character(i), ')')
    }
    eq <- paste(eq, '+', '(', as.character(signif(linm$coef[6], digits=digits_sign)), ')*x1*x2')
  }
  eq
}

get_pred_line <- function(xcount, linm, cur_xparams, mode='simple')
{
  xnames <- c()
  line <- as.character(signif(linm$coef[1], digits=digits_sign))
  if (mode == 'simple') {
    for (i in 1:xcount)
    {
      line <- paste(line, paste0(as.character(signif(linm$coef[i+1], digits=digits_sign)), '*', cur_xparams[i]), sep = '\t')
    }
  }
  else if (mode == 'sqrt') {
    for (i in 1:xcount)
    {
      line <- paste(line, paste0(as.character(signif(linm$coef[2*i], digits=digits_sign)), '*', cur_xparams[i]),
                    paste0(as.character(signif(linm$coef[2*i+1], digits=digits_sign)), '*', 'Sqrt(', cur_xparams[i], ')'), sep = '\t')
    }
  }
  else if (mode == 'ln') {
    for (i in 1:xcount)
    {
      line <- paste(line, paste0(as.character(signif(linm$coef[2*i], digits=digits_sign)), '*', cur_xparams[i]),
                    paste0(as.character(signif(linm$coef[2*i+1], digits=digits_sign)), '*', 'Log(', cur_xparams[i], ',Exp(1))'), sep = '\t')
    }
  }
  else if (mode == 'sqr') {
    for (i in 1:xcount)
    {
      line <- paste(line, paste0(as.character(signif(linm$coef[2*i], digits=digits_sign)), '*', cur_xparams[i]),
                    paste0(as.character(signif(linm$coef[2*i+1], digits=digits_sign)), '*', 'Pow(', cur_xparams[i], ',2)'), sep = '\t')
    }
  }
  else if (mode == 'mult' & xcount == 2) {
    for (i in 1:xcount)
    {
      line <- paste(line, paste0(as.character(signif(linm$coef[i+1], digits=digits_sign)), '*', cur_xparams[i]), sep = '\t')
    }
    line <- paste(line, paste0(as.character(signif(linm$coef[4], digits=digits_sign)), '*', cur_xparams[1], '*', cur_xparams[2]), sep='\t')
  }
  else if (mode == 'mult+sqrt' & xcount == 2) {
    for (i in 1:xcount)
    {
      line <- paste(line, paste0(as.character(signif(linm$coef[2*i], digits=digits_sign)), '*', cur_xparams[i]),
                    paste0(as.character(signif(linm$coef[2*i+1], digits=digits_sign)), '*', 'Sqrt(', cur_xparams[i], ')'), sep = '\t')
    }
    line <- paste(line, paste0(as.character(signif(linm$coef[6], digits=digits_sign)), '*', cur_xparams[1], '*', cur_xparams[2]), sep='\t')
  }
  line
}

get_outformula <- function(yparam, xparams, mode='simple', withoutmultsign=F)
{
  outformula <- paste(yparam, '~')
  if (mode == 'simple') {
    for (x in xparams) {
      sign <- '+'
      if (x == xparams[1]) {sign <- character()}
      outformula <- paste(outformula, sign, x)
    }
  }
  else if (mode == 'sqrt') {
    for (x in xparams) {
      sign <- '+'
      if (x == xparams[1]) {sign <- character()}
      outformula <- paste(outformula, sign, x, '+', paste0('sqrt(', x, ')'))
    }
  }
  else if (mode == 'ln') {
    for (x in xparams) {
      sign <- '+'
      if (x == xparams[1]) {sign <- character()}
      outformula <- paste(outformula, sign, x, '+', paste0('ln(', x, ')'))
    }
  }
  else if (mode == 'sqr') {
    for (x in xparams) {
      sign <- '+'
      if (x == xparams[1]) {sign <- character()}
      outformula <- paste(outformula, sign, x, '+', paste0(x, '^2'))
    }
  }
  else if (mode == 'mult' & length(xparams) == 2) {
    for (x in xparams) {
      sign <- '+'
      if (x == xparams[1]) {sign <- character()}
      outformula <- paste(outformula, sign, x)
    }
    if (withoutmultsign) {outformula <- paste(outformula, '+', paste0(xparams[1], '(mult)', xparams[2]))}
    else {outformula <- paste(outformula, '+', paste0(xparams[1], '*', xparams[2]))}
  }
  else if (mode == 'mult+sqrt' & length(xparams) == 2) {
    for (x in xparams) {
      sign <- '+'
      if (x == xparams[1]) {sign <- character()}
      outformula <- paste(outformula, sign, x, '+', paste0('sqrt(', x, ')'))
    }
    if (withoutmultsign) {outformula <- paste(outformula, '+', paste0(xparams[1], '(mult)', xparams[2]))}
    else {outformula <- paste(outformula, '+', paste0(xparams[1], '*', xparams[2]))}
  }
  outformula
}

getargs <- function() {
  options <- commandArgs(trailingOnly = TRUE)
  options
}

set_wd_from_args <- function() {
  if (!is.na(getargs()[1])) {
    print(getargs()[1])
    newwd <- gsub('\\?', ' ', as.character(getargs()[1]), ignore.case =FALSE, fixed=FALSE)
    setwd(newwd)
  }
}

get_xcount_from_args <- function() {
  if (!is.na(getargs()[2])) {
    print(getargs()[2])
    as.numeric(getargs()[2])
  }
}

get_radj_from_args <- function() {
  if (!is.na(getargs()[3])) {
    print(getargs()[3])
    as.numeric(getargs()[3])
  }
}

predictvals <- function(model, xvar, yvar, xrange=NULL, samples=100, ...) {
  if (is.null(xrange)) {
    if (any(class(model) %in% c('lm', 'glm')))
      xrange <- range(model$model[[xvar]])
    else if (any(class(model) %in% 'loess'))
      xrange <- range(model$x)
  }
  
  newdata <- data.frame(x = seq(xrange[1], xrange[2], length.out = samples))
  names(newdata) <- xvar
  newdata[[yvar]] <- predict(model, newdata = newdata, ...)
  newdata
}

#string insertion
split_str_by_index <- function(target, index) {
  index <- sort(index)
  substr(rep(target, length(index) + 1),
         start = c(1, index),
         stop = c(index -1, nchar(target)))
}

#Taken from https://stat.ethz.ch/pipermail/r-help/2006-March/101023.html
interleave <- function(v1,v2)
{
  ord1 <- 2*(1:length(v1))-1
  ord2 <- 2*(1:length(v2))
  c(v1,v2)[order(c(ord1,ord2))]
}

insert_str <- function(target, insert, index) {
  insert <- insert[order(index)]
  index <- sort(index)
  paste(interleave(split_str_by_index(target, index), insert), collapse='')
}
#end string insertion

lminfiles <- function(folder = file.path(getwd(), inputdir), xcounts = 1:3, rsqr_border=.6) {
  print(folder)
  files <- list.files(path=folder, pattern='*.txt', full.names=T, recursive=F)
  for (xc in xcounts) {
    for (file in files) {
      lminfile(xcount = xc, filename = file, rsqr_border=rsqr_border)
    }
  }
  
}

#source('lminfile_v-2.R')
#lminfiles(xcounts=2, rsqr_border = .8)

set_wd_from_args()
radj <- get_radj_from_args()
xcount <- get_xcount_from_args()
if (!is.null(radj) & !is.null(xcount)) lminfiles(xcounts=xcount, rsqr_border = radj)
#else lminfiles()
