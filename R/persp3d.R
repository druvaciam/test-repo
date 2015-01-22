persp3d.lm <- function(x, form, at, bounds, zlim, zlab, xlabs, col = "white", 
                       contours = NULL, hook, atpos = 3, decode = TRUE, theta = -25, 
                       phi = 20, r = 4, border = NULL, box = TRUE, ticktype = "detailed", 
                       ...) 
{
  draw.cont.line = function(line) {
    if (cont.varycol) {
      cont.col = col
      if (length(col) > 1) 
        cont.col = col[cut(c(line$level, dat$zlim), length(col))][1]
    }
    lines(trans3d(line$x, line$y, cont.z, transf), col = cont.col, 
          lwd = cont.lwd)
  }
  plot.data = contour.lm(x, form, at, bounds, zlim, xlabs, 
                         atpos = atpos, plot.it = FALSE)
  transf = list()
  if (missing(zlab)) 
    zlab = ""
  facet.col = col
  cont = !is.null(contours)
  if (mode(contours) == "logical") 
    cont = contours
  cont.first = cont
  cont.z = cz = plot.data[[1]]$zlim[1]
  cont.col = 1
  cont.varycol = FALSE
  cont.lwd = 1
  if (is.character(contours)) {
    idx = charmatch(contours, c("top", "bottom", "colors"), 
                    0)
    if (idx == 1) {
      cont.first = FALSE
      cont.z = plot.data[[1]]$zlim[2]
    }
    else if (idx == 2) {
    }
    else if (idx == 3) {
      cont.varycol = TRUE
      if (length(col) < 2) 
        col = rainbow(40)
    }
    else cont.col = contours
  }
  else if (is.list(contours)) {
    if (!is.null(contours$z)) 
      cz = contours$z
    if (is.numeric(cz)) 
      cont.z = cz
    else if (cz == "top") {
      cont.first = FALSE
      cont.z = plot.data[[1]]$zlim[2]
    }
    if (!is.null(contours$col)) 
      cont.col = contours$col
    if (!is.null(contours$lwd)) 
      cont.lwd = contours$lwd
    if (charmatch(cont.col, "colors", 0) == 1) {
      cont.varycol = TRUE
      if (length(col) < 2) 
        col = rainbow(40)
    }
  }
  for (i in 1:length(plot.data)) {
    dat = plot.data[[i]]
    cont.lines = NULL
    if (!missing(hook)) 
      if (!is.null(hook$pre.plot)) 
        hook$pre.plot(dat$labs)
    if (cont) 
      cont.lines = contourLines(dat$x, dat$y, dat$z)
    if (cont && cont.first) {
      transf = persp3d(dat$x, dat$y, dat$z, zlim = dat$zlim, 
                       theta = theta, phi = phi, r = r, col = NA, border = NA, 
                       box = FALSE, ...)
      lapply(cont.lines, draw.cont.line)
      par(new = TRUE)
    }
    if (length(col) > 1) {
      nrz = nrow(dat$z)
      ncz = ncol(dat$z)
      zfacet = dat$z[-1, -1] + dat$z[-1, -ncz] + dat$z[-nrz, 
                                                       -1] + dat$z[-nrz, -ncz]
      zfacet = c(zfacet/4, dat$zlim)
      facet.col = cut(zfacet, length(col))
      facet.col = col[facet.col]
    }
    transf = persp3d(dat$x, dat$y, dat$z, xlab = dat$labs[1], 
                     ylab = dat$labs[2], zlab = zlab, zlim = dat$zlim, 
                     col = facet.col, border = border, box = box, theta = theta, 
                     phi = phi, r = r, ticktype = ticktype, ...)
    if (atpos == 3) 
      #title(sub = dat$labs[5], ...)
      if (cont && !cont.first) 
        lapply(cont.lines, draw.cont.line)
    if (!missing(hook)) 
      if (!is.null(hook$post.plot)) 
        hook$post.plot(dat$labs)
    plot.data[[i]]$transf = transf
  }
  #    plot.data
  invisible(plot.data)
}