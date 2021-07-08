#Function for plotting series with outliers and without outliers
plot.tsoutliers1 <- function(x,title_name, xlabel,ylabel,xaxt,lwd_width,
                             args.lines.y = list(col = "gray80"), args.lines.yadj = list(col = "blue"),
                             args.lines.effects = list(type = "s", col = "red"),   
                             args.points = list(col = "gray80", bg = "red", pch = 21), plot.points = TRUE, 
                             args.x.axis = list(at = pretty(time(x$y)), tcl = -0.5, lwd = 3, lwd.ticks = 1),
                             args.y.axis = list(at = pretty(x$y), tcl = -0.5, lwd = 3, lwd.ticks = 1),
                             args.effects.axis = list(at = pretty(x$effects), tcl = -0.5, lwd = 3, lwd.ticks = 1),
                             ...)
{
  
  
  fargs.linesy <- formals(plot.tsoutliers)$args.lines.y
  efargs.linesy <- eval(fargs.linesy)
  if (!identical(args.lines.y, efargs.linesy))
  {
    args.lines.y <- c(args.lines.y, efargs.linesy)
    id <- which(duplicated(names(args.lines.y)))
    if (length(id) > 0)
      args.lines.y <- args.lines.y[-id]
  }
  
  fargs.linesyadj <- formals(plot.tsoutliers)$args.lines.yadj
  efargs.linesyadj <- eval(fargs.linesyadj)
  if (!identical(args.lines.yadj, efargs.linesyadj))
  {
    args.lines.yadj <- c(args.lines.yadj, efargs.linesyadj)
    id <- which(duplicated(names(args.lines.yadj)))
    if (length(id) > 0)
      args.lines.yadj <- args.lines.yadj[-id]
  }
  
  fargs.linesef <- formals(plot.tsoutliers)$args.lines.effects
  efargs.linesef <- eval(fargs.linesef)
  if (!identical(args.lines.effects, efargs.linesef))
  {
    args.lines.effects <- c(args.lines.effects, efargs.linesef)
    id <- which(duplicated(names(args.lines.effects)))
    if (length(id) > 0)
      args.lines.effects <- args.lines.effects[-id]
  }
  
  fargs.points <- formals(plot.tsoutliers)$args.points
  efargs.points <- eval(fargs.points)
  if (!identical(args.points, efargs.points))
  {
    args.points <- c(args.points, efargs.points)
    id <- which(duplicated(names(args.points)))
    if (length(id) > 0)
      args.points <- args.points[-id]
  }
  
  fargs.xaxis <- formals(plot.tsoutliers)$args.x.axis
  efargs.xaxis <- eval(fargs.xaxis)
  if (!identical(args.x.axis, efargs.xaxis))
  {
    args.x.axis <- c(args.x.axis, efargs.xaxis)
    id <- which(duplicated(names(args.x.axis)))
    if (length(id) > 0)
      args.x.axis <- args.x.axis[-id]
  }
  if (is.null(args.x.axis$labels))
    args.x.axis$labels <- args.x.axis$at
  args.x.axis$side <- 1
  
  fargs.yaxis <- formals(plot.tsoutliers)$args.y.axis
  efargs.yaxis <- eval(fargs.yaxis)
  if (!identical(args.y.axis, efargs.yaxis))
  {
    args.y.axis <- c(args.y.axis, efargs.yaxis)
    id <- which(duplicated(names(args.y.axis)))
    if (length(id) > 0)
      args.y.axis <- args.y.axis[-id]
  }
  if (is.null(args.y.axis$labels))
    args.y.axis$labels <- args.y.axis$at
  args.y.axis$side <- 2
  
  fargs.eaxis <- formals(plot.tsoutliers)$args.effects.axis
  efargs.eaxis <- eval(fargs.eaxis)
  if (!identical(args.effects.axis, efargs.eaxis))
  {
    args.effects.axis <- c(args.effects.axis, efargs.eaxis)
    id <- which(duplicated(names(args.effects.axis)))
    if (length(id) > 0)
      args.effects.axis <- args.effects.axis[-id]
  }
  if (is.null(args.effects.axis$labels))
    args.effects.axis$labels <- args.effects.axis$at
  if (is.null(args.effects.axis$side))
    args.effects.axis$side <- 4
  
  if (nrow(x$outliers) == 0)
  {
    cat(paste(sQuote("x"), "does not contain outliers to display\n"))
    return()
  }
  
  
  #do.call("plot", args = c(list(x = cbind(x$y, x$adj)), args.plot))
  plot(cbind(x$y, x$yadj), plot.type ="single", xaxt=xaxt,
       type = "n", xlab = paste( xlabel), ylab =  paste(ylabel),lwd=3)
  
  mtext(side = 3, text = paste(title_name), adj = 0)
  
  do.call("lines", args = c(list(x = x$y), args.lines.y))
  do.call("lines", args = c(list(x = x$yadj), args.lines.yadj))
  
  
  if (plot.points)
  {
    do.call("points", args = c(list(x = x$times, y = x$y[x$outliers[,"ind"]]), 
                               args.points))
  }
  
  
}



