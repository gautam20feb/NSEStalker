plotmyOHLC<-
function (x, xlim = NULL, ylim = NULL, xlab = "Time", ylab, col = par("col"), 
    bg = par("bg"), axes = TRUE, frame.plot = axes, ann = par("ann"), 
    main = NULL, date = c("calendar", "julian"), format = "%Y-%m-%d", 
    origin = "1899-12-30", ...) 
{
    if ((!is.mts(x)) || (colnames(x)[1] != "Open") || (colnames(x)[2] != 
        "High") || (colnames(x)[3] != "Low") || (colnames(x)[4] != 
        "Close")) 
        stop("x is not a open/high/low/close time series")
    xlabel <- if (!missing(x)) 
        deparse(substitute(x))
    else NULL
    if (missing(ylab)) 
        ylab <- xlabel
    date <- match.arg(date)
    time.x <- time(x)
    dt <- min(lag(time.x) - time.x)/3
    if (is.null(xlim)) 
        xlim <- range(time.x)
    if (is.null(ylim)) 
        ylim <- range(x[is.finite(x)])
    plot.new()
    plot.window(xlim, ylim, ...)
    segments(time.x, x[, "High"], time.x, x[, "Low"], col = col[1], 
        bg = bg)
    segments(time.x - dt, x[, "Open"], time.x, x[, "Open"], col = col[1], 
        bg = bg)
    segments(time.x, x[, "Close"], time.x + dt, x[, "Close"], 
        col = col[1], bg = bg)
    if (ann) 
        title(main = main, xlab = xlab, ylab = ylab, ...)
    if (axes) {
        if (date == "julian") {
            axis(1, ...)
            axis(2, ...)
        }
        else {
            DD <- attr(x,"index")
            axis(1, at = seq(n), labels = DD, ...)
            axis(2, ...)
        }
    }
    if (frame.plot) 
        box(...)
}