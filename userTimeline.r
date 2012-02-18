require(twitteR)
require(ggplot2)
require(plyr)
require(stringr)

username='sciruela'

mht=userTimeline(username,n=3200)
tw.df=twListToDF(mht)

trim <- function (x) sub('@','',x)

tw.df$rt=sapply(tw.df$text,function(tweet) trim(str_match(tweet,"^RT (@[[:alnum:]_]*)")[2]))
tw.df$rtt=sapply(tw.df$rt,function(rt) if (is.na(rt)) 'T' else 'RT')


pdf("/Users/sciruela/Documents/userTimeLine/graph1.pdf")
ggplot(tw.df)+geom_point(aes(x=created,y=screenName))
dev.off()

tw.dfs=subset(tw.df,subset=((Sys.time()-created)<8000))

pdf("/Users/sciruela/Documents/userTimeLine/graph2.pdf")
ggplot(tw.dfs)+geom_point(aes(x=created,y=screenName))
dev.off()

tw.dfx=ddply(tw.dfs, .var = "replyToSN", .fun = function(x) {return(subset(x, created %in% min(created),select=c(replyToSN,created)))})
tw.dfxa=arrange(tw.dfx,-desc(created))
tw.dfs$replyToSN=factor(tw.dfs$replyToSN, levels = tw.dfxa$replyToSN)

pdf("/Users/sciruela/Documents/userTimeLine/graph3.pdf")
ggplot(tw.dfs)+geom_point(aes(x=created,y=replyToSN))
dev.off()

pdf("/Users/sciruela/Documents/userTimeLine/graph4.pdf")
ggplot()+geom_point(data=subset(tw.dfs,subset=(!is.na(replyToSN))),aes(x=created,y=replyToSN),col='red') + geom_point(data=subset(tw.dfs,subset=(!is.na(rt))),aes(x=created,y=rt),col='blue') + geom_point(data=subset(tw.dfs,subset=(is.na(replyToSN) & is.na(rt))),aes(x=created,y=screenName),col='green')
dev.off()



r_table <- table(tw.dfs$replyToSN)
r_levels <- names(r_table)[order(-r_table)]
tw.dfs$replyToSN <- factor(tw.dfs$replyToSN, levels = r_levels) 

pdf("/Users/sciruela/Documents/userTimeLine/graph5.pdf")
ggplot(subset(tw.dfs,subset=(!is.na(replyToSN))),aes(x=replyToSN)) + geom_bar(aes(y = (..count..)))+opts(axis.text.x=theme_text(angle=-90,size=6))
dev.off()

head(table(tw.dfs$replyToSN))

topTastic=function(dfc,num=5){
	r_table <- table(dfc)
	r_levels <- names(r_table)[order(-r_table)]
	head(table(factor(dfc, levels = r_levels)),num)
}
topTastic(tw.dfs$rt)
topTastic(tw.dfs$replyToSN,10)

tw.dfs$month=sapply(tw.dfs$created, function(x) {p=as.POSIXlt(x);p$mon})
tw.dfs$hour=sapply(tw.dfs$created, function(x) {p=as.POSIXlt(x);p$hour})
tw.dfs$wday=sapply(tw.dfs$created, function(x) {p=as.POSIXlt(x);p$wday})

pdf("/Users/sciruela/Documents/userTimeLine/graph6.pdf")
ggplot(tw.dfs)+geom_jitter(aes(x=wday,y=hour))
dev.off()

pdf("/Users/sciruela/Documents/userTimeLine/graph7.pdf")
ggplot(tw.dfs,aes(x=created))+geom_bar(aes(y = (..count..)))
dev.off()

pdf("/Users/sciruela/Documents/userTimeLine/graph8.pdf")
ggplot(tw.dfs,aes(x=wday))+geom_bar(aes(y = (..count..)),binwidth=1)
dev.off()

pdf("/Users/sciruela/Documents/userTimeLine/graph9.pdf")
ggplot(tw.dfs,aes(x=hour))+geom_bar(aes(y = (..count..)),binwidth=1)
dev.off()

require(xts)
ts=xts(rep(1,times=nrow(tw.dfs)),tw.dfs$created)


ts.sum=apply.daily(ts,sum) 


ts.sum.df=data.frame(date=index(ts.sum), coredata(ts.sum))

colnames(ts.sum.df)=c('date','sum')

pdf("/Users/sciruela/Documents/userTimeLine/graph9.pdf")
ggplot(ts.sum.df)+geom_line(aes(x=date,y=sum))
dev.off()

pdf("/Users/sciruela/Documents/userTimeLine/graph10.pdf")
acf(ts.sum)
dev.off()



calendarHeat <- function(dates, 
values, 
ncolors=99, 
color="r2g", 
varname="Values",
date.form = "%Y-%m-%d", ...) {
	require(lattice)
	require(grid)
	require(chron)
	if (class(dates) == "character" | class(dates) == "factor" ) {
		dates <- strptime(dates, date.form)
	}
	caldat <- data.frame(value = values, dates = dates)
	min.date <- as.Date(paste(format(min(dates), "%Y"),
							  "-1-1",sep = ""))
	max.date <- as.Date(paste(format(max(dates), "%Y"),
							  "-12-31", sep = ""))
	dates.f <- data.frame(date.seq = seq(min.date, max.date, by="days"))
	
	caldat <- data.frame(date.seq = seq(min.date, max.date, by="days"), value = NA)
	dates <- as.Date(dates) 
	caldat$value[match(dates, caldat$date.seq)] <- values
	
	caldat$dotw <- as.numeric(format(caldat$date.seq, "%w"))
	caldat$woty <- as.numeric(format(caldat$date.seq, "%U")) + 1
	caldat$yr <- as.factor(format(caldat$date.seq, "%Y"))
	caldat$month <- as.numeric(format(caldat$date.seq, "%m"))
	yrs <- as.character(unique(caldat$yr))
	d.loc <- as.numeric()                        
	for (m in min(yrs):max(yrs)) {
		d.subset <- which(caldat$yr == m)  
		sub.seq <- seq(1,length(d.subset))
		d.loc <- c(d.loc, sub.seq)
	}  
	caldat <- cbind(caldat, seq=d.loc)
	
	r2b <- c("#0571B0", "#92C5DE", "#F7F7F7", "#F4A582", "#CA0020") #red to blue                                                                               
	r2g <- c("#D61818", "#FFAE63", "#FFFFBD", "#B5E384")   #red to green
	w2b <- c("#045A8D", "#2B8CBE", "#74A9CF", "#BDC9E1", "#F1EEF6")   #white to blue
	
	assign("col.sty", get(color))
	calendar.pal <- colorRampPalette((col.sty), space = "Lab")
	def.theme <- lattice.getOption("default.theme")
	cal.theme <-
	function() {  
		theme <-
		list(
			 strip.background = list(col = "transparent"),
			 strip.border = list(col = "transparent"),
			 axis.line = list(col="transparent"),
			 par.strip.text=list(cex=0.8))
    }
	lattice.options(default.theme = cal.theme)
	yrs <- (unique(caldat$yr))
	nyr <- length(yrs)
	print(cal.plot <- levelplot(value~woty*dotw | yr, data=caldat,
								as.table=TRUE,
								aspect=.12,
								layout = c(1, nyr%%7),
								between = list(x=0, y=c(1,1)),
								strip=TRUE,
								main = paste("Calendar Heat Map of ", varname, sep = ""),
								scales = list(
											  x = list(
													   at= c(seq(2.9, 52, by=4.42)),
													   labels = month.abb,
													   alternating = c(1, rep(0, (nyr-1))),
													   tck=0,
													   cex = 0.7),
											  y=list(
													 at = c(0, 1, 2, 3, 4, 5, 6),
													 labels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday",
																"Friday", "Saturday"),
													 alternating = 1,
													 cex = 0.6,
													 tck=0)),
								xlim =c(0.4, 54.6),
								ylim=c(6.6,-0.6),
								cuts= ncolors - 1,
								col.regions = (calendar.pal(ncolors)),
								xlab="" ,
								ylab="",
								colorkey= list(col = calendar.pal(ncolors), width = 0.6, height = 0.5),
								subscripts=TRUE
								) )
	panel.locs <- trellis.currentLayout()
	for (row in 1:nrow(panel.locs)) {
		for (column in 1:ncol(panel.locs))  {
			if (panel.locs[row, column] > 0)
			{
				trellis.focus("panel", row = row, column = column,
							  highlight = FALSE)
				xyetc <- trellis.panelArgs()
				subs <- caldat[xyetc$subscripts,]
				dates.fsubs <- caldat[caldat$yr == unique(subs$yr),]
				y.start <- dates.fsubs$dotw[1]
				y.end   <- dates.fsubs$dotw[nrow(dates.fsubs)]
				dates.len <- nrow(dates.fsubs)
				adj.start <- dates.fsubs$woty[1]
				
				for (k in 0:6) {
					if (k < y.start) {
						x.start <- adj.start + 0.5
					} else {
						x.start <- adj.start - 0.5
					}
					if (k > y.end) {
						x.finis <- dates.fsubs$woty[nrow(dates.fsubs)] - 0.5
					} else {
						x.finis <- dates.fsubs$woty[nrow(dates.fsubs)] + 0.5
					}
					grid.lines(x = c(x.start, x.finis), y = c(k -0.5, k - 0.5), 
							   default.units = "native", gp=gpar(col = "grey", lwd = 1))
				}
				if (adj.start <  2) {
					grid.lines(x = c( 0.5,  0.5), y = c(6.5, y.start-0.5), 
							   default.units = "native", gp=gpar(col = "grey", lwd = 1))
					grid.lines(x = c(1.5, 1.5), y = c(6.5, -0.5), default.units = "native",
							   gp=gpar(col = "grey", lwd = 1))
					grid.lines(x = c(x.finis, x.finis), 
							   y = c(dates.fsubs$dotw[dates.len] -0.5, -0.5), default.units = "native",
							   gp=gpar(col = "grey", lwd = 1))
					if (dates.fsubs$dotw[dates.len] != 6) {
						grid.lines(x = c(x.finis + 1, x.finis + 1), 
								   y = c(dates.fsubs$dotw[dates.len] -0.5, -0.5), default.units = "native",
								   gp=gpar(col = "grey", lwd = 1))
					}
					grid.lines(x = c(x.finis, x.finis), 
							   y = c(dates.fsubs$dotw[dates.len] -0.5, -0.5), default.units = "native",
							   gp=gpar(col = "grey", lwd = 1))
				}
				for (n in 1:51) {
					grid.lines(x = c(n + 1.5, n + 1.5), 
							   y = c(-0.5, 6.5), default.units = "native", gp=gpar(col = "grey", lwd = 1))
				}
				x.start <- adj.start - 0.5
				
				if (y.start > 0) {
					grid.lines(x = c(x.start, x.start + 1),
							   y = c(y.start - 0.5, y.start -  0.5), default.units = "native",
							   gp=gpar(col = "black", lwd = 1.75))
					grid.lines(x = c(x.start + 1, x.start + 1),
							   y = c(y.start - 0.5 , -0.5), default.units = "native",
							   gp=gpar(col = "black", lwd = 1.75))
					grid.lines(x = c(x.start, x.start),
							   y = c(y.start - 0.5, 6.5), default.units = "native",
							   gp=gpar(col = "black", lwd = 1.75))
					if (y.end < 6  ) {
						grid.lines(x = c(x.start + 1, x.finis + 1),
								   y = c(-0.5, -0.5), default.units = "native",
								   gp=gpar(col = "black", lwd = 1.75))
						grid.lines(x = c(x.start, x.finis),
								   y = c(6.5, 6.5), default.units = "native",
								   gp=gpar(col = "black", lwd = 1.75))
					} else {
						grid.lines(x = c(x.start + 1, x.finis),
								   y = c(-0.5, -0.5), default.units = "native",
								   gp=gpar(col = "black", lwd = 1.75))
						grid.lines(x = c(x.start, x.finis),
								   y = c(6.5, 6.5), default.units = "native",
								   gp=gpar(col = "black", lwd = 1.75))
					}
				} else {
					grid.lines(x = c(x.start, x.start),
							   y = c( - 0.5, 6.5), default.units = "native",
							   gp=gpar(col = "black", lwd = 1.75))
				}
				
				if (y.start == 0 ) {
					if (y.end < 6  ) {
						grid.lines(x = c(x.start, x.finis + 1),
								   y = c(-0.5, -0.5), default.units = "native",
								   gp=gpar(col = "black", lwd = 1.75))
						grid.lines(x = c(x.start, x.finis),
								   y = c(6.5, 6.5), default.units = "native",
								   gp=gpar(col = "black", lwd = 1.75))
					} else {
						grid.lines(x = c(x.start + 1, x.finis),
								   y = c(-0.5, -0.5), default.units = "native",
								   gp=gpar(col = "black", lwd = 1.75))
						grid.lines(x = c(x.start, x.finis),
								   y = c(6.5, 6.5), default.units = "native",
								   gp=gpar(col = "black", lwd = 1.75))
					}
				}
				for (j in 1:12)  {
					last.month <- max(dates.fsubs$seq[dates.fsubs$month == j])
					x.last.m <- dates.fsubs$woty[last.month] + 0.5
					y.last.m <- dates.fsubs$dotw[last.month] + 0.5
					grid.lines(x = c(x.last.m, x.last.m), y = c(-0.5, y.last.m),
							   default.units = "native", gp=gpar(col = "black", lwd = 1.75))
					if ((y.last.m) < 6) {
						grid.lines(x = c(x.last.m, x.last.m - 1), y = c(y.last.m, y.last.m),
								   default.units = "native", gp=gpar(col = "black", lwd = 1.75))
						grid.lines(x = c(x.last.m - 1, x.last.m - 1), y = c(y.last.m, 6.5),
								   default.units = "native", gp=gpar(col = "black", lwd = 1.75))
					} else {
						grid.lines(x = c(x.last.m, x.last.m), y = c(- 0.5, 6.5),
								   default.units = "native", gp=gpar(col = "black", lwd = 1.75))
					}
				}
			}
		}
		trellis.unfocus()
	} 
	lattice.options(default.theme = def.theme)
}

pdf("/Users/sciruela/Documents/userTimeLine/graph11.pdf")
calendarHeat(ts.sum.df$date, ts.sum.df$sum, varname="@sciruela Twitter activity")
dev.off()