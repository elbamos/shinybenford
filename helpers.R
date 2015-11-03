library(dplyr)

library(ggplot2)
library(BenfordTests)
library(benford.analysis)
library(quantmod)
library(shiny)
library(RColorBrewer)
library(wesanderson)
library(scales)
library(grid)
colors_discrete <- function(x) wes_palette("Darjeeling", n = x)
colors_divergent_discrete <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(11, "Spectral"))
colors_continuous <-  function(x) wes_palette(name= "Zissou",n = x, type= "continuous")
nacol <- colors_discrete(4)[4]

theme_set(
	theme_bw() %+replace%
		#	theme_fivethirtyeight() +
		theme(#legend.position="bottom",
			legend.key.size=unit(4,"mm"),
			legend.title=element_text(size=rel(0.8), face = "bold"),
			legend.margin=unit(0,"cm"),
			legend.key.size=unit(0.5,"lines"),
			legend.text=element_text(size = unit(10, "points")),
			axis.title.y = element_text(angle=90),
			axis.text = element_text(size=rel(0.7)),
			plot.margin = unit(c(0, 0.5, 1, 0), "lines"),
			#				plot.margin=unit(x = c(0,0,0,0), "cm"),
			axis.title=element_text(size=rel(0.8),face="bold")
		)
)

second_order <- function(x, digits = 2, round = 3) {
	n <- length(x)
	if (n == 0) return(NULL)
	first <- sort(x)[1:(n - 1)]
	second <- sort(x)[2:n]
	round(second - first, digits + round)
}

dbenf <- function(x, base = 10) { log(1 + 1/x, base = base) }

make_one <- function(financials) {
	holder <- c()
	for (i in 1:ncol(financials$BS$Q)) {
		holder <- c(holder, financials$BS$Q[,i])
	}
	for (i in 2:ncol(financials$BS$A) ) {
		holder <- c(holder, financials$BS$A[,i])
	}
	for (i in 1:ncol(financials$CF$Q)) {
		holder <- c(holder, financials$BS$Q[,i])
	}
	for (i in 2:ncol(financials$CF$A) ) {
		holder <- c(holder, financials$BS$A[,i])
	}
	for (i in 1:ncol(financials$IS$Q)) {
		holder <- c(holder, financials$BS$Q[,i])
	}
	for (i in 1:ncol(financials$IS$A) ) {
		holder <- c(holder, financials$BS$A[,i])
	}
	holder
}


Pr <- function(x, digits = 1, frequency = FALSE) {
	plc <- data.frame(Digits = BenfordTests::signifd(x, digits = digits)) %>%
		group_by(Digits) %>%
		summarise(Freq = n()) %>%
		filter(between(Digits, min(BenfordTests::signifd.seq(digits)), max(BenfordTests::signifd.seq(digits))) &
					 	! is.na(Digits) &
					 	! is.null(Digits)) %>%
		right_join(data.frame(Digits = BenfordTests::signifd.seq(digits))) %>%
		mutate(Digits = ordered(Digits),
					 expected = dbenf(signifd.seq(digits)),
					 group_holder = 1) %>%
		arrange(Digits)
	plc$expected <- plc$expected * abs(sum(plc$Freq, na.rm = TRUE))
	plc
}

summation <- function(x, digits = 2) {
	data.frame(Digits = signifd(x, digits = digits), Freq = x) %>%
		right_join(data.frame(Digits = BenfordTests::signifd.seq(digits))) %>%
		mutate(Digits = ordered(Digits)) %>%
		group_by(Digits) %>%
		summarize(Freq = sum(Freq, na.rm = TRUE)) %>%
		arrange(Digits) %>%
		ungroup() %>%
		mutate(Freq = Freq/sum(Freq, na.rm = TRUE),
					 group_holder = 1, expected = 0)
}

symmetrical <- function(x, digits = 2, Fun = summation) {
	x1 <- x[x > 0]
	x2 <- abs(x[x < 0])
	x1 <- Fun(x1, digits = digits)
	x2 <- Fun(x2, digits = digits)
	if (any(!is.na(x2$Freq))) {
		x2[,2] <- 0 - x2[,2]
		to_go <- inner_join(x1, x2, by = "Digits") %>%
			mutate(freq_down = Freq.y, Freq = Freq.x,
						 expected_down = expected.y,
						 expected = expected.x)
		return(to_go)
	}
	return(x1)
}


calc_breaks <- function(n, digits) {
	as.integer(seq(from = min(signifd.seq(digits)),
								 to = max(signifd.seq(digits)),
								 length.out = min(20, n)))
}

summationChart <- function(x, digits = 2, title = "") {
	x$x <- x$Freq / sum(x$Freq)
	.e <- environment()
	plt <- ggplot(data = x, aes(x = Digits), environment = .e) +
		geom_bar(aes(y = Freq), alpha = 0.7, fill = "orange",color="firebrick4", stat = "identity") +
		#			geom_smooth(stat = "function", fun = function(xx) { dbenf(as.numeric(levels(xx)))}, se = FALSE,
		labs(title = title, x = "Significant Digits", y = "") +
		scale_x_discrete(breaks = calc_breaks(nrow(x), digits))

	if("freq_down" %in% colnames(x)) plt <- plt +
		geom_bar(aes(y = freq_down), alpha = 0.7, fill = "tomato",color="firebrick4", stat = "identity")
	plt
}

aStart <- 5

alphas <- seq(from = 0, to = 0.5, length.out = aStart + 2)[-c(1, aStart + 2)]

make_cis <- function(n, digits) {
	expand.grid(alpha = alphas/2, sdigit = signifd.seq(digits)) %>%
	mutate(pdfbenf = rep(pbenf(digits), each = length(alphas))) %>%
	mutate(ci = qnorm(alpha, mean = pdfbenf * n, sd = sqrt(pdfbenf * n * (1 - pdfbenf))),
				 cin = qnorm(1 - alpha, mean = pdfbenf * n, sd = sqrt(pdfbenf * n * (1 - pdfbenf))),
				 sdigit = ordered(sdigit)) %>%
	mutate(ci = ifelse(ci < 0, 0, ci), cin = ifelse(ci < 0, 0, cin), sdigit = ordered(sdigit)) %>%
	group_by(alpha, sdigit)
}

benfordChart <- function(x, digits = 2, title = "") {
	n <- sum(x[x$Freq > 0,"Freq"], na.rm = TRUE)
	cis <- make_cis(n, digits)
	.e <- environment()
	plt <- ggplot(data = x, environment = .e) +
		geom_bar(aes(x = Digits, y = Freq),
						 alpha = 0.7, fill = "orange", color = if (digits >= 3) {"chocolate1";} else {"firebrick4";},
						 stat = "identity") +
		labs(title = title, x = "Significant Digits", y = "Frequency") +
		scale_x_discrete(breaks = calc_breaks(nrow(x), digits)) +
		geom_ribbon(
			aes(x = cis$sdigit, ymin = cis$ci, ymax = cis$cin, group = cis$alpha),
			fill = "cornflowerblue", color = "blue",alpha = 0.3/length(alphas + 3), size = 0.1) +
		geom_line( position = "identity",
							aes(x = Digits, y = expected, group = 1),  color = "blue", alpha = 1)

	if ("freq_down" %in% colnames(x)) {
		cis2 <- make_cis(abs(sum(x[x$Freq < 0,]$Freq, na.rm = TRUE)), digits)
		print(x[x$Freq < 0, "expected_down"])
		plt <- plt +
			geom_bar(aes(x = Digits, y = freq_down),
						 alpha = 0.7, fill = "tomato", color = if (digits >= 3) {"chocolate3";} else {"firebrick4";},
						 stat = "identity") +
			geom_ribbon(
				aes(x = cis2$sdigit, ymin = -cis2$ci, ymax = -cis2$cin, group = cis2$alpha),
				fill = "cornflowerblue", color = "blue",alpha = 0.3/length(alphas + 3), size = 0.1) +
 			geom_line(position = "identity",
 							aes(x = Digits, y = -expected_down, group = 1),  color = "blue", alpha = 1)
	}
	plt
}