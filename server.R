library(shiny)


namesToPretty <- c("sino" = "Sino Forest Financials",
"tax" = "Taxable Incomes, 1978",
"census2000" = "Town Populations (Census 2000)",
 "census2009" = "Town Populations (Census 2009)",
 "census2010"  = "Town Populations (Census 2010)",
 "corporatePayment" = "Sample General Ledger Payments")

shinyServer(function(input, output) {
	prePowerData <- reactive({
		if (input$typeOfBenford == "ticker") {
			validate(
				need(input$tckr != "", "Please enter a ticker."))
		}
		if (input$typeOfBenford == "upload") {
			validate (
			need(! is.null(input$datafile) & input$datafile != "", "Please select a single column .csv file."
			))
		}
		x <- switch(input$typeOfBenford,
					 demo = switch(input$demoChoice,
					 				sino = function() {data(sino.forest); sino.forest[,1]},
					 				tax = function() {data(taxable.incomes.1978); taxable.incomes.1978[,1]},
					 				census2000 = function() {data(census.2000_2010); census.2000_2010[,4]},
					 				census2009 = function() {data(census.2009); census.2009[,3]} ,
					 				census2010 = function() {data(census.2000_2010); census.2000_2010[,5]} ,
					 				corporatePayment = function() {data(corporate.payment); corporate.payment[,4]}
					 		)(),
					 upload = fileData(),
					 ticker = try(make_one(quantmod::getFinancials(input$tckr, auto.assign = FALSE)))
		)
		validate(
			need(! class(x) == "try-error", "Error downloading ticker data."),
			need(is.numeric(x), "The data is not numeric -- bad ticker or file?"),
			need(is.vector(x), "Internal error."),
			need(length(x) > 5, "Insufficient data to process.")
		)
		x
	})

	dataToUse <- reactive(prePowerData()^input$pwr)

	dataName <- reactive(
		switch(input$typeOfBenford,
					 demo = namesToPretty[input$demoChoice],
					 ticker = input$tckr,
					 upload = input$datafile
		)
	)

	fileData <- reactive({
		validate (
			need(! is.null(input$datafile) & input$datafile != "", "Please select a single column .csv file."
		))
		infile <- input$datafile
		read.csv(infile$datapath, header = TRUE, colClasses = "numeric")
	})

	benfData1 <- reactive(if (input$positive) {symmetrical(dataToUse(), 1, Pr)} else {Pr(dataToUse(),1)})
	benfData2 <- reactive(if (input$positive) {symmetrical(dataToUse()[dataToUse() > 10], 2, Pr)}
												else {Pr(dataToUse()[dataToUse() > 10],2)})
	benfData3 <- reactive(if (input$positive) {symmetrical(dataToUse()[dataToUse() > 100], 3, Pr)}
												else {Pr(dataToUse()[dataToUse() > 100],3)})
	benfData4 <- reactive(if (input$positive) {symmetrical(dataToUse()[dataToUse() > 1000], 4, Pr)}
												else {Pr(dataToUse()[dataToUse() > 1000],4)})

sec_order <- reactive( if (input$positive) {
				dt <- dataToUse()
				pos_slice <- second_order(dt[dt > 0])
				neg_slice <- 0 - second_order(dt[dt < 0])
				symmetrical(c(neg_slice, pos_slice) , 2, Pr)
			}
											 else {Pr(dataToUse() %>% second_order(),2)}
				)


	sum_data <- reactive(if (input$positive) {symmetrical(dataToUse(), 2, summation)} else {summation(dataToUse(),2)})

  output$oneDigit <- renderPlot(benfordChart(benfData1(),
    title = paste(dataName(), "at One Significant Digit"), digits = 1)
  )
	output$twoDigits <- renderPlot(benfordChart(benfData2(),
		title = paste(dataName(), "at Two Significant Digits"), digits = 2)
	)
	output$secondOrder <- renderPlot(benfordChart(sec_order(),
		title = paste(dataName(), "Second Order"), digits = 2))
	output$threeDigits <- renderPlot(benfordChart(benfData3(),
		 title = paste(dataName(), "at Three Significant Digits"), digits = 3)
	)
	output$summation <- renderPlot(summationChart(sum_data() , digits = 2,
		title = paste(dataName(), "Summation"))
	)
	output$fourDigits <- renderPlot(benfordChart(benfData4(), digits = 4,
																								 title = paste(dataName(), "Four Digits"))
	)
})

