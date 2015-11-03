library(shiny)

source("./helpers.R")

demoDataList <- c(
	"Sino Forest Financials" = "sino",
	"Taxable Incomes, 1978" = "tax",
	"Town Populations (Census 2000)" = "census2000",
	"Town Populations (Census 2009)" = "census2009",
	"Town Populations (Census 2010)" = "census2010",
	"Sample General Ledger Payments" = "corporatePayment"
)
shinyUI(fluidPage(

  # Application title
  titlePanel("Demonstration Benford Analysis"),
  helpText("This is a simple app to demonstrate the use of the Benford distribution in forensic accounting and fraud detection."),
  helpText("It was originally intended as a demonstration piece at the beginning of a larger project."),
  helpText("It tests sets of numbers against the Benford distribution, and permits transformations used in the field."),
  helpText("It includes as demonstration datasets the datasets included in the benford.analysis R package."),
  helpText("It will also attempt to download a company's most recent annual and quarterly financial statements from Edgar if given a ticker."),
  helpText("Or, the user can upload a set of numbers in .csv format."),
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
    	radioButtons("typeOfBenford", label = "",
    							 choices = c("Demonstration Data" = "demo", "Stock Ticker" = "ticker", "Upload File" = "upload"),
    							 selected = "demo", inline = FALSE),
    	conditionalPanel(condition = "input.typeOfBenford == 'demo'",
    			selectInput("demoChoice", "Demonstration Dataset: ", demoDataList, multiple = FALSE, selected = "tax"),
    			helpText("These are the sample datasets used by a forensic accounting text.")),
    	conditionalPanel(condition = "input.typeOfBenford == 'ticker'", textInput("tckr", "Ticker: ", value = ""),
    									 helpText("The app users the ticker symbol to pull adjusted numbers from Yahoo or Google Finance for the most recent four annual filings and three quarterly filings.")),
    	conditionalPanel(condition = "input.typeOfBenford == 'upload'",
    									 fileInput('datafile', "Choose CSV file",
    									 accept=c('text/csv', 'text/comma-separated-values,text/plain')),
    									 helpText("The file must have a single column of comma-delimited numbers, without a header.")),
    	sliderInput("pwr",
                  "Power:",
                  min = 1,
                  max = 50,
                  value = 1),
    	helpText("Raising the data to an exponent can sometimes smooth-out the distribution enough to highlight genuine oddities.")# ,
#    	checkboxInput("doThreeDigits", "Plot three significant digits (can be slow)", value = FALSE)
			, checkboxInput("positive", "Separate Positive & Negative Values", value = FALSE)
    	),

    mainPanel(
				tabsetPanel(
      		tabPanel("One Digit", plotOutput("oneDigit")),
      		tabPanel("Two Digits", plotOutput("twoDigits")),
      		tabPanel("Second Order", plotOutput("secondOrder"),
      						 helpText("Second order differencing sorts the series, and shows each number less the next-smaller number.")),
      		tabPanel("Summation", plotOutput("summation"),
      						 helpText("Summation analysis adds numbers in the dataset, grouped by significant digit.")
      						 ),
      		tabPanel("Three Digits (can be slow)", plotOutput("threeDigits")),
      		tabPanel("Four Digits (can be slow)", plotOutput("fourDigits")),
      		selected = "Two Digits")
    )
  )
))
