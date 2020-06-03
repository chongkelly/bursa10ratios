#----------------------------------------------------------------
# RShiny on Analyzing 10 Financial Ratios using Bursa Market
# See publised app on  
# https://kellychong.shinyapps.io/stockglance3
# Author : Kelly Chong
# Date   : 21st May 2020


library(tidyverse)
library(GGally)
library(treemapify)
library(shiny)

shares2 <- readRDS("shares3klse.rds")

ui <- fluidPage(
  titlePanel("Analyzing 10 Financial Ratios"),
  
  sidebarLayout(
    sidebarPanel(h3(strong("... at a Glance")),
                 p("We are told that ratios are important; 
                   but comparing ratios against ...", em("what?")),
                 br(),
                 p("In here, this tool compiles the top 50 stocks in 
                 Malaysia; effectively giving you a simple, 
                   quick glimpse.") ,
                 p("Simply pick a number and start analyzing 
                   away like a pro."),
                 br(),
                 
                 sliderInput("number", 
                             label = "Top number stocks by Market Cap",
                             min = 0, max = 50, value = c(0, 30)),
                 
                 selectInput("sector",
                             label = "Choose a Sector to display",
                             choices = shares2$sector,
                             selected = 1),
                 p("As of 21st May 2020" , style="font-size:85%", align="right")
                 
    ),
    
    # Overview ----------------------------------------------------------------
    mainPanel(
      tabsetPanel(
        tabPanel("Overview", 
                 br(),
                 p("First off, let's get a feel of the entire stock market, Bursa:"),
                 p("This tree diagram shows stocks by",
                   strong("Market Cap."), "The bigger the box it occupies, 
                   the larger the size of company. Can you tell who is the boss in each sector?"),
                 plotOutput("tree"),
                 br(),
                 br(),
                 p("Next, our sweet lollipop chart shows stocks by",
                   strong("Share Price."), "Plus, the share prices are coloured 
                   according to", strong("Sector.")),
                 plotOutput("lollipop"),
                 p(em("NOTE: For missing lines, the share price of the particular stock
                 is out of range; thus not displayed."), style="font-size:80%"),
                 br(),
                 br(),
                 p("Okay, enough of these. Let's dive into the serious stuff:",
                   em("ratios.")),
                 plotOutput("scatterwrap"),
                 p(strong("Dividend"), "is the main way companies return money to 
                 their shareholders.", strong("Profit Margin"), "is how much a company's 
                 sales flow to the bottom line minusing all cost and tax. 
                   Naturally, the higher the profitability ratios, the better it is for us.",
                   em("More profit and more return!")),
                 p("Scrutinizing the plots, 
                   we can see that the dividend yield varies significantly not just
                   across sectors but within sectors too.That's why analysts always say -Be careful
                   what you compare against!"),
                 p("To make things simple, let's read the ratios sector by sector. Simply click on the", 
                   strong("Sector"), "tab above to start."),
                 br(),
                 br(),
                 p(em("Geek alert: The line cutting across companies is the simple 
                   linear regression. A very slanted upward line shows high positive relationship 
                   between dividend yield and profit margin.A horizontal line shows no 
                   relationship. Since we are measuring both profitability ratios, 
                      it make sense for a company with higher profit margin, to have higher yield, right?"), 
                   style = "font-size:85%")
        ),
        
        # Sector ------------------------------------------------------------------
        tabPanel("Sector",
                 br(), 
                 fluidRow(
                   br(),
                   p("Go ahead and choose a",
                     strong("Sector"), "on the left panel."),
                   p(em(" Geek alert: There are 11 sectors,
                   24 industrial groups and 69 industries
                      according to GICS"), style="font-size:85%" ),
                   br(),
                   p("In this ",
                     strong("Sector,"), "we can see the",
                     strong("Range of Stock Prices"),
                     "in the last 52 weeks.The dot 
                     represents latest closing share price. Some dots are at 
                     the lower tail end, meaning they 
                     are trading at a one year low. So do we buy low now? "),
                   column(12, plotOutput("lollipop_sector"))
                 ),
                 fluidRow(
                   p(em("NOTE: If at any point, the graphs do not appear, try increasing your
                        range of stocks selected; or change a sector. "), style="font-size:80%")
                 ),
                 fluidRow(
                   h2("Price Ratios"),
                   p("Moving on to the all important ratios, starting with",
                     em(strong("price ratios.")), "There are 3 general ones analysts
                     use to get an idea of whether a stock's price is reasonable or not."),
                   p("They are separately",
                     em("price-to-earnings ratio (PE),
                       price-to-sales ratio (PS) and 
                       price-to-book ratio (PB).")),
                   p("Usually, the higher the ratio means the more expensive the stock is.
                      For example, the higher the",
                     strong("PE,"), "the more expensive the stock is compared to its earnings.
                      The following bar charts rank stocks from the most expensive
                      to least expensive. Do the higher ranked stocks sound familiar to you?")
                 ),
                 fluidRow(
                   column(3,plotOutput("bar_pe")),
                   column(3,plotOutput("bar_ps")),
                   column(3,plotOutput("bar_pb"))
                 ),
                 
                 # Profit ratios ----------------------------------------------------------
                 fluidRow(
                   h2("Profitability Ratios"),
                   p("Next, we have",
                     em(strong("profitability ratios.")), "The 3 most followed metrics are used to gauge how well
                    a company is at converting business operations into profits."),
                   p("They are separately",
                     em("return on asset (ROA),
                     return on equity (ROE) and profit margin")),
                   p("Naturally, the higher the profit ratios the better it is.
                      The scatter plot shows top right corner represents high",
                     strong("Profit Margin"), "and high",
                     strong("ROA;"), "while the darker the color, the higher 
                    its", strong("ROE."),"So ideally, we would want to buy stocks that are
                    concetrated at the upper right corner with a darker shade"),
                   column(12, plotOutput("scatter_profit"))
                 ),
                 
                 # Liqiuidity and Debt Ratios ----------------------------------------------
                 fluidRow(
                   h2("Liquidity and Debt Ratios"),
                   p(em(strong("Liquidity ratio")), "indicates how capable a business is of meeting its
                      short-term obligations. Liquidity is represented by",
                     em("current ratio and quick ratio")),
                   p(em(strong("Debt ratio")), "concentrates on the long term health of 
                      a business. 2 main debt ratios are",
                     em("debt-to-equity ratio and interest coverage ratio")),
                   p("The parallel plot below displays the relationship of all 4 ratios together.
                    Ideally, we would like to have stocks with high ", strong("Current,"), 
                     "high ", strong("Quick,"), "low ", strong("Debt-to-Equity"),
                     "and high", strong ("Interest Coverage."))
                 ),
                 fluidRow(
                   column(12,plotOutput("parallel_liquid"))
                 ),
                 fluidRow(
                   p(em("NOTE: If an error message is shown/ratios remain stubbornly at 0, 
                    it means unfortunately
                         the author does not have complete data for this selected sector.
                         Try increasing the range of stocks selected; or change a sector. For 
                         illustration purposes, the ratios are standardized to respective
                         z-scores."), 
                     style="font-size:80%")
                 ),
                 
                 # Conclusion --------------------------------------------------------------
                 fluidRow(
                   h2("Conclusion"),
                   p("Well done, there you have it! All 10 ratios",
                     em("(3 price, 3 profitability, 2 liquidity 
                       and 2 debt)"), "in a single page. By understanding
                    what they mean and making valid comparisons, you have just managed what
                    analyts call", strong(em("quantitative analysis")), "of stocks."),
                   br(),
                   p("Before we end and pat ourselves on the back, let's try to 
                    answer the questions above:"),
                   p("1) If share price is at 1 year low, should we buy low now?",
                     span(em("It depends. Prices may be low due to", strong("temporary shocks"),
                             "or", strong("fundamental changes."), "To find out, you need to dig further into the company."), 
                          style="color:grey")),
                   p("2) From above example, higher PE/PS/PB ratio means the stock is expesive; so that's bad?",
                     span(em("It depends too. There's a reason why people are willing to pay a ", strong("price premium"),
                             "for certain stocks. Usually, higher PE represents growth stock, while
                         lower PE represents value stock. Of course, we're oversimplifying the entire
                         field of finance here; but you get the gist. "), style="color:grey")),
                   p("3) Referring to the linear regression in previous tab, a higher profit margin
                    translates to higher dividend yield?",
                     span(em("You guessed it- it depends. Yes, a higher profit margin means the company has 
                         potentially more leftovers after deducting costs. However, the company can choose to
                         retain the profit or distribute it to investors as return depending
                         on the company's ", strong("reinvestment strategy."), 
                             "Example, Warren Buffet's company, Berkshire Hathaway is famous for not paying a single 
                         sen of dividend, although it's sitting on huge cashpiles."), style="color:grey")),
                   p("That's it, as promised :)"),
                   br(),
                   br(),
                   br(),
                   p(em("DISCLAIMER: All views expressed here are entirely Kelly's
                       own opinion. All trading strategies are used at reader's own risk.
                       With that being said, if you disagree with me on any of
                       the above items or would like to delve deeper into this fascnicating discipline together
                       , just fire away an email to", a("kellychong142@gmail.com"), "Trust me,
                       I'll be extremely delighted to share/learn from you.") ,style = "font-size:85%")
                 )
                 
                 
        )
      )
    )
  )
)

# Define server -----------------------------------------------------------
server <- function(input, output) {
  
  #setting the input
  data <- reactive ({
    shares2[input$number[1]:input$number[2],]
  })
  
  
  sector <- reactive ({
    data() %>% 
      filter(sector== input$sector)
  })
  
  # Tab Overview ------------------------------------------------------------
  #generating a treemap market cap
  output$tree <- renderPlot({
    data() %>%
      ggplot(aes(area=marketcap, fill=sector, subgroup=sector, subgroup2=names)) +
      geom_treemap()+
      geom_treemap_subgroup_border(size=2) +
      geom_treemap_subgroup2_border(size=2) +
      geom_treemap_text(aes(label=names), color="black")+
      scale_x_continuous(expand = c(0, 0)) +
      scale_y_continuous(expand = c(0, 0))  
  })
  
  
  #generating a lollipop share chart
  output$lollipop <- renderPlot({
    data() %>%  
      ggplot(aes(reorder(names, shareprice), 
                 shareprice))+
      geom_point(aes(col=sector, siz=marketcap))+
      ylim(0,33)+
      coord_flip() +
      scale_color_discrete() +
      geom_segment(aes(x=names, 
                       xend=names, 
                       y=0, 
                       yend=shareprice,
                       color=sector)) +
      xlab("") 
  })
  
  #generating multiple scatter plots 
  output$scatterwrap <- renderPlot ({
    data() %>%   
      ggplot(aes(profit_margin,div_yield,
                 size= marketcap,
                 color= sector,
                 label= names)) +
      geom_point() +
      geom_text(check_overlap = TRUE,
                size=2, color="Black",
                vjust = 0, 
                nudge_y = 0) +
      geom_smooth(method="lm") +
      facet_wrap(~ sector)  +
      coord_cartesian(xlim=c(0,0.5), ylim=c(0, 0.2))+
      xlab("Profit Margin") +
      ylab("Dividend Yield")
  })
  
  # Tab Sector --------------------------------------------------------------
  # Create a custom color scale
  
  
  # generating a lollipop sector share chart
  output$lollipop_sector <- renderPlot({
    sector() %>%  
      ggplot(aes(reorder(names, shareprice), 
                 shareprice))+
      geom_point(aes(col=names, siz=marketcap))+
      ylim(0,max(sector()$shareprice_52high))+ 
      coord_flip() +
      scale_color_discrete() +
      geom_segment(aes(x=names, 
                       xend=names, 
                       y=shareprice_52low, 
                       yend=shareprice_52high,
                       color=names)) +
      xlab("") +
      ylab("Share Price with 52weeks range") +
      theme_grey() +
      theme(legend.position = "none")
  })
  
  # generating bar charts for price ratio
  output$bar_pe <- renderPlot({ #price ratio 1
    sector() %>%  
      ggplot(aes(reorder(names, pe), pe, fill=pe)) +
      geom_bar(stat="identity", width=0.5) +
      scale_fill_distiller(palette = 1, guide = FALSE, direction = 1)+
      coord_flip() +
      theme_classic()+
      ylab("PE") +
      xlab("") 
  })
  
  output$bar_ps <- renderPlot({ #price ratio 2
    sector() %>% 
      ggplot(aes(reorder(names, ps), ps, fill=ps)) +
      geom_bar(stat="identity", width=0.5) +
      scale_fill_distiller(palette = 2, guide = FALSE, direction = 1)+
      coord_flip() +
      theme_classic() +
      ylab("PS")+
      xlab("")
  })
  
  output$bar_pb <- renderPlot({ #price ratio 3
    sector() %>%
      ggplot(aes(reorder(names, pb), pb, fill=pb)) +
      geom_bar(stat="identity", width=0.5) +
      scale_fill_distiller(palette = 3, guide = FALSE, direction = 1)+
      coord_flip() +
      theme_classic() +
      ylab("PB")+
      xlab("")
  })
  
  output$scatter_profit <- renderPlot({
    sector() %>% #profitability ratios
      ggplot(aes(profit_margin, roa,
                 size= marketcap,
                 color=roe,
                 label= names)) +
      geom_point() +
      scale_color_gradient2(low="red",high="blue")+
      geom_text(check_overlap = TRUE,
                size=5, color= "Black",
                vjust = 1) +
      theme_grey()+
      ylab("Return on Asset") +
      xlab("Profit Margin") +
      scale_size_area()
  })
  
  output$parallel_liquid <- renderPlot({ 
    sector() %>%  #liquidity ratios
      ggparcoord(columns = c(13:16), groupColumn = 1,
                 showPoints = TRUE, 
                 scale= "center" ,
                 alphaLines = 1) + 
      scale_color_discrete() +
      scale_size_area() +
      theme_grey() +
      xlab("") +
      ylab("")
  })
}



# Run the App -------------------------------------------------------------
shinyApp(ui=ui, server=server)

