## app.R ##
#Packages <- c("shiny", "shinydashboard","reshape2","ggplot2","PerformanceAnalytics","corrplot","rsconnect","TTR","dplyr","curl")
#lapply(Packages, install.packages, character.only = TRUE)

require(shiny)
require(dplyr)
require(reshape2)
require(ggplot2)
require(shinydashboard)
require(PerformanceAnalytics)
require(corrplot)
require(TTR)
require(rsconnect)
require(curl)

ui <- dashboardPage(
  dashboardHeader(title = "Asset Allocation"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Cross_assets", tabName = "Cross_assets", icon = icon("amazon")),
      menuItem("Equities", tabName = "Equities", icon = icon("th")),
      menuItem("Bonds", tabName = "Bonds", icon = icon("th")),
      menuItem("Trend", tabName = "Trend", icon = icon("th")),
      menuItem("Time_Series", tabName = "Time_Series", icon = icon("th"))
    )
  ),
  
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "Cross_assets",
              h1("자산 전체 모니터링"),
              h2("연도설정 추가"),
              p("최근 1개월은 급락이후 급등 장세를 연출했으나 지난 1주일은 변동성 확대기간을 거침
> ###### 향후 변동성 확대는 불가필 할 것으로 보이나 지난 3월 경험했던 깊이와 넓이는 아닐 것으로 판단"),
              
              fluidPage(
                
                box(
                  title = "01 Cross assets Correlation",
                  plotOutput("plot01", height = 500, width = 700),
                ),
                
                box(
                  title = "02 World Index correlation",
                  plotOutput("plot02", heigh = 500, width = 700)
                  )
                ),
                
                box(
                  title = "03 Global Equity Performance",
                  plotOutput("plot03", height = 500, width = 700),
                ),
                

                box(
                  title = "04 Global MSCI Index Returns 2010 ~ 2019",
                  plotOutput("plot04", height = 500, width = 700),
                  
                  )
      ),
      
      # Second tab content
      tabItem(tabName = "Equities",
              h1("Equities tab content_h1"),
              p("기간 설정: 2008 ~"),
              
              fluidPage(
                box(
                  title = "05 Return / Volatility",
                  plotOutput("plot05", height = 500, width = 500)
                ),
                
                box(
                  title =  "06 Efficient Frontier",
                  plotOutput("plot06", height = 500, width = 500)
                )
              )
      ),
      
      tabItem(tabName = "Bonds",
              h1("채권 퍼포먼스"),
              p("자산배분을 위한 퍼포먼스와 상관관계"),
              fluidPage(
                box(
                  title = "07 Bond Performance",
                  plotOutput("plot07", height = 500, width = 700)
                ),
                
                box(
                  title = "08 Bond correlation",
                  plotOutput("plot08", height = 500, width = 700)
                )
              )
      ),
      
      tabItem(tabName = "Trend",
              h1("short-term trend / momentum"),
              p("장기 특성과 비교하여 단기 트렌드의 변화를 모니터링하기 위함."),
              fluidRow(
                box(
                  title = "09 Technical Analysis_RSI: ACWI",
                  plotOutput("plot09", height = 500, width = 700)
                )
              )
      ),
      
      tabItem(tabName = "Time_Series",
              h1("check charts"),
              fluidRow(
                sidebarPanel(
                                dateRangeInput('days', 'Select a date range:', 
                                               start = Sys.Date() - 365,
                                               end = Sys.Date(), 
                                               max = Sys.Date()
                                               )
                ),
                box(
                  title = "10 Check Performance",
                  plotOutput("plot10", height = 500, width = 700),
                  plotOutput("plot11", height = 500, width = 700)
                )
              )
        
      )
      
    )
  )
)


##############################################################
##--#       server        --------------------------------####
##############################################################

server <- function(input, output) {
  
  # read RDS
  rets_month_equity <- readRDS("equity_compact_monthlyReturn.RDS")
  rets_week_equity <- readRDS("equity_compact_weeklyReturn.RDS")
  prices_day_equity <- readRDS("equity_compact_daily.RDS")
  rets_month_bond <- readRDS("bond_monthlyReturn.RDS")
  assets <- readRDS("assets_monthlyReturn.RDS")
  
  cross_assets <- readRDS("cross_assets.RDS") %>% na.omit
  cross_assets_m <- to.monthly(cross_assets, indexAt = "lastOf", OHLC = FALSE)
  
  
  rets_eq_2010_2019 <- rets_month_equity['2010-01-01/2019-12-31']
  rets_bond_2015_2019 <- rets_month_bond['2015-01-01/2019-12-31']
  cross_assets_m_2015_2019 <- cross_assets_m['2015-01-01/2019-12-31']
  
  # Technical
  prices_day_equity <- prices_day_equity['2020-01-01/2020-05-27']
  cormat_equity_monthly <- cor(rets_eq_2010_2019)
  cormat_bond_monthly <- cor(rets_bond_2015_2019)
  
  
  covs = cov(rets_eq_2010_2019)*12
  yearly_mean <- Return.annualized(rets_eq_2010_2019, geometric = FALSE) %>% c()
  yearly_vol <- StdDev.annualized(rets_eq_2010_2019) %>% c()
  yearly_stat <- cbind(yearly_mean, yearly_vol) %>% data.frame()
  Indexes <- c(as.character(colnames(rets_eq_2010_2019)))
  
  ################### Correlation Matrix ###################
  get_lower_tri <- function(cormat){
    cormat[upper.tri(cormat)] <- NA
    return(cormat)
  }
  get_upper_tri <- function(cormat){
    cormat[lower.tri(cormat)] <- NA
    return(cormat)
  }
  
  output$plot01 <- renderPlot({
    
    Year <- as.factor(format(index(cross_assets_m_2015_2019),'%Y'))
    chart.Correlation(cross_assets_m_2015_2019,bg=seq(1:12)[Year],pch=21)
    
  })

  
  output$plot02 <- renderPlot({
    
    lower_melted_Equities <- get_lower_tri(cormat_equity_monthly)
    tri_melted_corEquity <- melt(lower_melted_Equities, na.rm = TRUE)
    
    ggplot(data = tri_melted_corEquity, aes(Var2, Var1, fill = value))+
      geom_tile(color = "gray") + scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = .5, limit = c(0,1), space = "Lab", name="Equities Correlation") + theme_minimal() + theme(text = element_text(size=11), axis.text.x = element_text(angle = 90, vjust = 1, size = 11,  hjust = 1)) + coord_fixed() + theme(axis.title=element_blank())
 
    
  })
  
  output$plot03 <- renderPlot({
    
    charts.PerformanceSummary(rets_eq_2010_2019, main = "Equity Index Returns")
    
  })
  
  
  output$plot04 <- renderPlot({
    
    corrplot(cor(assets), type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)
    
  })
  
  output$plot05 <- renderPlot({

    yearly_stat %>%
      ggplot(aes(x = yearly_vol, y = yearly_mean, label = Indexes, color = Indexes )) +
      geom_point(shape = 5) +
      scale_x_continuous(expand = c(0.02, 0.02)) + 
      scale_y_continuous(expand = c(0.02, 0.02)) +
      geom_text(size = 3, vjust = -0.7) +
      theme(legend.position = '') +
      xlab('Volatility') +
      ylab('Return')
  
    
  })
  
  output$plot06 <- renderPlot({
    
    # Simulation
    numAssets = length(colnames(rets_eq_2010_2019))
    numPortfolio = 10000
    sim = list()
    
    for (i in 1 : numPortfolio) {
      
      wt = runif(numAssets, min=0, max=1)
      wt = wt / sum(wt)
      expected_return = wt %*% yearly_mean
      expected_vol = (t(wt) %*% covs %*% wt) %>% sqrt()
      expected_sharpe = (expected_return-0.01) / expected_vol
      
      sim[[i]] = c(wt, expected_return, expected_vol, expected_sharpe)
      
    }
    
    sim = do.call(rbind, sim) %>% data.frame()
    colnames(sim) = c(Indexes, 'Return', 'Volatility', 'Sharpe')
    
    target_minvol = sim[which.min(sim$Volatility), ]
    target_maxsharpe = sim[which.max(sim$Sharpe), ]
    
    # plot simulation
      sim %>%
      ggplot(aes(x = `Volatility`, y = `Return`, color = `Sharpe`)) +
      geom_point() +
      scale_color_gradient(low = 'grey', high = 'red') +
      theme_classic() +
        geom_point(data = target_minvol,
                   aes(x = Volatility, y = Return),
                   col = 'black', shape = 18, size = 3) +
        geom_point(data = target_maxsharpe,
                   aes(x = Volatility, y = Return),
                   col = 'black', shape = 18, size = 3) +
        annotate('text',
                 x = target_minvol$Volatility + 0.03,
                 y = target_minvol$Return,
                 label='Min Vol',
                 fontface=2) +
        annotate('text',
                 x = target_maxsharpe$Volatility + 0.03,
                 y = target_maxsharpe$Return,
                 label='Max Sharpe',
                 fontface=2) +
        geom_point(data = yearly_stat,
                   aes(x = yearly_vol, y = yearly_mean),
                   color = 'black', shape = 4, size = 3) + 
        geom_abline(intercept = 0.01,
                    slope = target_maxsharpe$Sharpe,
                    size = .1) +
        expand_limits(x=0) +
        expand_limits(y=0) 
    
  })
  
  output$plot07 <- renderPlot({
    
    charts.PerformanceSummary(rets_bond_2015_2019, main = "Bond Index Returns")
    
  })
  
  output$plot08 <- renderPlot({
    
    Year <- as.factor(format(index(rets_bond_2015_2019),'%Y'))
    chart.Correlation(rets_bond_2015_2019,bg=seq(1:8)[Year],pch=21)
    
  })
  
    output$plot09 <- renderPlot({

    rsi <- RSI(prices_day_equity$`MSCI ACWI`, n = 5)    
    plot(rsi, main = "RSI 5 days", xlab = "time", ylab = "RSI: 5days")
    
  })
    
    output$plot10 <- renderPlot({
      
      filtered_df <- reactive({
        cross_A %>%
          filter(between(date, input$days[1], input$days[2]))
      })
      
      #ggplot(filtered_df(), aes_string(x = "date", y = c("MSCI.ACWI", "GOLD"))) + geom_line() + geom_point() + ggtitle("ACWI plot")
      plot(melt(filtered_df()[,-1]))
      
      
    })

}



shinyApp(ui, server)

