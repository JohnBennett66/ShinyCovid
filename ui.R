### UI SCRIPT
### 

### LOAD PACKAGES
library(rsconnect)
library(censusapi)
library(fpp2)
library(scales)
library(shiny)
library(ggplot2)
library(ggrepel)
library(stringr)
# library(wbstats)
library(dplyr)
library(data.table)
library(lubridate)
library(usmap)
library(maps)
library(shinythemes)
library(RSocrata)
###
options(datatable.optimize=1)

# this is just for testing things, it's text assigned to a variable that can be used to 
# test if variables can be displayed properly in diffferent functions, etc.
popsicle <- c("a big bunch of text explaning a thing or two about something or another.")
# icon("icon", class = NULL, lib = "font-awesome")

################################################# ###
###  PAGE LAYOUT :: FLUID PAGE :: NAVBAR AT TOP  ####
################################################# ###
fluidPage(title = "Worldwide COVID-19 Data Tracking App", 
          # themeSelector(), # turn this on to try different built in styles
  ### NAVBAR ####
  navbarPage("COVID-19 DATA TRACKING", theme = shinytheme("paper"), 
		
############################ ###
###  HOME MENU & TAB PANEL  ####
###  SINGLE TAB PANEL        ###
############################ ###
    tabPanel(icon("home", class = NULL, lib = "font-awesome"), fluid = TRUE,
      h2("Welcome to My COVID-19 Tracker", .noWS = 'before'),
      fluidRow(
        ###  INTRO STATEMENT  ####
		    column(6,
    			HTML("This app provides some analysis of the Covid-19 data worldwide. <br>
    			The analysis comes from a USA-centric perspective, but feel free to make suggestions. 
    				(see the About tab) <br>
    			The menu structure should be self explanatory. <br>
    			I hope you find this useful. <br>
    			 - John Bennett <br>
    			 <br><br>
    			 <h5><b>NOTE: </b>This is a new version and is not 100% finished. Sorry if anything 
    			     does not work. Also, check back regularly to see new updates. 
    			     <b style='color:rgb(33, 150, 243)'>v2.0::released::16 Oct 2020</b></h5>"),
		    ), # column one

 ###  MENU LEGEND  ####
			  column(6, 
			    p(HTML("&nbsp;&nbsp;MENU LEGEND"), style="background-color:rgb(51,51,51); color:white", 
			      .noWS = 'after'),  
			    HTML("&nbsp;&nbsp;"), icon("home"), HTML(":&nbsp;&nbsp;The <b>Home</b> Page (this page)"), br(),
			    HTML("&nbsp;&nbsp;"), icon("graduation-cap"), 
			        HTML(":&nbsp;&nbsp;The <b>Learning</b> Page (learn about the data and why we 
			             choose certain metrics<sup>*</sup>)"), br(),
			    HTML("&nbsp;&nbsp;"), icon("globe"), HTML(":&nbsp;&nbsp;The <b>World</b> Page 
			                        (Worldwide data; overview, ranking, trends, comparisons)"), br(),
			    HTML("&nbsp;&nbsp;"), icon("flag-usa"), HTML(":&nbsp;&nbsp;The <b>United States</b> Page 
			                           (United States data; overview, rankings, trends, compaisons)"), br(),
			    HTML("&nbsp;&nbsp;"), icon("disease"), 
			        HTML(":&nbsp;&nbsp;The <b>Other Diseases</b> Page (all diseases data; overview, 
			             rankings, trends, compaisons<sup>&dagger;</sup>)"), br(),
			    HTML("&nbsp;&nbsp;<strong>About:</strong>&nbsp;&nbsp;The <b>About</b> Page 
			         (about the data and the author, including links)"), br(),
			    HTML("&nbsp;&nbsp;&nbsp;&nbsp;<sup>*</sup> <u>metrics</u>::data is something like 'the 
			         # of covid cases' and <u>metrics</u> are like 'the # of covid cases 
			         <i>per</i> 100,000 people'"), br(),
			    HTML("&nbsp;&nbsp;&nbsp;&nbsp;<sup>&dagger;</sup> currently only the US"), br(),
			  ) # column two
		  ), # fluid row
			hr(),

###  OVERVIEW :: CURRENT SNAPSHOT  ####
			h4("Current Stats"),
			HTML("<em>Data current as of :  </em>"), strong(HTML(display.date)),

###  WORLD 
			HTML("<h5><strong>Worldwide</strong></h5>"),
			HTML("Cumulative Cases ::  "), strong(HTML(comma(world.cases))), 
			HTML(" &nbsp;&nbsp;==>>&nbsp;&nbsp; Weekly Increase ::  "), 
			strong(HTML(percent(world.cases.increase, accuracy = 0.01))), br(), 
			HTML("Cumulative Deaths ::  "), strong(HTML(comma(world.deaths))), 
			HTML(" &nbsp;&nbsp;==>>&nbsp;&nbsp; Weekly Increase ::  "), 
			  strong(HTML(percent(world.deaths.increase, accuracy = 0.01))), br(), 
			div(class = "row", style = "height: 210px",
			  div(class = "col-sm-3", style = "height: 210px",
			          HTML("New Cases :: Daily Trend"),
			          plotOutput('world_growth_cases_daily')
			  ),
			  div(class = "col-sm-9", style = "height: 210px",
			         HTML("New Deaths :: Daily Trend"),
			         plotOutput('world_growth_deaths_daily') 
			  )
		  ), 
			div(class = "row", style = "width: 500px", style = "height: 50px",

###  UNITED STATES 
			    HTML("<h5><strong>United States</strong></h5>"),
			    HTML("Cumulative Cases ::  "), strong(HTML(comma(us.cases))), 
    			HTML(" &nbsp;&nbsp;==>>&nbsp;&nbsp; Weekly Increase ::  "), 
    			strong(HTML(percent(us.cases.increase, accuracy = 0.01))), br(), 
    			HTML("Cumulative Deaths ::  "), strong(HTML(comma(us.deaths))), 
    			HTML(" &nbsp;&nbsp;==>>&nbsp;&nbsp; Weekly Increase ::  "), 
    			strong(HTML(percent(us.deaths.increase, accuracy = 0.01))), 
			),br(), 
			div(class = "row", style = "height: 210px",
			  div(class = "col-sm-3", style = "height: 210px",
			         HTML("New Cases :: Daily Trend"),
			         plotOutput('us_growth_cases_daily')
			  ),
			  div(class = "col-sm-9" , style = "height: 210px",
			         HTML("New Deaths :: Daily Trend"),
			         plotOutput('us_growth_deaths_daily') 
			  )
			),
			hr(),
			HTML("<strong>NOTE: </strong>This is 'Version 2', in case it looks different 
			     from the last time you visited."
			)
		), #tabpanel welcome
		
################################ ###
###  LEARNING MENU & TAB PANEL  ####
###  SINGLE MENU ITEM            ###
################################ ###
		navbarMenu(icon("graduation-cap", class = NULL, lib = "font-awesome"), 
			tabPanel("Learn About The Data", fluid = TRUE,
				h3("How Should We Count COVID-19:"),
				sidebarLayout(
  				sidebarPanel(
					  # shinythemes::themeSelector(), # for choosing new themes
					  selectInput('learn_type', 
					              HTML("Which Data Should We Use? <p>Cases or Deaths?</p>"),
					              c("Cases" = "cases", "Deaths" = "deaths")),
					  width = 2
					), # sidebarpanel :: input dropdown :: which data
					mainPanel(
						h4("Cumulative"),
						fluidRow(style="height:350px",
						  column(5, style="height:350px", 
  						  plotOutput('plot_today_world'), 
						  ),
						  column(5, style="height:350px", 
  						  htmlOutput('text_today_world'),
						  )
						),
						h4("Cumulative per 100,000 population"), 
						fluidRow(style="height:350px", 
						  column(5, style="height:350px", 
						    plotOutput('plot_today_world_100k'), 
						  ),
						  column(5, style="height:350px", 
						    htmlOutput('text_today_world_100k'), 
						  )
						), 
						h4("Cumulative per 100,000 percent change"),
						fluidRow(style="height:350px",  
						  column(5, style="height:350px", 
    						plotOutput('plot_today_world_100k_pctchg'), 
						  ), 
						  column(5, style="height:350px", 
    						HTML("<strong>NOTE: </strong>The percent change is week over week. <br>  
      							The formula :: ( (today&lsquo;s value - last week&lsquo;s value) &#247; 
      							last week&lsquo;s value). <br> 
      							Also, not all terms are used technically. This is for a lay audience and <br> 
      							often uses terms colloquially to faciliate understanding for the target audience. 
      							<br> <br>"
    						    )
						  )
						)
					) # mainpanel :: cases/deaths 
				) # sidebarlayout
			), #tabpanel :: learn about the data
			
			tabPanel("Learn About Visualizations", fluid = TRUE, 
			         h3("Learn About How Charts, Graphs, and Plots Help Make Data Understandable :: COMING SOON")
			  
			) #tabpanel :: learn about visualizations
		),  # navbarmenu :: education

############################# ###
###  WORLD MENU & TAB PANEL  ####
###  MULTIPLE MENU ITEMS      ###
############################# ###
		navbarMenu(icon("globe", class = NULL, lib = "font-awesome"),
     tabPanel("Overview", fluid = TRUE,
        h3("The Worldwide Overview"),
        sidebarLayout(
          sidebarPanel(
            # shinythemes::themeSelector(),
            selectInput('world_type', p("Which Data? Cases or Deaths", style="font-size:13px"),
                        c("Daily Cases" = "cases", "Daily Deaths" = "deaths")
            ),
            width = 2
          ), # sidebarpanel
        mainPanel(
          h4("Cumulative Cases per 100,000 Population"),
          fluidRow(style="height:350px",
                   column(5, style="height:350px",
                          plotOutput('plot_overview_world_100k'),
                   ),
                   column(5, style="height:350px",
                          htmlOutput('text_overview_world_100k'),
                   )
          ),
          h4("Overview for the World Overall"),
            fluidRow(style="height:200px",
              column(5, style="height:200px",
                div(class = "row", style = "width: 500px", style = "height: 50px",
                  HTML("<h5><strong>The World</strong></h5>"),
                  HTML("<h6>Cumulative Data</h6>"), 
                  HTML("Cases ::  "), strong(HTML(comma(world.cases))), 
                    HTML(" &nbsp;&nbsp;==>>&nbsp;&nbsp; Last Week"), strong(HTML(comma(world.cases.lastweek))), br(),
                  HTML("Deaths ::  "), strong(HTML(comma(world.deaths))), 
                    HTML(" &nbsp;&nbsp;==>>&nbsp;&nbsp; Last Week"), strong(HTML(comma(world.deaths.lastweek))), br(), 
                  HTML("Cases/100k People :: "), strong(HTML(comma(world.cases/p2020[, sum(pop)*10]))), 
                    HTML(" &nbsp;&nbsp;==>>&nbsp;&nbsp; Weekly Increase ::  "),
                    strong(HTML(percent(world.cases.increase, accuracy = 0.01))), br(),
                  HTML("Deaths/100k People :: "), strong(HTML(comma(world.deaths/p2020[, sum(pop)*10]))), 
                    HTML(" &nbsp;&nbsp;==>>&nbsp;&nbsp; Weekly Increase ::  "), 
                    strong(HTML(percent(world.cases.increase, accuracy = 0.01))), br(),
                  
                  ),br(),


                           ),
                           column(5, style="height:200px",
                                  HTML("other stuff :: coming soon")
                           )
                  ),
                  HTML("<strong>NOTE: </strong>The percent change is week over week. <br>
  			The formula :: ( (today&lsquo;s value - last week&lsquo;s value) &#247;
  			last week&lsquo;s value). <br>
  			Also, not all terms are used technically. This is for a lay audience and <br>
  			often uses terms colloquially to faciliate understanding for the target audience.
  			<br> <br>"
                  )
                ) # mainpanel
              ) # sidebarlayout
     )#, #tabpanel :: overview

  #    ###  WORLD :: MENU DIVIDER  ####
  #    '------',
  #    
  #    ###  WORLD :: RANKINGS MENU ITEM AND TAB PANEL  ####
  #    tabPanel("World Rankings", fluid = TRUE, 
  #             # shinythemes::themeSelector(),
  #             sidebarLayout(
  #               sidebarPanel(
  #                 selectInput('world_rank', 
  #                             p("Select Sort Order", style="font-size:13px"),
  #                             c("Cases/100k" = "ccper100k", 
  #                               "Deaths/100k" = "cdper100k", 
  #                               "Cases Change" = "cc_pctchg", 
  #                               "Deaths Change" = "cd_pctchg", 
  #                               "Mortality Rate" = "mortality")
  #                 ), width = 2
  #               ), #sidebar
  #               mainPanel(
  #                 fluidRow(
  #                   column(5, 
  #                          HTML("<h4>Statistics Sorted by Selected Metric<sup>&dagger;</sup></h4>"), 
  #                          tableOutput('world_ranking'), 
  #                          HTML("<b><sup>&dagger;</sup> Selected Metric is in first column.<br>
  #           Selected Metric is sorted in descending order (highest value at the top).<br>
  #           The data is current as of </b>", strong(HTML(display.date)))
  #                   ), #col1
  #                   column(5, 
  #                          h4("Chart for Selected Metric"), 
  #                          plotOutput('plot_today_world_rank'), 
  #                          htmlOutput('text_today_world_rank')
  #                   ) #col2
  #                 ) #row 
  #               ), #main
  #             ), #layout
  #    ), # tab-"us rankings"
  #    
  #    ###  US :: TRENDS MENU ITEM AND TAB PANEL  ####
  #    tabPanel("US Trends", fluid = TRUE,
  #             h3("The World Trending:"),
  #             
  #             # shinythemes::themeSelector(),
  #             selectInput('world_trend', p("Which Data? Cases or Deaths", style="font-size:13px"),
  #                         c("Cases/100k" = "cases", 
  #                           "Deaths/100k" = "deaths", 
  #                           "Cases Change" = "cc_pctchg", 
  #                           "Deaths Change" = "cd_pctchg")
  #             ),
  #             
  #             
  #             
  #             h4("US Trends :: Since 21 January 2020"),
  #             fluidRow(
  #               h4("States Trending"),
  #               column(6, 
  #                      h5("Daily"), 
  #                      plotOutput('world_daily_cases')
  #               ), 
  #               column(6, 
  #                      h5("Discussion"),
  #                      htmlOutput('world_daily_cases_text'),
  #                      plotOutput('world_cases_percent_detail')
  #               )
  #             ),
  #             fluidRow(
  #               column(9, 
  #               ), #col1
  #               column(3, 
  #                      selectInput('world_trend_table', p("Which Data? Cases or Deaths", style="font-size:13px"),
  #                                  c("Cases/100k" = "cases", 
  #                                    "Deaths/100k" = "deaths", 
  #                                    "Cases Change" = "cc_pctchg", 
  #                                    "Deaths Change" = "cd_pctchg"))
  #               ) #col2
  #             ) #row
  #             
  #             
  #    ) #tabpanel us trends
  #            
		), # navbarmenu :: the world








##################################### ###
###  UNITED STATES MENU & TAB PANEL  ####
###  MULTIPLE MENU ITEMS              ###
##################################### ###
    navbarMenu(icon("flag-usa", class = NULL, lib = "font-awesome"), 
               
###  US :: OVERVIEW MENU ITEM AND TAB PANEL  ####
			tabPanel("Overview", fluid = TRUE,
				h3("The United States Overview"),
				sidebarLayout(
					sidebarPanel(
					# shinythemes::themeSelector(),
						selectInput('us_type', p("Which Data? Cases or Deaths", style="font-size:13px"),
												c("Daily Cases" = "cases", "Daily Deaths" = "deaths")
						),
						width = 2
					), # sidebarpanel
					mainPanel(
						h4("Cumulative Cases per 100,000 Population"), 
						fluidRow(style="height:350px",  
						  column(5, style="height:350px", 
						    plotOutput('plot_overview_us_100k'), 
						  ), 
						  column(5, style="height:350px", 
    						htmlOutput('text_overview_us_100k'),
    					) 
						),
						fluidRow(style="height:40px", 
						         column(5, h4("Overview for the US Overall")),
						         column(5, h4("Overview for State Rankings")),
						), # row :: headlines
						fluidRow(style="height:150px", 
						  column(5, style="height:150px", 
				         div(class = "row", style = "width: 500px", style = "height: 50px",
				             HTML("<h5><strong>United States</strong></h5>"),
				             HTML("Cumulative Cases ::  "), strong(HTML(comma(us.cases))), 
				             HTML(" &nbsp;&nbsp;==>>&nbsp;&nbsp; Weekly Increase ::  "), 
				             strong(HTML(percent(us.cases.increase, accuracy = 0.01))), br(), 
				             HTML("Cumulative Deaths ::  "), strong(HTML(comma(us.deaths))), 
				             HTML(" &nbsp;&nbsp;==>>&nbsp;&nbsp; Weekly Increase ::  "), 
				             strong(HTML(percent(us.deaths.increase, accuracy = 0.01))), br(),
				             HTML("Cases per 100,000 People ::  "), 
				             strong(HTML(comma(us.allup[date == reporting.date, ccper100k]))), 
				             HTML(" &nbsp;&nbsp;==>>&nbsp;&nbsp; Weekly Increase ::  "), 
				             strong(HTML(percent(us.allup[date == reporting.date, cc_pctchg]))), br(),
				             HTML("Deaths per 100,000 People ::  "), 
				             strong(HTML(comma(us.allup[date == reporting.date, cdper100k]))), 
				             HTML(" &nbsp;&nbsp;==>>&nbsp;&nbsp; Weekly Increase ::  "), 
				             strong(HTML(percent(us.allup[date == reporting.date, cd_pctchg])))
				         ),br(), 
						         
						         
						         ),
						  column(5, style="height:150px", 
						         HTML("Top States :: Coming Soon")
						         )
						),
						hr(style="border-top-color:rgb(33,150,243)"),
						HTML("<strong>NOTE: </strong>The percent change is week over week. <br>  
									The formula :: ( (today&lsquo;s value - last week&lsquo;s value) &#247; 
									last week&lsquo;s value). <br> 
									Also, not all terms are used technically. This app is for a lay audience and <br> 
									often uses terms colloquially to faciliate understanding for the target audience. 
									<br> <br>"
						)
					) # mainpanel
				) # sidebarlayout
			), #tabpanel where we are now

###  US :: MENU DIVIDER  ####
			'------',

###  US :: RANKINGS MENU ITEM AND TAB PANEL  ####
			tabPanel("US Rankings", fluid = TRUE, 
			         # shinythemes::themeSelector(),
			  sidebarLayout(
			    sidebarPanel(
			      selectInput('us_rank', 
			                        p("Select Sort Order", style="font-size:13px"),
			                        c("Cases/100k" = "ccper100k", 
			                          "Deaths/100k" = "cdper100k", 
			                          "Cases Change" = "cc_pctchg", 
			                          "Deaths Change" = "cd_pctchg", 
			                          "Mortality Rate" = "mortality")
			      ), width = 2
			    ), #sidebar
			    mainPanel(
			      fluidRow(
			        column(5, 
                 HTML("<h4>Statistics Sorted by Selected Metric<sup>&dagger;</sup></h4>"), 
                 tableOutput('us_ranking'), 
                 HTML("<b><sup>&dagger;</sup> Selected Metric is in first column.<br>
                      Selected Metric is sorted in descending order (highest value at the top).<br>
                      The data is current as of ", display.date,"</b><br><br>")
			        ), #col1
			        column(5, 
	               h4("Chart for Selected Metric"), 
	               plotOutput('plot_today_us_rank')
			        ) #col2
			      ) #row 
			    ), #main
			  ), #layout
			), # tab-"us rankings"
			        
###  US :: TRENDS MENU ITEM AND TAB PANEL  ####
			tabPanel("US Trends", fluid = TRUE,
					 h3("The US Trending:"),
					 
					 # shinythemes::themeSelector(),
					 selectInput('us_trend', p("Which Data? Cases or Deaths", style="font-size:13px"),
					             c("Cases/100k" = "cases", 
					               "Deaths/100k" = "deaths", 
					               "Cases Change" = "cc_pctchg", 
					               "Deaths Change" = "cd_pctchg")
					 ), 
				 
				 
				 
				   h4("US Trends :: Since 21 January 2020"),
				   fluidRow(
				     h4("States Trending"),
				      column(6, 
				             h5("Daily"), 
				             plotOutput('us_daily_cases')
				             ), 
				      column(6, 
				             h5("Discussion"),
				             htmlOutput('us_daily_cases_text'),
				             plotOutput('cases_percent_detail')
				             )
				   ),
					 fluidRow(
					   column(9, 
					          ), #col1
					   column(3, 
					          selectInput('us_trend_table', p("Which Data? Cases or Deaths", style="font-size:13px"),
					                      c("Cases/100k" = "cases", 
					                        "Deaths/100k" = "deaths", 
					                        "Cases Change" = "cc_pctchg", 
					                        "Deaths Change" = "cd_pctchg"))
					 ) #col2
					 ) #row

						 
			) #tabpanel us trends


		),  # navbarmenu
             
    tabPanel("About", fluid = TRUE,
			mainPanel(
				h3("About the Data"),
				p("This data is provided from John Hopkins and other sources."),
				p("The data is available from dataworld.com,
					which is where I refresh the data for this app."),
				p("The visualizations are mine, not that they are so fancy."),
				p("Regarding the app, I had inspiration/assistance from the
					NCAA Swim Team Finder app in the Shiny gallery."),
				p("You can see it here, ",
					a(href="https://shiny.rstudio.com/gallery/ncaa-swim-team-finder.html",
						"NCAA Swim Team Finder Shiny App Example")
				),				
				h3("About the Author"),
				p("I am John Bennett. I am a data scientest looking to
					improve my skills so, I built this Shiny app for fun."),
				p("If you have suggestions for this app or want to reach
					me for any reason, you can find me here."),
				p("Github :: JohnBennett66, ", 
				  a(href = "http://github.com/JohnBennett66/ShinyCovid", "github.com/JohnBennett66/ShinyCovid")),
				p("also,"),
				p("Twitter ::  @john_bennett"),
				p("LinkedIn :: https://www.linkedin.com/in/johnbennett/"),
				p(),
				p("This app can also be reached at ",a(href="http://bit.ly/jb66ct", "bit.ly/jb66ct"))													
      ) # mainpanel
    ), # ABOUT TAB   
	tabPanel(htmlOutput('current_data_date'))
  ), # navbarpage
) #fluidpage
