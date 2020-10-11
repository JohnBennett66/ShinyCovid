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
library(wbstats)
library(dplyr)
library(data.table)
library(lubridate)
library(usmap)
library(maps)
library(shinythemes)
library(RSocrata)
###
options(datatable.optimize=1)

popsicle <- c("a big bunch of text explaning a thing or two about something or another.")
# icon("icon", class = NULL, lib = "font-awesome")

###  PAGE LAYOUT :: MAIN & SIDEBAR :: SINGLE PLOT  ####
fluidPage(title = "Worldwide COVID-19 Data Tracking App", 
          # themeSelector(),
  navbarPage("COVID-19 DATA TRACKING", theme = shinytheme("yeti"), 
		tabPanel(icon("home", class = NULL, lib = "font-awesome"), fluid = TRUE,
      h2("Welcome to My COVID-19 Tracker", .noWS = 'before'),
      fluidRow(
		    column(6,
    			HTML("This app provides some analysis of the Covid-19 data worldwide. <br>
    			The analysis comes from a USA-centric perspective, but feel free to make suggestions. 
    				(see the About tab) <br>
    			The menu structure should be self explanatory. <br>
    			I hope you find this useful. <br>
    			 - John Bennett <br>"),
		    ), # column one
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
			h4("Current Stats"),
			HTML("<em>Data current as of :  </em>"), strong(HTML(display.date)),
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
		navbarMenu(icon("graduation-cap", class = NULL, lib = "font-awesome"), 
			tabPanel("Learn About The Data", fluid = TRUE,
				h3("How Should We Count COVID-19:"),
				sidebarLayout(
  				sidebarPanel(
					  # shinythemes::themeSelector(),
					  selectInput('world_type', 
					              HTML("Which Data Should We Use? <p>Cases or Deaths?</p>"),
					              c("Cases" = "cases", "Deaths" = "deaths")),
					  width = 2
					), # sidebarpanel :: input dropdown :: which data
					mainPanel(
						h4("Cumulative Cases"),
						fluidRow(style="height:350px",
						  column(5, style="height:350px", 
  						  plotOutput('plot_today_world'), 
						  ),
						  column(5, style="height:350px", 
  						  htmlOutput('text_today_world'),
						  )
						),
						h4("Cumulative Cases per 100,000 population"), 
						fluidRow(style="height:350px", 
						  column(5, style="height:350px", 
						    plotOutput('plot_today_world_100k'), 
						  ),
						  column(5, style="height:350px", 
						    htmlOutput('text_today_world_100k'), 
						  )
						), 
						h4("Cumulative Cases per 100,000 percent change"),
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
			) #tabpanel :: where are we
		),  # navbarmenu :: education
		navbarMenu(icon("globe", class = NULL, lib = "font-awesome"),
		  tabPanel("stuff goes here", fluid = TRUE 
		  ) # tab panel :: stuff goes here
		), # navbarmenu :: the world
		### ### ### ### ### ###
		###  THE US MENU   ####
		### ### ### ### ### ###
		navbarMenu(icon("flag-usa", class = NULL, lib = "font-awesome"), 
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
						h4("Overview for the US Overall"),
						fluidRow(style="height:350px", 
						  column(5, style="height:350px", 
						         div(class = "row", style = "width: 500px", style = "height: 50px",
						             HTML("<h5><strong>United States</strong></h5>"),
						             HTML("Cumulative Cases ::  "), strong(HTML(comma(us.cases))), 
						             HTML(" &nbsp;&nbsp;==>>&nbsp;&nbsp; Weekly Increase ::  "), 
						             strong(HTML(percent(us.cases.increase, accuracy = 0.01))), br(), 
						             HTML("Cumulative Deaths ::  "), strong(HTML(comma(us.deaths))), 
						             HTML(" &nbsp;&nbsp;==>>&nbsp;&nbsp; Weekly Increase ::  "), 
						             strong(HTML(percent(us.deaths.increase, accuracy = 0.01))), 
						         ),br(), 
						         
						         
						         ),
						  column(5, style="height:350px", 
						         HTML("other stuff")
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
			), #tabpanel where we are now
			tabPanel("US Rankings", fluid = TRUE, 
			         # shinythemes::themeSelector(),
			  sidebarLayout(
			    sidebarPanel(
			      selectInput('us_rank', 
			                        p("Select Sort Order", style="font-size:13px"),
			                        c("Cases/100k" = "ccper100k", "Deaths/100k" = "cdper100k", 
			                          "Cases Change" = "cc_pctchg", "Deaths Change" = "cd_pctchg")
			      ), width = 2
			    ), #sidebar
			    mainPanel(
			      fluidRow(
			        column(5, 
			               tableOutput('us_ranking') 
			        ), 
			        column(5, 
			               plotOutput('plot_today_us_rank')
			        )
			      ) 
			    ), #main
			  ), #layout
			), # tab-"us rankings"
			        
			        
			tabPanel("US Trends", fluid = TRUE,
					 h3("The States ranked:"),
					 sidebarLayout(
						 sidebarPanel(
							 # shinythemes::themeSelector(),
							 selectInput('us_type', p("Which Data? Cases or Deaths", style="font-size:13px"),
													 c("Daily Cases" = "cases", "Daily Deaths" = "deaths")),
							 width = 2
						 ), # sidebarpanel
						 mainPanel(
						   h4("Overall Trend for the US :: Since 21 January 2020"),
						   plotOutput('plot_us_trend_overall'),
						   # htmlOutput()

						 )
					 )
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
