options("rgdal_show_exportToProj4_warnings"="none")
options(tigris_use_cache = TRUE)
options(scipen = 999)

library(shiny)
library(DT)
library(ggplot2)
library(jpeg)
library(grid)
library(leaflet)
library(leaflet.extras)
library(scales)
library(readxl)
library(sp)
library(rgdal)
library(tigris)
library(rgeos)
library(ggmap)
library(maps)
library(mapdata)
library(maptools)
library(ggthemes)
library(stringr)
library(mapview)
library(RColorBrewer)



egdata <- read.csv("sourcecopy2.csv")
names(egdata)[names(egdata) == "CENSUS.BLOCK"] <- "GEOID10"
SCHI <- subset(tigris::blocks(17, 031,year = 2010), select = c("GEOID10","NAME10","INTPTLAT10","INTPTLON10","geometry"))
bindmap <- merge(SCHI, egdata, by = "GEOID10" )
community <- "All Chicago"
community <- append(community, unique(bindmap$COMMUNITY.AREA.NAME))

dfbindmap <- merge(egdata,SCHI, by = "GEOID10")
dfbindmap <- subset(dfbindmap, select = c("COMMUNITY.AREA.NAME","INTPTLON10","INTPTLAT10"))
dfbindmap$INTPTLON10 <- as.numeric(dfbindmap$INTPTLON10)
dfbindmap$INTPTLAT10 <- as.numeric(dfbindmap$INTPTLAT10)



ui <- navbarPage(
	
	  title = "CS 424 Project3",
		

	  tabPanel(title = "Map", 
		  tags$style(
			  '.navbar { background-color: #a2d2ff;}'
		  ),
		  mainPanel(
			  div(class="outer",
			  tags$style(type = "text/css", ".outer { position: fixed; border-width:5px; border-style:double; width: 60%; height: 80%;left:300px}"),
			  leafletOutput("map", width = "100%", height = "100%")
			  
			  ),
			  absolutePanel(top = 0, right = 0,left = 30, bottom = 0,
			    div(class = "boxstyle1",
					tags$style(type = "text/css", " .boxstyle1{text-indent: 10px;border-width:5px; border-style:double;font-weight: bolder;background: #afeeee; width:225px;height: 50px;}"),
					checkboxInput("legend", "Show legend", TRUE)
				)
			  ),
			  absolutePanel(top = 70, right = 0,left = 30, bottom = 0,
				  div(class = "boxstyle2",
						tags$style(type = "text/css", " .boxstyle2{text-indent: 10px;border-width:5px; border-style:double;font-weight: normal;background: #edf2fb; width:225px;}"),
						selectizeInput("C", "Community: ", community,width = "75%", selected = "Near West Side"),
						selectizeInput("data", "Type: ", list("Electricity", "Gas","Building Type","Building Age","Building Height","Total Population"),width = "75%"),
						selectizeInput("M", "Month: ", list("Total", "January","February","March","April","May","June","July","August","September","October","November", "December"),width = "75%"),
						selectizeInput("T", "Building Type: ", list("All", "Commercial","Residential","Industrial"),width = "75%"),
						selectizeInput("L", "Legend Color: ", list("RdYlGn", "Spectral","YlOrRd"),width = "75%")
				  )
			  ),
			  fixedPanel(top = 15, right = 0,left = "78%", bottom = 0,
				dataTableOutput("table"),
				plotOutput("graph")
			  ),
			  absolutePanel(top = 15, right = 0,left = 350, bottom = 0,
						actionButton("reset", "Reset Map")
			  )
		   )
	  ),

	  tabPanel(title = "Compare", 
		mainPanel(
			div(class="cmpm1",
			  tags$style(type = "text/css", ".cmpm1 {position: fixed;border-width:5px; border-style:double; width: 40%; height: 43%;left: 5%}"),
			  leafletOutput("cmp1", width = "100%", height = "100%")
			 ),
			
			fixedPanel(top = "51%",right = 0,left = "5%", bottom = 0,
				div(class = "boxstyle4",
					tags$style(type = "text/css", " .boxstyle4{text-indent: 10px;border-width:5px; border-style:double;font-weight: normal;background: #edf2fb; width:225px;}"),
					selectizeInput("C2", "Community: ", community,width = "75%", selected = "Near West Side"),
					selectizeInput("data2", "Type: ", list("Electricity", "Gas","Building Type","Building Age","Building Height","Total Population"),width = "75%"),
					selectizeInput("M2", "Month: ", list("Total", "January","February","March","April","May","June","July","August","September","October","November", "December"),width = "75%"),
					selectizeInput("T2", "Building Type: ", list("All", "Commercial","Residential","Industrial"),width = "75%"),
					selectizeInput("L2", "Legend Color: ", list("RdYlGn", "Spectral","YlOrRd"),width = "75%")
				)
			),
			absolutePanel(top = 15, right = 0,left = "11%", bottom = 0,
					actionButton("reset2", "Reset Map")
			),
			div(class="cmpm2",
			  tags$style(type = "text/css", ".cmpm2 {position: fixed;border-width:5px; border-style:double; width: 40%; height: 43%;left: 55%}"),
			  leafletOutput("cmp2", width = "100%", height = "100%")
		    ),
			
			fixedPanel(top = "51%",right = 0,left = "55%", bottom = 0,
				div(class = "boxstyle6",
					tags$style(type = "text/css", " .boxstyle6{text-indent: 10px;border-width:5px; border-style:double;font-weight: normal;background: #edf2fb; width:225px;}"),
					selectizeInput("C3", "Community: ", community,width = "75%", selected = "Loop"),
					selectizeInput("data3", "Type: ", list("Electricity", "Gas","Building Type","Building Age","Building Height","Total Population"),width = "75%"),
					selectizeInput("M3", "Month: ", list("Total", "January","February","March","April","May","June","July","August","September","October","November", "December"),width = "75%"),
					selectizeInput("T3", "Building Type: ", list("All", "Commercial","Residential","Industrial"),width = "75%"),
					selectizeInput("L3", "Legend Color: ", list("RdYlGn", "Spectral","YlOrRd"),width = "75%")
				)
			),
			absolutePanel(top = 15, right = 0,left = "87%", bottom = 0,
					actionButton("reset3", "Reset Map")
			)
		)






	  ),

	  tabPanel(title = "About Page",
		
		mainPanel(
			div(
				class="textstyle1",
				tags$style(type = "text/css", ".textstyle1 {
					position: relative;
					margin: 10px;
					border-width:5px;  
					border-style:double;
					background: #afeeee;
					background-clip: content-box;
					font-weight: bold;
					font-size: large;
					width: 1080px;
					height: 50px;}"),
				textOutput("text1")
			),
			div(
				class="textstyle2",
				tags$style(type = "text/css", ".textstyle2 {
					position: relative;
					margin: 10px;
					border-width:5px;  
					border-style:double;
					background: #afeeee;
					background-clip: content-box;
					font-weight: bold;
					font-size: large;
					width: 1080px;
					height: 50px;}"),
				textOutput("text2")
			),
			div(
				class="textstyle3",
				tags$style(type = "text/css", ".textstyle3 {
					position: relative;
					margin: 10px;
					border-width:5px;  
					border-style:double;
					background: #afeeee;
					background-clip: content-box;
					font-weight: bold;
					font-size: large;
					width: 1080px;
					height: 100px;}"),
				textOutput("text3")
			),
			div(
				class="textstyle4",
				tags$style(type = "text/css", ".textstyle4 {
					position: relative;
					margin: 10px;
					border-width:5px;  
					border-style:double;
					background: #afeeee;
					background-clip: content-box;
					font-weight: bold;
					font-size: large;
					width: 1080px;
					height: 50px;}"),
				textOutput("text4")
			),
			div(
				class="textstyle5",
				tags$style(type = "text/css", ".textstyle5 {
					position: relative;
					margin: 10px;
					border-width:5px;  
					border-style:double;
					background: #afeeee;
					background-clip: content-box;
					font-weight: bold;
					font-size: large;
					width: 1080px;
					height: 50px;}"),
				textOutput("text5")
			)
		)
	  )
	
 )



 server <- function(input,output,session){

	observeEvent(input$reset, {
		updateSelectInput(session,"C", selected = "Near West Side")
		updateSelectInput(session,"data", selected = "Electricity")
		updateSelectInput(session,"M", selected = "Total")
		updateSelectInput(session,"T",selected = "All")
		updateSelectInput(session,"L",selected = "RdYlGn")
		updateCheckboxInput(session,"legend", value = TRUE)
	})

	observeEvent(input$reset2, {
		updateSelectInput(session,"C2", selected = "Near West Side")
		updateSelectInput(session,"data2", selected = "Electricity")
		updateSelectInput(session,"M2", selected = "Total")
		updateSelectInput(session,"T2",selected = "All")
		updateSelectInput(session,"L2",selected = "RdYlGn")
		
	})

	observeEvent(input$reset3, {
		updateSelectInput(session,"C3", selected = "Loop")
		updateSelectInput(session,"data3", selected = "Electricity")
		updateSelectInput(session,"M3", selected = "Total")
		updateSelectInput(session,"T3",selected = "All")
		updateSelectInput(session,"L3",selected = "RdYlGn")
		
	})

	

	observe({

		C <- c(0)

		

		if(input$data == "Electricity"){
			
			C <- "E"
		}

		if(input$data == "Gas"){
			
			C <- "G"
		}

		if(input$data == "Building Type"){
			
			C <- "HBT"
		}

		if(input$data == "Building Age"){
			
			C <- "HBA"
		}

		if(input$data == "Building Height"){
			
			C <- "HBH"
		}

		if(input$data == "Total Population"){
			
			C <- "HTP"
		}

		check <- c(0)

		if(C == "E"){
			if(input$M == "Total"){
				check <- "TOTAL.KWH"
				heatmap <- subset(bindmap, select = c("TOTAL.KWH","COMMUNITY.AREA.NAME"))
			}
			else if(input$M == "January"){
				check <- "KWH.JANUARY.2010"
				heatmap <- subset(bindmap, select = c("KWH.JANUARY.2010","COMMUNITY.AREA.NAME"))
			}
			else if(input$M == "February"){
				check <- "KWH.FEBRUARY.2010"
				heatmap <- subset(bindmap, select = c("KWH.FEBRUARY.2010","COMMUNITY.AREA.NAME"))
			}
			else if(input$M == "March"){
				check <- "KWH.MARCH.2010"
				heatmap <- subset(bindmap, select = c("KWH.MARCH.2010","COMMUNITY.AREA.NAME"))
			}
			else if(input$M == "April"){
				check <- "KWH.APRIL.2010"
				heatmap <- subset(bindmap, select = c("KWH.APRIL.2010","COMMUNITY.AREA.NAME"))
			}
			else if(input$M == "May"){
				check <- "KWH.MAY.2010"
				heatmap <- subset(bindmap, select = c("KWH.MAY.2010","COMMUNITY.AREA.NAME"))
			}
			else if(input$M == "June"){
				check <- "KWH.JUNE.2010"
				heatmap <- subset(bindmap, select = c("KWH.JUNE.2010","COMMUNITY.AREA.NAME"))
			}
			else if(input$M == "July"){
				check <- "KWH.JULY.2010"
				heatmap <- subset(bindmap, select = c("KWH.JULY.2010","COMMUNITY.AREA.NAME"))
			}
			else if(input$M == "August"){
				check <- "KWH.AUGUST.2010"
				heatmap <- subset(bindmap, select = c("KWH.AUGUST.2010","COMMUNITY.AREA.NAME"))
			}
			else if(input$M == "September"){
				check <- "KWH.SEPTEMBER.2010"
				heatmap <- subset(bindmap, select = c("KWH.SEPTEMBER.2010","COMMUNITY.AREA.NAME"))
			}
			else if(input$M == "October"){
				check <- "KWH.OCTOBER.2010"
				heatmap <- subset(bindmap, select = c("KWH.OCTOBER.2010","COMMUNITY.AREA.NAME"))
			}
			else if(input$M == "November"){
				check <- "KWH.NOVEMBER.2010"
				heatmap <- subset(bindmap, select = c("KWH.NOVEMBER.2010","COMMUNITY.AREA.NAME"))
			}
			else if(input$M == "December"){
				check <- "KWH.DECEMBER.2010"
				heatmap <- subset(bindmap, select = c("KWH.DECEMBER.2010","COMMUNITY.AREA.NAME"))
			}
		}

		if(C == "G"){
			

			if(input$M == "Total"){
				check <- "TOTAL.THERMS"
				heatmap <- subset(bindmap, select = c("TOTAL.THERMS","COMMUNITY.AREA.NAME"))
			}
			else if(input$M == "January"){
				check <- "THERM.JANUARY.2010"
				heatmap <- subset(bindmap, select = c("THERM.JANUARY.2010","COMMUNITY.AREA.NAME"))
			}
			else if(input$M == "February"){
				check <- "THERM.FEBRUARY.2010"
				heatmap <- subset(bindmap, select = c("THERM.FEBRUARY.2010","COMMUNITY.AREA.NAME"))
			}
			else if(input$M == "March"){
				check <- "THERM.MARCH.2010"
				heatmap <- subset(bindmap, select = c("THERM.MARCH.2010","COMMUNITY.AREA.NAME"))
			}
			else if(input$M == "April"){
				check <- "TERM.APRIL.2010"
				heatmap <- subset(bindmap, select = c("TERM.APRIL.2010","COMMUNITY.AREA.NAME"))
			}
			else if(input$M == "May"){
				check <- "THERM.MAY.2010"
				heatmap <- subset(bindmap, select = c("THERM.MAY.2010","COMMUNITY.AREA.NAME"))
			}
			else if(input$M == "June"){
				check <- "THERM.JUNE.2010"
				heatmap <- subset(bindmap, select = c("THERM.JUNE.2010","COMMUNITY.AREA.NAME"))
			}
			else if(input$M == "July"){
				check <- "THERM.JULY.2010"
				heatmap <- subset(bindmap, select = c("THERM.JULY.2010","COMMUNITY.AREA.NAME"))
			}
			else if(input$M == "August"){
				check <- "THERM.AUGUST.2010"
				heatmap <- subset(bindmap, select = c("THERM.AUGUST.2010","COMMUNITY.AREA.NAME"))
			}
			else if(input$M == "September"){
				check <- "THERM.SEPTEMBER.2010"
				heatmap <- subset(bindmap, select = c("THERM.SEPTEMBER.2010","COMMUNITY.AREA.NAME"))
			}
			else if(input$M == "October"){
				check <- "THERM.OCTOBER.2010"
				heatmap <- subset(bindmap, select = c("THERM.OCTOBER.2010","COMMUNITY.AREA.NAME"))
			}
			else if(input$M == "November"){
				check <- "THERM.NOVEMBER.2010"
				heatmap <- subset(bindmap, select = c("THERM.NOVEMBER.2010","COMMUNITY.AREA.NAME"))
			}
			else if(input$M == "December"){
				check <- "THERM.DECEMBER.2010"
				heatmap <- subset(bindmap, select = c("THERM.DECEMBER.2010","COMMUNITY.AREA.NAME"))
			}
		}

		if(C == "HBT"){
			heatmap <- subset(bindmap, select = c("BUILDING.TYPE","COMMUNITY.AREA.NAME"))
			if(input$T == "All"){
				check <- "BUILDING.TYPE"
			}
			else if(input$T == "Commercial"){
				check <- "BUILDING.TYPE"
				heatmap <- subset(heatmap, heatmap$BUILDING.TYPE %in% "Commercial")
			}
			else if(input$T == "Residential"){
				check <- "BUILDING.TYPE"
				heatmap <- subset(heatmap, heatmap$BUILDING.TYPE %in% "Residential")
			}
			else if(input$T == "Industrial"){
				check <- "BUILDING.TYPE"
				heatmap <- subset(heatmap, heatmap$BUILDING.TYPE %in% "Industrial")
			}
		}

		if(C == "HBA"){
			
			check <- "AVERAGE.BUILDING.AGE"
			heatmap <- subset(bindmap, select = c("AVERAGE.BUILDING.AGE","COMMUNITY.AREA.NAME"))
		}

		if(C == "HBH"){
			

			check <- "AVERAGE.HOUSESIZE"
			heatmap <- subset(bindmap, select = c("AVERAGE.HOUSESIZE","COMMUNITY.AREA.NAME"))
		}

		if(C == "HTP"){
			

			check <- "TOTAL.POPULATION"
			heatmap <- subset(bindmap, select = c("TOTAL.POPULATION","COMMUNITY.AREA.NAME"))
		}

		COMMUNITY.AREA.NAME <- c("All Chicago")
		INTPTLON10 <- c(-87.623177)
		INTPTLAT10 <- c(41.881832)
		position <- data.frame(COMMUNITY.AREA.NAME,INTPTLON10,INTPTLAT10)

		if(input$C != "All Chicago"){
			position <- subset(dfbindmap, dfbindmap$COMMUNITY.AREA.NAME %in% input$C)
			position <- position[(NROW(position)/2),]
			heatmap <- subset(heatmap, heatmap$COMMUNITY.AREA.NAME %in% input$C)
		}

		if(C == "E" || C == "G"){
			Datatable <- as.data.frame(heatmap)
			Datatable <- subset(Datatable, select = c(check, "COMMUNITY.AREA.NAME"))
			Energy_Use <- sum(Datatable[1], na.rm = TRUE)
			Community <- input$C
			DTable <- data.frame(Energy_Use,Community)
			if(C == "E"){
				output$graph <- renderPlot(ggplot(DTable, aes(x = Community, y = Energy_Use))+ geom_bar(stat='identity', width = 0.5, fill = "#FFC300"))
			}
			else{
				output$graph <- renderPlot(ggplot(DTable, aes(x = Community, y = Energy_Use))+ geom_bar(stat='identity', width = 0.5, fill = "#FF5733"))
			}
			output$table <- renderDataTable(DTable)
		}

		if (input$legend) {
			if(check != "BUILDING.TYPE"){
				if(input$L == "YlOrRd"){
					if(length(heatmap$COMMUNITY.AREA.NAME) != 0){
						m <- mapview(heatmap, zcol = check,col.regions = brewer.pal(11, input$L))
						output$map <- renderLeaflet({
							m@map%>% setView(position$INTPTLON10, position$INTPTLAT10,zoom = 14)
						})
					}
					else{
						m <- mapview()
						output$map <- renderLeaflet({
							m@map%>% setView(position$INTPTLON10, position$INTPTLAT10,zoom = 14)
						})
					}
				}
				else{
					if(length(heatmap$COMMUNITY.AREA.NAME) != 0){
						m <- mapview(heatmap, zcol = check,col.regions = rev(brewer.pal(11, input$L)))
						output$map <- renderLeaflet({
							m@map%>% setView(position$INTPTLON10, position$INTPTLAT10,zoom = 14)
						})
					}
					else{
						m <- mapview()
						output$map <- renderLeaflet({
							m@map%>% setView(position$INTPTLON10, position$INTPTLAT10,zoom = 14)
						})
					}
				}
			}
			else{
				if(input$L == "YlOrRd"){
					if(length(heatmap$COMMUNITY.AREA.NAME) != 0){
						m <- mapview(heatmap, zcol = check,col.regions = brewer.pal(3, input$L))
						output$map <- renderLeaflet({
							m@map%>% setView(position$INTPTLON10, position$INTPTLAT10,zoom = 14)
						})
					}
					else{
						m <- mapview()
						output$map <- renderLeaflet({
							m@map%>% setView(position$INTPTLON10, position$INTPTLAT10,zoom = 14)
						})
					}
				}
				else{
					if(length(heatmap$COMMUNITY.AREA.NAME) != 0){
						m <- mapview(heatmap, zcol = check,col.regions = rev(brewer.pal(3, input$L)))
						output$map <- renderLeaflet({
							m@map%>% setView(position$INTPTLON10, position$INTPTLAT10,zoom = 14)
						})
					}
					else{
						m <- mapview()
						output$map <- renderLeaflet({
							m@map%>% setView(position$INTPTLON10, position$INTPTLAT10,zoom = 14)
						})
					}
				}
			}
		}
		else{
			if(check != "BUILDING.TYPE"){
				if(input$L == "YlOrRd"){
					if(length(heatmap$COMMUNITY.AREA.NAME) != 0){
						m <- mapview(heatmap, zcol = check,col.regions = brewer.pal(11, input$L), legend = FALSE)
						output$map <- renderLeaflet({
							m@map%>% setView(position$INTPTLON10, position$INTPTLAT10,zoom = 14)
						})
					}
					else{
						m <- mapview()
						output$map <- renderLeaflet({
							m@map%>% setView(position$INTPTLON10, position$INTPTLAT10,zoom = 14)
						})
					}
				}
				else{
					if(length(heatmap$COMMUNITY.AREA.NAME) != 0){
						m <- mapview(heatmap, zcol = check,col.regions = rev(brewer.pal(11, input$L)), legend = FALSE)
						output$map <- renderLeaflet({
							m@map%>% setView(position$INTPTLON10, position$INTPTLAT10,zoom = 14)
						})
					}
					else{
						m <- mapview()
						output$map <- renderLeaflet({
							m@map%>% setView(position$INTPTLON10, position$INTPTLAT10,zoom = 14)
						})
					}
				}
			}
			else{
				if(input$L == "YlOrRd"){
					if(length(heatmap$COMMUNITY.AREA.NAME) != 0){
						m <- mapview(heatmap, zcol = check,col.regions = brewer.pal(3, input$L), legend = FALSE)
						output$map <- renderLeaflet({
							m@map%>% setView(position$INTPTLON10, position$INTPTLAT10,zoom = 14)
						})
					}
					else{
						m <- mapview()
						output$map <- renderLeaflet({
							m@map%>% setView(position$INTPTLON10, position$INTPTLAT10,zoom = 14)
						})
					}
				}
				else{
					if(length(heatmap$COMMUNITY.AREA.NAME) != 0){
						m <- mapview(heatmap, zcol = check,col.regions = rev(brewer.pal(3, input$L)), legend = FALSE)
						output$map <- renderLeaflet({
							m@map%>% setView(position$INTPTLON10, position$INTPTLAT10,zoom = 14)
						})
					}
					else{
						m <- mapview()
						output$map <- renderLeaflet({
							m@map%>% setView(position$INTPTLON10, position$INTPTLAT10,zoom = 14)
						})
					}
				}
			}
		}

		
	})

	observe({

		C <- c(0)

		

		if(input$data2 == "Electricity"){
			
			C <- "E"
		}

		if(input$data2 == "Gas"){
			
			C <- "G"
		}

		if(input$data2 == "Building Type"){
			
			C <- "HBT"
		}

		if(input$data2 == "Building Age"){
			
			C <- "HBA"
		}

		if(input$data2 == "Building Height"){
			
			C <- "HBH"
		}

		if(input$data2 == "Total Population"){
			
			C <- "HTP"
		}

		check <- c(0)

		if(C == "E"){
			if(input$M2 == "Total"){
				check <- "TOTAL.KWH"
				heatmap <- subset(bindmap, select = c("TOTAL.KWH","COMMUNITY.AREA.NAME"))
			}
			else if(input$M2 == "January"){
				check <- "KWH.JANUARY.2010"
				heatmap <- subset(bindmap, select = c("KWH.JANUARY.2010","COMMUNITY.AREA.NAME"))
			}
			else if(input$M2 == "February"){
				check <- "KWH.FEBRUARY.2010"
				heatmap <- subset(bindmap, select = c("KWH.FEBRUARY.2010","COMMUNITY.AREA.NAME"))
			}
			else if(input$M2 == "March"){
				check <- "KWH.MARCH.2010"
				heatmap <- subset(bindmap, select = c("KWH.MARCH.2010","COMMUNITY.AREA.NAME"))
			}
			else if(input$M2 == "April"){
				check <- "KWH.APRIL.2010"
				heatmap <- subset(bindmap, select = c("KWH.APRIL.2010","COMMUNITY.AREA.NAME"))
			}
			else if(input$M2 == "May"){
				check <- "KWH.MAY.2010"
				heatmap <- subset(bindmap, select = c("KWH.MAY.2010","COMMUNITY.AREA.NAME"))
			}
			else if(input$M2 == "June"){
				check <- "KWH.JUNE.2010"
				heatmap <- subset(bindmap, select = c("KWH.JUNE.2010","COMMUNITY.AREA.NAME"))
			}
			else if(input$M2 == "July"){
				check <- "KWH.JULY.2010"
				heatmap <- subset(bindmap, select = c("KWH.JULY.2010","COMMUNITY.AREA.NAME"))
			}
			else if(input$M2 == "August"){
				check <- "KWH.AUGUST.2010"
				heatmap <- subset(bindmap, select = c("KWH.AUGUST.2010","COMMUNITY.AREA.NAME"))
			}
			else if(input$M2 == "September"){
				check <- "KWH.SEPTEMBER.2010"
				heatmap <- subset(bindmap, select = c("KWH.SEPTEMBER.2010","COMMUNITY.AREA.NAME"))
			}
			else if(input$M2 == "October"){
				check <- "KWH.OCTOBER.2010"
				heatmap <- subset(bindmap, select = c("KWH.OCTOBER.2010","COMMUNITY.AREA.NAME"))
			}
			else if(input$M2 == "November"){
				check <- "KWH.NOVEMBER.2010"
				heatmap <- subset(bindmap, select = c("KWH.NOVEMBER.2010","COMMUNITY.AREA.NAME"))
			}
			else if(input$M2 == "December"){
				check <- "KWH.DECEMBER.2010"
				heatmap <- subset(bindmap, select = c("KWH.DECEMBER.2010","COMMUNITY.AREA.NAME"))
			}
		}

		if(C == "G"){
			

			if(input$M2 == "Total"){
				check <- "TOTAL.THERMS"
				heatmap <- subset(bindmap, select = c("TOTAL.THERMS","COMMUNITY.AREA.NAME"))
			}
			else if(input$M2 == "January"){
				check <- "THERM.JANUARY.2010"
				heatmap <- subset(bindmap, select = c("THERM.JANUARY.2010","COMMUNITY.AREA.NAME"))
			}
			else if(input$M2 == "February"){
				check <- "THERM.FEBRUARY.2010"
				heatmap <- subset(bindmap, select = c("THERM.FEBRUARY.2010","COMMUNITY.AREA.NAME"))
			}
			else if(input$M2 == "March"){
				check <- "THERM.MARCH.2010"
				heatmap <- subset(bindmap, select = c("THERM.MARCH.2010","COMMUNITY.AREA.NAME"))
			}
			else if(input$M2 == "April"){
				check <- "TERM.APRIL.2010"
				heatmap <- subset(bindmap, select = c("TERM.APRIL.2010","COMMUNITY.AREA.NAME"))
			}
			else if(input$M2 == "May"){
				check <- "THERM.MAY.2010"
				heatmap <- subset(bindmap, select = c("THERM.MAY.2010","COMMUNITY.AREA.NAME"))
			}
			else if(input$M2 == "June"){
				check <- "THERM.JUNE.2010"
				heatmap <- subset(bindmap, select = c("THERM.JUNE.2010","COMMUNITY.AREA.NAME"))
			}
			else if(input$M2 == "July"){
				check <- "THERM.JULY.2010"
				heatmap <- subset(bindmap, select = c("THERM.JULY.2010","COMMUNITY.AREA.NAME"))
			}
			else if(input$M2 == "August"){
				check <- "THERM.AUGUST.2010"
				heatmap <- subset(bindmap, select = c("THERM.AUGUST.2010","COMMUNITY.AREA.NAME"))
			}
			else if(input$M2 == "September"){
				check <- "THERM.SEPTEMBER.2010"
				heatmap <- subset(bindmap, select = c("THERM.SEPTEMBER.2010","COMMUNITY.AREA.NAME"))
			}
			else if(input$M2 == "October"){
				check <- "THERM.OCTOBER.2010"
				heatmap <- subset(bindmap, select = c("THERM.OCTOBER.2010","COMMUNITY.AREA.NAME"))
			}
			else if(input$M2 == "November"){
				check <- "THERM.NOVEMBER.2010"
				heatmap <- subset(bindmap, select = c("THERM.NOVEMBER.2010","COMMUNITY.AREA.NAME"))
			}
			else if(input$M2 == "December"){
				check <- "THERM.DECEMBER.2010"
				heatmap <- subset(bindmap, select = c("THERM.DECEMBER.2010","COMMUNITY.AREA.NAME"))
			}
		}

		if(C == "HBT"){
			heatmap <- subset(bindmap, select = c("BUILDING.TYPE","COMMUNITY.AREA.NAME"))
			if(input$T2 == "All"){
				check <- "BUILDING.TYPE"
			}
			else if(input$T2 == "Commercial"){
				check <- "BUILDING.TYPE"
				heatmap <- subset(heatmap, heatmap$BUILDING.TYPE %in% "Commercial")
			}
			else if(input$T2 == "Residential"){
				check <- "BUILDING.TYPE"
				heatmap <- subset(heatmap, heatmap$BUILDING.TYPE %in% "Residential")
			}
			else if(input$T2 == "Industrial"){
				check <- "BUILDING.TYPE"
				heatmap <- subset(heatmap, heatmap$BUILDING.TYPE %in% "Industrial")
			}
		}

		if(C == "HBA"){
			
			check <- "AVERAGE.BUILDING.AGE"
			heatmap <- subset(bindmap, select = c("AVERAGE.BUILDING.AGE","COMMUNITY.AREA.NAME"))
		}

		if(C == "HBH"){
			

			check <- "AVERAGE.HOUSESIZE"
			heatmap <- subset(bindmap, select = c("AVERAGE.HOUSESIZE","COMMUNITY.AREA.NAME"))
		}

		if(C == "HTP"){
			

			check <- "TOTAL.POPULATION"
			heatmap <- subset(bindmap, select = c("TOTAL.POPULATION","COMMUNITY.AREA.NAME"))
		}

		COMMUNITY.AREA.NAME <- c("All Chicago")
		INTPTLON10 <- c(-87.623177)
		INTPTLAT10 <- c(41.881832)
		position <- data.frame(COMMUNITY.AREA.NAME,INTPTLON10,INTPTLAT10)

		if(input$C2 != "All Chicago"){
			position <- subset(dfbindmap, dfbindmap$COMMUNITY.AREA.NAME %in% input$C2)
			position <- position[(NROW(position)/2),]
			heatmap <- subset(heatmap, heatmap$COMMUNITY.AREA.NAME %in% input$C2)
		}
		
		
		if(check != "BUILDING.TYPE"){
			if(input$L2 == "YlOrRd"){
				if(length(heatmap$COMMUNITY.AREA.NAME) != 0){
					m1 <- mapview(heatmap, zcol = check,col.regions = brewer.pal(11, input$L2))
					output$cmp1 <- renderLeaflet({
						m1@map%>% setView(position$INTPTLON10, position$INTPTLAT10,zoom = 13)
					})
				}
				else{
					m1 <- mapview()
					output$cmp1 <- renderLeaflet({
						m1@map%>% setView(position$INTPTLON10, position$INTPTLAT10,zoom = 14)
					})
				}
			}
			else{
				if(length(heatmap$COMMUNITY.AREA.NAME) != 0){
					m1 <- mapview(heatmap, zcol = check,col.regions = rev(brewer.pal(11, input$L2)))
					output$cmp1 <- renderLeaflet({
						m1@map%>% setView(position$INTPTLON10, position$INTPTLAT10,zoom = 13)
					})
				}
				else{
					m1 <- mapview()
					output$cmp1 <- renderLeaflet({
						m1@map%>% setView(position$INTPTLON10, position$INTPTLAT10,zoom = 14)
					})
				}
			}
		}
		else{
			if(input$L2 == "YlOrRd"){
				if(length(heatmap$COMMUNITY.AREA.NAME) != 0){
					m1 <- mapview(heatmap, zcol = check,col.regions = brewer.pal(3, input$L2))
					output$cmp1 <- renderLeaflet({
						m1@map%>% setView(position$INTPTLON10, position$INTPTLAT10,zoom = 13)
					})
				}
				else{
					m1 <- mapview()
					output$cmp1 <- renderLeaflet({
						m1@map%>% setView(position$INTPTLON10, position$INTPTLAT10,zoom = 14)
					})
				}
			}
			else{
				if(length(heatmap$COMMUNITY.AREA.NAME) != 0){
					m1 <- mapview(heatmap, zcol = check,col.regions = rev(brewer.pal(3, input$L2)))
					output$cmp1 <- renderLeaflet({
						m1@map%>% setView(position$INTPTLON10, position$INTPTLAT10,zoom = 13)
					})
				}
				else{
					m1 <- mapview()
					output$cmp1 <- renderLeaflet({
						m1@map%>% setView(position$INTPTLON10, position$INTPTLAT10,zoom = 14)
					})
				}
			}
		}
		
		

		
	})

	observe({

		C <- c(0)

		

		if(input$data3 == "Electricity"){
			
			C <- "E"
		}

		if(input$data3 == "Gas"){
			
			C <- "G"
		}

		if(input$data3 == "Building Type"){
			
			C <- "HBT"
		}

		if(input$data3 == "Building Age"){
			
			C <- "HBA"
		}

		if(input$data3 == "Building Height"){
			
			C <- "HBH"
		}

		if(input$data3 == "Total Population"){
			
			C <- "HTP"
		}

		check <- c(0)

		if(C == "E"){
			if(input$M3 == "Total"){
				check <- "TOTAL.KWH"
				heatmap3 <- subset(bindmap, select = c("TOTAL.KWH","COMMUNITY.AREA.NAME"))
			}
			else if(input$M3 == "January"){
				check <- "KWH.JANUARY.2010"
				heatmap3 <- subset(bindmap, select = c("KWH.JANUARY.2010","COMMUNITY.AREA.NAME"))
			}
			else if(input$M3 == "February"){
				check <- "KWH.FEBRUARY.2010"
				heatmap3 <- subset(bindmap, select = c("KWH.FEBRUARY.2010","COMMUNITY.AREA.NAME"))
			}
			else if(input$M3 == "March"){
				check <- "KWH.MARCH.2010"
				heatmap3 <- subset(bindmap, select = c("KWH.MARCH.2010","COMMUNITY.AREA.NAME"))
			}
			else if(input$M3 == "April"){
				check <- "KWH.APRIL.2010"
				heatmap3 <- subset(bindmap, select = c("KWH.APRIL.2010","COMMUNITY.AREA.NAME"))
			}
			else if(input$M3 == "May"){
				check <- "KWH.MAY.2010"
				heatmap3 <- subset(bindmap, select = c("KWH.MAY.2010","COMMUNITY.AREA.NAME"))
			}
			else if(input$M3 == "June"){
				check <- "KWH.JUNE.2010"
				heatmap3 <- subset(bindmap, select = c("KWH.JUNE.2010","COMMUNITY.AREA.NAME"))
			}
			else if(input$M3 == "July"){
				check <- "KWH.JULY.2010"
				heatmap3 <- subset(bindmap, select = c("KWH.JULY.2010","COMMUNITY.AREA.NAME"))
			}
			else if(input$M3 == "August"){
				check <- "KWH.AUGUST.2010"
				heatmap3 <- subset(bindmap, select = c("KWH.AUGUST.2010","COMMUNITY.AREA.NAME"))
			}
			else if(input$M3 == "September"){
				check <- "KWH.SEPTEMBER.2010"
				heatmap3 <- subset(bindmap, select = c("KWH.SEPTEMBER.2010","COMMUNITY.AREA.NAME"))
			}
			else if(input$M3 == "October"){
				check <- "KWH.OCTOBER.2010"
				heatmap3 <- subset(bindmap, select = c("KWH.OCTOBER.2010","COMMUNITY.AREA.NAME"))
			}
			else if(input$M3 == "November"){
				check <- "KWH.NOVEMBER.2010"
				heatmap3 <- subset(bindmap, select = c("KWH.NOVEMBER.2010","COMMUNITY.AREA.NAME"))
			}
			else if(input$M3 == "December"){
				check <- "KWH.DECEMBER.2010"
				heatmap3 <- subset(bindmap, select = c("KWH.DECEMBER.2010","COMMUNITY.AREA.NAME"))
			}
		}

		if(C == "G"){
			

			if(input$M3 == "Total"){
				check <- "TOTAL.THERMS"
				heatmap3 <- subset(bindmap, select = c("TOTAL.THERMS","COMMUNITY.AREA.NAME"))
			}
			else if(input$M3 == "January"){
				check <- "THERM.JANUARY.2010"
				heatmap3 <- subset(bindmap, select = c("THERM.JANUARY.2010","COMMUNITY.AREA.NAME"))
			}
			else if(input$M3 == "February"){
				check <- "THERM.FEBRUARY.2010"
				heatmap3 <- subset(bindmap, select = c("THERM.FEBRUARY.2010","COMMUNITY.AREA.NAME"))
			}
			else if(input$M3 == "March"){
				check <- "THERM.MARCH.2010"
				heatmap3 <- subset(bindmap, select = c("THERM.MARCH.2010","COMMUNITY.AREA.NAME"))
			}
			else if(input$M3 == "April"){
				check <- "TERM.APRIL.2010"
				heatmap3 <- subset(bindmap, select = c("TERM.APRIL.2010","COMMUNITY.AREA.NAME"))
			}
			else if(input$M3 == "May"){
				check <- "THERM.MAY.2010"
				heatmap3 <- subset(bindmap, select = c("THERM.MAY.2010","COMMUNITY.AREA.NAME"))
			}
			else if(input$M3 == "June"){
				check <- "THERM.JUNE.2010"
				heatmap3 <- subset(bindmap, select = c("THERM.JUNE.2010","COMMUNITY.AREA.NAME"))
			}
			else if(input$M3 == "July"){
				check <- "THERM.JULY.2010"
				heatmap3 <- subset(bindmap, select = c("THERM.JULY.2010","COMMUNITY.AREA.NAME"))
			}
			else if(input$M3 == "August"){
				check <- "THERM.AUGUST.2010"
				heatmap3 <- subset(bindmap, select = c("THERM.AUGUST.2010","COMMUNITY.AREA.NAME"))
			}
			else if(input$M3 == "September"){
				check <- "THERM.SEPTEMBER.2010"
				heatmap3 <- subset(bindmap, select = c("THERM.SEPTEMBER.2010","COMMUNITY.AREA.NAME"))
			}
			else if(input$M3 == "October"){
				check <- "THERM.OCTOBER.2010"
				heatmap3 <- subset(bindmap, select = c("THERM.OCTOBER.2010","COMMUNITY.AREA.NAME"))
			}
			else if(input$M3 == "November"){
				check <- "THERM.NOVEMBER.2010"
				heatmap3 <- subset(bindmap, select = c("THERM.NOVEMBER.2010","COMMUNITY.AREA.NAME"))
			}
			else if(input$M3 == "December"){
				check <- "THERM.DECEMBER.2010"
				heatmap3 <- subset(bindmap, select = c("THERM.DECEMBER.2010","COMMUNITY.AREA.NAME"))
			}
		}

		if(C == "HBT"){
			heatmap3 <- subset(bindmap, select = c("BUILDING.TYPE","COMMUNITY.AREA.NAME"))
			if(input$T3 == "All"){
				check <- "BUILDING.TYPE"
			}
			else if(input$T3 == "Commercial"){
				check <- "BUILDING.TYPE"
				heatmap3 <- subset(heatmap3, heatmap3$BUILDING.TYPE %in% "Commercial")
			}
			else if(input$T3 == "Residential"){
				check <- "BUILDING.TYPE"
				heatmap3 <- subset(heatmap3, heatmap3$BUILDING.TYPE %in% "Residential")
			}
			else if(input$T3 == "Industrial"){
				check <- "BUILDING.TYPE"
				heatmap3 <- subset(heatmap3, heatmap3$BUILDING.TYPE %in% "Industrial")
			}
		}

		if(C == "HBA"){
			
			check <- "AVERAGE.BUILDING.AGE"
			heatmap3 <- subset(bindmap, select = c("AVERAGE.BUILDING.AGE","COMMUNITY.AREA.NAME"))
		}

		if(C == "HBH"){
			

			check <- "AVERAGE.HOUSESIZE"
			heatmap3 <- subset(bindmap, select = c("AVERAGE.HOUSESIZE","COMMUNITY.AREA.NAME"))
		}

		if(C == "HTP"){
			

			check <- "TOTAL.POPULATION"
			heatmap3 <- subset(bindmap, select = c("TOTAL.POPULATION","COMMUNITY.AREA.NAME"))
		}

		COMMUNITY.AREA.NAME <- c("All Chicago")
		INTPTLON10 <- c(-87.623177)
		INTPTLAT10 <- c(41.881832)
		position <- data.frame(COMMUNITY.AREA.NAME,INTPTLON10,INTPTLAT10)

		if(input$C3 != "All Chicago"){
			position <- subset(dfbindmap, dfbindmap$COMMUNITY.AREA.NAME %in% input$C3)
			position <- position[(NROW(position)/2),]
			heatmap3 <- subset(heatmap3, heatmap3$COMMUNITY.AREA.NAME %in% input$C3)
		}

		
		if(check != "BUILDING.TYPE"){
			if(input$L3 == "YlOrRd"){
				if(length(heatmap3$COMMUNITY.AREA.NAME) != 0){
					m2 <- mapview(heatmap3, zcol = check,col.regions = brewer.pal(11, input$L3))
					output$cmp2 <- renderLeaflet({
						m2@map%>% setView(position$INTPTLON10, position$INTPTLAT10,zoom = 13)
					})
				}
				else{
					m2 <- mapview()
					output$cmp2 <- renderLeaflet({
						m2@map%>% setView(position$INTPTLON10, position$INTPTLAT10,zoom = 14)
					})
				}
			}
			else{
				if(length(heatmap3$COMMUNITY.AREA.NAME) != 0){
					m2 <- mapview(heatmap3, zcol = check,col.regions = rev(brewer.pal(11, input$L3)))
					output$cmp2 <- renderLeaflet({
						m2@map%>% setView(position$INTPTLON10, position$INTPTLAT10,zoom = 13)
					})
				}
				else{
					m2 <- mapview()
					output$cmp2 <- renderLeaflet({
						m2@map%>% setView(position$INTPTLON10, position$INTPTLAT10,zoom = 14)
					})
				}
			}
		}
		else{
			if(input$L3 == "YlOrRd"){
				if(length(heatmap3$COMMUNITY.AREA.NAME) != 0){
					m2 <- mapview(heatmap3, zcol = check,col.regions = brewer.pal(3, input$L3))
					output$cmp2 <- renderLeaflet({
						m2@map%>% setView(position$INTPTLON10, position$INTPTLAT10,zoom = 13)
					})
				}
				else{
					m2 <- mapview()
					output$cmp2 <- renderLeaflet({
						m2@map%>% setView(position$INTPTLON10, position$INTPTLAT10,zoom = 14)
					})
				}
			}
			else{
				if(length(heatmap3$COMMUNITY.AREA.NAME) != 0){
					m2 <- mapview(heatmap3, zcol = check,col.regions = rev(brewer.pal(3, input$L3)))
					output$cmp2 <- renderLeaflet({
						m2@map%>% setView(position$INTPTLON10, position$INTPTLAT10,zoom = 13)
					})
				}
				else{
					m2 <- mapview()
					output$cmp2 <- renderLeaflet({
						m2@map%>% setView(position$INTPTLON10, position$INTPTLAT10,zoom = 14)
					})
				}
			}
		}
		
		

		
	})



	
		

	

	output$text1 <- renderText({
		"Data source: https://www.kaggle.com/chicago/chicago-energy-usage-2010" 
	})

	output$text2 <- renderText({
		"File Name:energy-usage-2010.csv" 
	})

	output$text3 <- renderText({
		"Changed: Copy the source file and change the CENSUS BLOCK to GEOID10. Deleted redundant data in the source data, such as power or thermo SQFT data, STD, mean and other 35 columns. However, due to the poor effect of reducing the memory footprint, it is no different from directly using the source data." 
	})

	output$text4 <- renderText({
		"Author: Jiajun Mo " 
	})

	output$text5 <- renderText({
		"Date: 2021/4/22" 
	})

	

	
	

 
 }

 shinyApp(ui, server)