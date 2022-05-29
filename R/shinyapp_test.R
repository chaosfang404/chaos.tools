cvd_ui <- function(cvd_data)
{
	cvd_server <-	function(
						input,
						output
	){
		output$cvd_caption <-	renderText(
								{
									paste0("Chromosome ", input$chr)
								})
	
		output$cvd_plot <-	renderPlot(
							{
								plot_cvd(
									cvd_data,
									chr = input$chr,
									min = input$interaction_distance[1],
									max = input$interaction_distance[2],
									slop_position = c(input$slop_position_x,input$slop_position_y),
									slop_length = input$slop_length
	
								)
							})
		output$cvd_table <- renderDataTable({
									cvd_data[chr == input$chr]
							})
	}

	cvd_ui <-	fluidPage(
					titlePanel("Decaycurve"),
					sidebarLayout(
						sidebarPanel(					
							selectInput(
								inputId = "chr",
								label = "chr",
								unique(cvd_data$chr)
							),
							sliderInput(
								"interaction_distance",
								"interaction distance",
								value = c(1e4,1e7),
								min = 1e4,
								max = 1e8,
								animate = T
							),
							sliderInput(
								"slop_position_x",
								"slop position x",
								value = 4,
								min = 4,
								max = 7,
								step = 0.1,
								animate = T
							),
							sliderInput(
								"slop_position_y",
								"slop position y",
								value = 6.8,
								min = 0,
								max = 10,
								step = 0.1,
								animate = T
							),
							sliderInput(
								"slop_length",
								"slop length",
								value = 2,
								min = 0,
								max = 5,
								step = 0.1,
								animate = T
							)
						),
						mainPanel(
							h3(textOutput("cvd_caption")),
							plotOutput("cvd_plot"),
							h3("Data"),
							dataTableOutput("cvd_table")
						)
					)
				)
	shinyApp(cvd_ui,cvd_server)
}