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

rna_ui <- function(.data)
{
	rna_server <-	function(
						input,
						output
	){
		output$rna_caption <-	renderText(
								{
									"Volcano plot"
								})
	
		output$rna_plot <-	renderPlot(
							{
								volcano_plot(
									.data,
									top_gene_number = input$top_gene_number,
									fold_change = input$fold_change,
									p_value = input$p_value
								)
							})
		output$rna_table <- renderDataTable({
									.data
							})
	}

	rna_ui <-	fluidPage(
					titlePanel("Transcriptome"),
					sidebarLayout(
						sidebarPanel(					
							sliderInput(
								"top_gene_number",
								"Top gene number",
								value = 10,
								min = 0,
								max = 100,
								step = 1,
								animate = T
							),
							sliderInput(
								"fold_change",
								"fold change",
								value = 1.5,
								min = 0,
								max = 5,
								step = 0.1,
								animate = T
							),
							sliderInput(
								"p_value",
								"P value",
								value = 0.05,
								min = 0.001,
								max = 0.5,
								step = 0.001,
								animate = T
							)
						),
						mainPanel(
							h3(textOutput("rna_caption")),
							plotOutput("rna_plot"),
							h3("Data"),
							dataTableOutput("rna_table")
						)
					)
				)
	shinyApp(rna_ui,rna_server)
}
