### This is an R script to teach Multidimensional Scaling concepts, using U.S. cities as example data.

### Begin by opening R Studio and setting the working directory.

###	Navigate to your working folder and use the "More" button under the "Files" tab in the lower-right window.

### Next, install and load the R packages 'vegan', 'ggplot2', and 'ggrepel'.

    install.packages("vegan")
	
	install.packages("ggplot2")
	
	install.packages("ggrepel")

    library(vegan)
	
	library(ggplot2)
	library(tidyverse)
	library(ggrepel)


	#theme function
	anh_theme <- function() {
	  theme(
	    axis.text.x = element_text(colour = "black", face = "bold", size = 14, hjust = 1),
	    axis.text.y = element_text(colour = "black", size = 14, face = "bold"),
	    axis.title.x = element_text(face = "bold", size = 15, colour = "black", margin = margin(t = 5)),
	    axis.title.y = element_text(face = "bold", size = 15, colour = "black", 
	                                angle = 90, vjust = 0.5, margin = margin(r = 10)),
	    axis.title.y.right = element_text(size = 15, angle = 270, vjust = 0.5),
	    legend.text = element_text(size = 12, face = "bold", colour = "black"),
	    legend.title = element_text(size = 14, colour = "black", face = "bold"),
	    legend.position = "right",
	    legend.key = element_blank(),
	    panel.background = element_blank(),
	    panel.border = element_rect(colour = "black", fill = NA, size = 1.2),
	    plot.title = element_text(color = "black", size = 18, face = "bold", hjust = 0.5),
	    strip.text = element_text(size = 14)
	  )
	}
	
# Import the inter-city distance matrix named "CityDistRawKM.csv".

    CityDistRaw <- read.csv("./MultidimensionalScaling/CityDistRawKM.csv", row.names = 1, header = T)
glimpse(CityDistRaw)

# Convert the imported file to a R matrix (a matrix is different than the standard 'dataframe' table in R).

    DistMat <- as.matrix(CityDistRaw)


# Run METRIC Multidimensional Scaling with 2 dimensions (Vegan function 'wcmdscale').

    CityMDS <- wcmdscale(DistMat, k = 2)	# The 'k' setting specifies the number of dimensions to use.


# Save the MDS results as a data frame.

	CityMDS <- as.data.frame(CityMDS)


# Plot the 2-dimensional ordination.

    NMDSplot <- ggplot(CityMDS, aes(x = V1, y = V2)) + geom_point(color = 'blue', size = 3) + 
	
					   xlim(-3000, 3000) + ylim(-3000, 3000) + anh_theme()

    NMDSplot

# Add a new 'Site'column to [CityMDS] for use in plotting.

	CityMDS$Site <- row.names(CityMDS)


# Rebuild the ordination scatterplot with offset labels.

	NMDSplot <- NMDSplot + geom_label_repel(aes(label = Site), size = 2,
  
										    box.padding = 0.5, segment.color = 'grey50')

	NMDSplot


# Export the ordination scatterplot.

	ggsave("./MultidimensionalScaling/UScities_NMDSplot.png", width = 7, height = 7,dpi=300, units = "in")



### End of script.	