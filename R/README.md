

# Advanced R Shiny Interactive AE Plots
PHUSE Forest Plot Project Repository

### Overview of AE_LB Shiny Application:
    The purpose of this AE/LB Application is to visualize the AE (Adverse Event) and LB(Laboratory) dataset through Interactive R shiny Graphs (AE -Volcano and Forest plot/ LB – eDISH Plot). In this AE/LB Application, we can generate the plots which will be according to the filters that we select. Since we have many table of tables to visualize certain statistics and measures for AE and LB, we can simply use this app to display plots for all the combinations of treatments.

![image](https://user-images.githubusercontent.com/65352723/190404121-28d475cc-937a-49e4-aacc-754b1bb84cc9.png)

This is the look of the application initially when one loads it. The advantage of this R shiny application is reducing the duration of generating the Graphical representation and it is more informative. We can also download the plots from this application. 

### Data Import from Local and Report Selection:

![image](https://user-images.githubusercontent.com/65352723/190404434-2f878343-7d04-4d0f-9af6-ed7f237a537e.png)
![image](https://user-images.githubusercontent.com/65352723/190405037-b1207ada-e266-45ec-a06a-e128dfee2591.png)

### Data from Server:
Same applies for server data upload in which the user has to give the server path where the data is placed.

![image](https://user-images.githubusercontent.com/65352723/190405145-3bd351c8-8241-45e1-961d-e53f61e9c6ef.png)

### Volcano Inputs:

![image](https://user-images.githubusercontent.com/65352723/190430557-4f8e93f0-b4c9-42a4-8025-6dc39e99f6a4.png)

![image](https://user-images.githubusercontent.com/65352723/190405694-0fb47986-d5bd-4ba1-92e5-a36b7ac073b8.png)

![image](https://user-images.githubusercontent.com/65352723/190406199-27f7a242-b15f-4af9-a365-54223e272790.png)

### Volcano Visualization:
Click the **Create** button to get the Plots.

![image](https://user-images.githubusercontent.com/65352723/190406393-c471e685-ee33-45a7-abd5-6af9c9827236.png)

### Forest Inputs:

![image](https://user-images.githubusercontent.com/65352723/190430389-17667199-5972-4d19-bc4f-eb2930176a1e.png)

![image](https://user-images.githubusercontent.com/65352723/190406934-5b588dc4-b06c-435e-821b-f95d698c3999.png)

![image](https://user-images.githubusercontent.com/65352723/190429919-a2382431-2350-480b-8797-661770a6d183.png)

### Forest Visualization:
![image](https://user-images.githubusercontent.com/65352723/190425886-c98d00d6-6626-42d2-b453-fb2e64a62b8c.png)

### Listings and Download options:

For all AE/LB plots, hovering over a point will give you info about the subject, and by double-clicking a datapoint in the plot, you will see the corresponding listing just below the plot. Example listing from AE data:

![image](https://user-images.githubusercontent.com/65352723/190426848-5e06c730-5de6-4c08-8e58-4f85313d9b9e.png)

You can choose which columns to display in this listing from the drop down above it: 

![image](https://user-images.githubusercontent.com/65352723/190426190-3fd4e42e-71dc-4d7b-9f4a-8f03101e182c.png)

We can download the current plot in pdf, html or pptx format by the clicking the download button which is placed at the top right of the application.

![image](https://user-images.githubusercontent.com/65352723/190426240-1e37cb71-3912-42ad-bd2b-d3a517b5b5e9.png)

### Usage Notes and Common errors:
•	Check whether given subsets/filters are present in data
•	When the plot does not have any data input – i.e the subset given by user is not present in the data, this message will be displayed to alert the user:

![image](https://user-images.githubusercontent.com/65352723/183668648-521a206c-81a5-4150-a537-9b9faa53072e.png)

In this example, the data does not contain the Parameters mentioned in the SUBSET input.

### App devlopment software and packages details: 
###### R version 4.0.3 (2020-10-10)
officer : 0.4.1 htmltools : 0.5.2 epitools : 0.5.10.1 shinyWidgets : 0.6.4 survminer : 0.4.9 ggpubr : 0.4.0 survival : 3.2.7 forcats : 0.5.1 stringr : 1.4.0 dplyr : 1.0.7 purrr : 0.3.4 readr : 2.0.1 tidyr : 1.1.3 tibble : 3.1.4 tidyverse : 1.3.1 shinycssloaders : 1.0.0 scales : 1.1.1 plotly : 4.9.4.1 ggplot2 : 3.3.5 cowplot : 1.1.1 haven : 2.4.3 DT : 0.20 data.table : 1.14.0 shinyjs : 2.1.0 shiny : 1.7.0

#### AE LB Summaries Application

 knitr::include_app("https://rsc.pfizer.com/content/67a30ace-cf44-4f74-afa2-d147826d8426")

