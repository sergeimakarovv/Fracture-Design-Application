# `Fracture-Design` Shiny Application

## **Description:**

The **Fracture-Design** App helps to calculate the optimal fracture parameters, the actual proppant placement and the treatment mode of hydraulic fracturing operation.
It is developed for oil and gas professionals and analysts working with hydraulic fracturing. The app is available in **English** and **Russian**. 

[![Button-8.png](https://i.postimg.cc/XvwTVW8C/Button-8.png)](https://sergei-makarov.shinyapps.io/Fracture-Design/)

[![4.png](https://i.postimg.cc/t7Gpd164/4.png)](https://postimg.cc/ZvHG45jG)

A user is required to input parameters of the oil and gas reservoir, fracturing fluid and proppant. The app creates data tables containing information about the fracture design and treatment mode. 
The report with calculation results is available to download in Excel and Word formats. 

Full methodology and logic of fracture design optimization are described in attached `PDF files` in English and Russian.

## **Project overview**

The application consists of 2 main files: `ui.R` and `server.R`.

The Word output file is created from RMarkdown files: `Report_en.Rmd`, `Report_ru.Rmd`. 

In order to run the application, ensure to download the necessary packages as in `requirements.txt` and set the working directory to local. 
