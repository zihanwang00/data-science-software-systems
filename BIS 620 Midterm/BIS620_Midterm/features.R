# Team Members: Zihan Wang, Kelly Wang, Sara Shen.

# Project Purpose: 
# The objective is to develop a user-friendly search tool that empowers users to explore and analyze clinical trials effortlessly. 
# To enhance the user experience, visual plots like global heat maps and pie charts are integrated, providing intuitively visual insights into complex data. 
# The emphasis is on offering viewers the flexibility to create their own judgments and analyses. 
# Users can choose filters and selections tailored to their preferences, allowing for personalized exploration.
# By segmenting high-level data into distinct categories, 
# the application enables users to allocate their resources effectively and access specific information relevant to their research or decision-making needs.
# This flexible approach of varying breadth of scope promotes open-ended research of the disease conditions and interventions, 
# encouraging users to draw their own conclusions, identify patterns, and generate hypotheses based on the data presented.
# The user-driven design ensures that the tool caters to a wide range of users, 
# from medical professionals and researchers to patients, offering a comprehensive and informative resource for clinical trial analysis.



# Feature 1: Create a map showing the number of trials in each ountry by a brief keyword search
#' @title World Map of Trials
#' @description This feature prints out a world map so that the viewer may have an overview of the number of trials across different countries. 

#' @motivation It will be important for the viewer to see the amount of trials distributed across countries.
# It also allows the viewer to earn a breadth of information based on a cursory glance and the visual aspect may also be more appealing 
# than a table for viewers- another reason why it’s one of the first mid foremost tabs to be displayed. 

#' @implementation In the shiny app, we created a section of our app that is designated to create an overarching picture of the weights of 
# the number of ids and participants distributed across countries on a world map. We used a standard atlas and proceeded to superimpose 
# the colors onto the countries based on the number of ID’s. We also added an additional feature #5 that will supplement the information here as well.
#' 
#' @param the studies to get the number of studies trials for.




# Feature 2: Create a condition keyword search to filter condition plot, update on condition plot
#' @title Condition Keyword Search
#' @description This feature is accessible through the buttons labeled “Condition Names”. 
# The user search allows us to see the brief titles or types of trials that they are interested in. 
# From there on, one can do visualizations or analyses based on those types of trials. 

#' @motivation When writing Shiny Apps, we want to reframe our thinking to the user experience. 
# A keyword search seems intuitive and allows the viewer to narrow in on specific interests they want to research more on. 

#' @implementation In the Shiny app, we designed a search button for different conditions. 
# It allows users to see the conditions related to certain keywords, and gives us the bar plots regarding different conditions. 
#' 
#' @param input$condition_kw




# Feature 3: Create Bar Charts based on different Intervention.
#' @title Intervention and Condition Mapping
#' @description This feature is accessible through the buttons labeled “Choose an Intervention” and “Condition Bar Plot”. 
# This provides users with the ability to select an intervention type and see a list of conditions that are 
# associated with studies involving that type of intervention.

#' @motivation We wanted to implement the condition mapping to the intervention so that users can familiarize themselves 
# with the type of conditions that are often associated with interventions. This can be informative to know to the extent 
# how flexible the interventions are and to what scope that can reach. By visualizing the conditions associated with interventions, 
# users can better comprehend the context and potential applications of these interventions. This can aid in decision-making and 
# research as users explore and analyze the connections between interventions and specific medical conditions.

#' @implementation By designing this feature, users can search for specific intervention types using the “Choose an Intervention” button. 
# The results in the “Condition Bar Plot” part will give the top 10 conditions for that specific intervention. For example, if we 
# select “Diagnostic Test” as the intervention type, the bar plot in the “Condition Bar Plot” button will reveal that conditions COVID-19,
# Breast Cancer, Coronary Artery Disease, Prostate Cancer, Colorectal Cancer, Heart Failure, Sepsis, Stroke, and Cardiovascular Disease 
# are the most frequently associated with the use of diagnostic tests, along with their respective counts. This is then run through the 
# function: get_conditions_for_intervention_type(interventionType) 
#' 
#' @param input$interventionType




# Feature 4: Pie Charts of Outcome Types based on different Interventions.
#' @title Pie Chart for outcomes
#' @description In our R shiny app, we have a new tab named, Condition Pie Chart, where it displays the top 10 conditions based on 
# the intervention chosen. For example in the situation where “drug” is chosen as the intervention, we see that most results are “health” 
# and then in descending order, the types of outcomes that can usually follow. 

#' @motivation This feature allows us to share a proportion of the types of outcomes based on a given intervention. This is important 
# to the user and good to have as a feature on the app because pie charts generally provide a quick and easy-to-understand overview of 
# the distributions at a quick glance. Users can immediately see the relative proportions of different outcome types associated with various 
# interventions. Additionally, pie charts allow for an intuitive visual comparison of outcome types amongst different interventions. 

#' @implementation By designing this feature, users can access the outcomes by clicking the different interventions. Users can identify 
# which interventions have a higher or lower prevalence of specific outcomes, which can be valuable for research or decision-making. 
# Furthermore, users can begin to see emerging patterns and which interventions are more likely to lead to positive outcomes. This is 
# then run through the function get_outcome_pie_for_intervention <- function(interventionType).
#' 
#' @param input$interventionType




# Feature 5: Word Cloud
#' @title Word Cloud of Conditions
#' @description Within the tab, we generated a word cloud of varying sizes. We notice here that the biggest word is cancer with disease 
# following as second, which is understandable since a large subject at hand is about the disease, cancer, and the conditions as well as 
# interventions that may come along with it. The coloring also helps to categorize the weight of each of the words. We see that the green 
# words, which are smaller, are often referring to conditions, interventions, and parts of the body that appear less often, whereas the 
# red and blue words appear more often in the data. 

#' @motivation Including a tab of a word cloud can be incredibly beneficial for the user to sweep through from a cursory glance, the types 
# of terminology that may be in relevant and order of importance. It provides a holistic view of the data and notes the patterns of repeated 
# terms to the viewers at an instant. Word clouds offer a user-friendly visual summary of the most common conditions and interventions. By 
# displaying words with varying sizes, they effectively represent the relative frequency and identify trends amongst the data. Users can 
# instantly grasp which conditions or interventions are the most prevalent.

#' @implementation We create a new tab for the word cloud map feature. Within that feature, we created a function that does a count of their 
# varying degrees of frequency from the text corpus. From there, based on the frequency value, we dilate the size of the given word to normalize
# the word along with its frequency count. Afterwards, it is also color coded by the weights of frequencies as well, allowing a visual 
# categorization that distinguishes each of the words on the screen. This is all done through the function word_cloud(study, condition). 
#' 
#' @param study
#' @param condition




# Feature 6: ID count of individual countries
#' @title Add a dropdown for selecting a country
#' @description If we reference back to feature #1, we notice that within the world heat map, we can see a tool that allows us to select one of 
# the 223 presented countries that have information regarding the number of ID’s. After selecting a country, the viewer can see the following 
# information presented: Country : “Country name” | Number of ID’s #

#' @motivation Within the world map, an additional modification that can help is to see the specific numbers. Even though the world map serves 
# as a heat map to the varying levels of id count, from the viewer perspective, this may beg the question of “how many exactly” are there within 
# each country- a supplemental yet crucial piece of information. 

#' @implementation Within the code, we simplify and modify the portion of the original world map to include a selection bar of countries. 
# Initially, we planned to list out the ID’s in a tabled format along with the NCT ID’s, however that overwhelmed the SHINY app since many countries 
# had too many ID’s such as the US with 167,299 ID’s. It would not have been beneficial for the user to see the ID’s all clustered on top of the map 
# and would also make for a visually more confusing experience. Hence, we compose a dataframe of unique countries and their corresponding total counted 
# number of ID’s. Afterwards, the intent in mind is to let the viewer see the holistic picture of the map and based off that, make their judgment on 
# which country they’d like to see more information on. This is run through the function, count_country_id(country_df).

#' 
#' @param input$countries

