-------------------------------------------------------------------------------------------------------------------------------------------------------
# ManCity-Hays and CodeCo Hackathon
Our team has worked on an Expected Points model and Evidential Reasoning Model for women's premier league data provided by Statsbomb and Second Spectrum.

Aim: To identify top players by devising new scoring methods to score the individual performances. 
Data Source: Statsbomb and SecondSpectrum

#################################################################

About Expected Points Model:

With our expected points visualizations, we intended on giving coaches a better understanding of their teams performances during a particular game, as the final score is not always representative of a team's performance during a game. 

Clustering is a powerful technique and using it to show passing clusters is an effective way of assisting coaches in determining which passes on the pitch are most effective in producing the most dangerous shots. 

The webappdata foler and webapp data has all the folders for the aforementioned model

#################################################################

About Evidential Reasoning Model: 

The Evidential Reasoning Model is based on the Dempster-Shafer theory for modelling uncertainities and possibilities.
Using this model we have made an attempt to use a Multi Criteria Decision Analysis model for scoring and prescribing best defensive measures based on defensive attributes (metrics) based on our domain knowledge. We have made use of the Intelligent Decision System (IDS) software designed by Prof. Jian Bo-Yang of the University of Manchester. 

To know more about this theory and its applications we attach here an academic paper by the professor himself:

https://personalpages.manchester.ac.uk/staff/jian-bo.yang/JB%20Yang%20Journal_Papers/RuleUtility.pdf

Refer to the following files in the repository (under the Defensive Rankings- ER Model folder) for this bit of the project:

IDS Downlaod- StudentVersion ZipFile (Setup)

Manity_Arsenal_evetsdata.csv (Statsbomb Events data)

Defensive Metrics- Alteryx Workflow file to build and consolidate data

ManCity_Arsenal_Defense_FullMatchStats- csv (Data cleaned from Statsbom events file)

ManCity Players Defensive Scores- IDS File

ManCity Players Defensive Results- Text File

Arsenal Players Defensive Scores- IDS File

Arsenal Players Defensive Results - Text File

################### Webapp details

To access the final output we have designed a webapp on R to be accessed by managers:

Webapp link: https://raheelsq.shinyapps.io/Hackathon/

To run the web app, download all the data within the "webappdata" folder and use them to run "app.R". 

The webapp has 3 tabs (on the left side) namely- tactics, passing clusters, and defensive ranks.

Toogle the tabs to know more bout the performance statistics.

-------------------------------------------------------------------------------------------------------------------------------------------------------



