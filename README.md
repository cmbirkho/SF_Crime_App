#### This repository documents the analysis and code used to host an app aimed at helping SF residents gain more insight into criminal activity within their neighborhoods.


#### The application does the following:
- San Francisco crime data is downloaded from [**here**](https://data.sfgov.org/Public-Safety/Police-Department-Incident-Reports-2018-to-Present/wg3w-h783) and then Extract, Transform, and Load operations are performed and the data is loaded and ready to be accessed via the User Interface. 
- The user can interact with the data in several ways by exploring each of the following tabs:
    - EDA
    - Inferential Stats
    - Predictions
    - Data Explorer
    - Your Data
- As the user interacts with the data those interactions/results are saved into a SQLite database. The user can easily access and see the data they generated and what was collected by opening the "Your Data" tab.
