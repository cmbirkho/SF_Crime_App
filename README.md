#### **Overview**
#### This repository documents the analysis and code used to host an app aimed at helping SF residents gain more insight into criminal activity within their neighborhoods and explore some concepts of data science.

#### **What does the application do?**
- San Francisco crime data is downloaded and Extract, Transform, and Load operations are performed and the data is stored in a SQLite database.
- The user can interact with the data in several ways by exploring each of the following tabs on the UI:
    - Overview
    - Inferential Stats
    - Machine Learning
    - Data Download
    - About


#### **Where does the data come from?**
- Police Department Incident Reports: 2018 to Present. For more information go [**here**](https://data.sfgov.org/Public-Safety/Police-Department-Incident-Reports-2018-to-Present/wg3w-h783)

#### **Development Process**
1. Find a dataset that's interesting!
2. Lay out an outline of the UI and what the application should do on paper. This outline will be iterated on throughout the development process.
3. Perform Exploratory Data Analysis on the SF incident reports data
4. Lay out how the data should flow from source to application