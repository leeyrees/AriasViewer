# AriasViewer
In this repository, the code for the  [AriasViewer]( https://leyresastre.shinyapps.io/AriasViewer/) app can be found, as well as the simulated data files one can employ to try out the app. 

This app has been created as a tool for visualization of musical proportions. In order for the app to work, it is necessary the upload of three data files. This repository contains three simulated data files, so anyone can use the app: Arias_change_fake.xlsx, Arias_proportions_fake.xlsx, and Arias_scoring_fake.xlsx. The columns of these files contain specific information, as follows:
- Arias_change_fake.xlsx: ID, Opera, Aria, Year, Composer, Act, Scene, Act & Scene, City, Country, Character, Type (character type), Form, Key_A (tonal center for section A), Key_B (tonal center for section B), TSig_A (number of beats and the duration of such beats in each measure in A), TSig_B number of beats and the duration of such beats in each measure in B), Tempo_A (speed at which section A is played), and Tempo_B (speed at which section B is played). 
- Arias_proportions_fake.xlsx: ID, Opera, Aria, Year, Composer, Act, Scene, Act & Scene, City, Country, Character, Type (character type), Form, Totalcc (Total number of measures), Rit1 (number of measures in Rit1), a1 (number of measures in a1), Rit2 (number of measures in Rit2), a2 (number of measures in a2), Rit3 (number of measures in Rit3), B (number of measures in B), Rit4 (number of measures in Rit4), Rit1' (number of measures in Rit1'), a1' (number of measures in a1'), Rit2' (number of measures in Rit2'), a2' (number of measures in a2'), Rit3' (number of measures in Rit3'), and A' (number of measures in A'). 
- Arias_scoring_fake.xlsx : ID, Opera, Aria, Year, Composer, Act, Scene, Act & Scene, City, Country, Character, Type (character type), Form, fl (equal to 1 when a flute is present in the aria and 0 otherwise), ob (equal to 1 when an oboe is present in the aria and 0 otherwise), eh (equal to 1 when an English horn is present in the aria and 0 otherwise), hn (equal to 1 when a horn is present in the aria and 0 otherwise), and tp (equal to 1 when a trumpet is present in the aria and 0 otherwise). 

Other variables can be found in the files, however, they are not relevant in the app. It is important that the uploaded files have the same structure as these files.

Users can display several type of graphs of different variables and custom how they want to visualize the data in two distinct sections:

- Variable section: This section includes a description of the variables, a customizable data table, and some graphics of the variables (marginal graphs, two/three variables graphs, and regression graphs).
- Temporal evolution: This section includes three different graphs in order to explore the temporal evolution of the data. When desired, these visualizations can be subject to certain factors.

The archives server.R and UI.R contain the code for the app. The first one contains the instructions of how the app is built and works, while the second one contains the layout and appearence of the app. 

Please, cite as: Sastre Martinez de Azagra, Leyre: “AriasViewer: A tool for interactive exploration ofmusical proportions”, Master’s thesis, Universidad Carlos III de Madrid, 2021.
### We hope you find the app useful. 
