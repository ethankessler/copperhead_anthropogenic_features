# copperhead_anthropogenic_features
Code and data to replicate analysis of northern copperhead movement and habitat selection in response to anthropogenic linear features in an urban nature park.

The analyses have been split into two sets of code to ensure sensitive snake locations remain private. 

code/acon_movement_rand.R contains a version of the full code utilized to analyze data in the associated manuscript, but generates random movement paths within the study area by generating a random starting location and generating an annual movement path within the sample size contained in our data set and utilizing the results of our sex-specific hurdle model to generate steps.

code/acon_movement_actual.R contains code to run the resulting feature crossing data with all spatial information removed. The code imports the data used to run the movement probability/step length hurdle model, the random paths analyses, and the step selection functions, then runs the code used to run the analyses and create the figures and tables presented in the manuscript.
