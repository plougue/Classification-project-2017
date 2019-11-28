# A first try at classification with real data sets

## Introduction 

This project was a school project for my first machine learning class in 2017. We were given 3 real-life data sets. The idea was to test classification methods we were taught in order to build a classifier as precise as possible. Everything had to be implemented using R.
The given data sets were already quite "clean", and 2 of them had gotten through a lot of crucial pre-processing. This was still a very interesting first concrete exercise to experiment with classification methods.
Part of the grade was given according to how well the classifier suceeded when compared to the rest of the class. The classifiers I had built were ranked among the highest of my class and I was quite proud of the result at the time.

## The data sets

### Expressions
1O8 60x70 images of faces represented by a vector of size 4200 containing greyscale values of each pixel.
Each image is labeled with the corresponding facial expression among : joy, surprise, dasness, digust, anger, fear.

### Characters
10000 training vectors of 16 features pre-processed from images of characters from A to Z labelled by the corresponding letter.

### Speech
2250 : of 256 pre-processed features extracted from recording each time a different variation of speaker and phoneme.
Labelled by the corresponding phoneme : "sh" (**sh**e), "del" (**d**ark), "iy" (sh**e**), "aa" (d**a**rk), "ao" (w**a**ter).

## Files and results
**RELEVANT FILES ARE WRITTEN IN FRENCH**, as was custom at the time in my engineering diploma.
* project.pdf contains the instructions with a description of the data sets
* report.pdf contains a 12 page analysis of the data sets and how classification methods compared to one another. The different methods were compared mostly using cross-validation. The main work of the report was to analyse why method X was better than method Y according to things like the number of features and samples.
* Expression.R Expression_svm.R contains R files in which I experimented with different methods for the "Expression" data sets. This file was meant to experiment only and is not clean by any mean. There were similar files for the other 2 data sets but they got lost.
* classifieur_X.R contains the classifier trained with data set X using mostly information contained in env.Rdata
