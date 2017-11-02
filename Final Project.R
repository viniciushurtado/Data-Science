library(utils)
library(ggplot2)
library(caret)

setwd("~/Stanford/Courses/Data Science/Final Work/DataSets")

Cleveland = read.csv("processed.cleveland.data",header=FALSE)
Switzerland = read.csv("processed.switzerland.data",header=FALSE)
Va = read.csv("processed.va.data",header=FALSE)
Hungarian = read.csv("processed.hungarian.data",header=FALSE)
