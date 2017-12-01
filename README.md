# Motiviation
When I first had to check my grade for my first programming assignment I was apalled that it was organized into a huge html table with a non sticky header, so I constantly had to scroll up and down just to line up which column belonged to which category. But the chain of thought is that once you see your grade, you would like to visualize the grade distribution for the assignment as to get a feel for the curve of the class. So I navigated to the distribution part of gradesource. Unfortunately, the histogram, was in the form of an html table. The sizes of each tick was uniform so when you had an enormous scroll bar if the grades mostly fell into on of the histogram bins. Which made it hard for the professor to show the class how they did.

# What gradesourceTidier does
So what I did was use R, since I've heard that it's a great statistical language, to tabulate and graph this data into an easy to read format. I went further and took advantage of the R Shiny framework to create a SPA website where students can easily input their data and obtain a table with only information relevant to them and drop down boxes to select the grade distribution for any assignment.
