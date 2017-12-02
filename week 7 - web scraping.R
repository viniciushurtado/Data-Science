#week 7 - pre work - web scraping

setwd("~/Stanford/Courses/Data Science/R Files/DataSet/")
library(RSelenium) 
RSelenium::rsDriver() #not working - as I used my work PC and I don't have adm access

vignette("RSelenium-docker", package = "RSelenium") #steps that i followed to make it work on my PC (should install docker and VNC viewer)

##Docker help - usefull commands in docker applicaton----
# docker ps -Visualize running dockers
# docker ps -q -Stop containers
# docker stop $(docker ps -q) -Stop containers
# docker run -d -p 4445:4444 selenium/standalone-chrome -Run a server for example using Docker (w/o visualizaion)

##RSelenium code----

# Step 1: run command on docker app to create the new server: docker run -d -p 4445:4444 -p 5901:5900 selenium/standalone-chrome-debug
# Step 2: run R code
library(RSelenium)
remDr <- remoteDriver(remoteServerAddr = "192.168.99.100",port = 4445L,browserName="chrome")
remDr$open(silent=TRUE)
# Step 3: open VNC viewer to view browser navigation
#remDr$navigate("http://www.google.com/ncr")
#remDr$getTitle
#remDr$screenshot(display = TRUE)
#remDr = rD$client
#remDr$goBack()
#remDr$goForward()
#remDr$getCurrentUrl()
#//*[@id="hdtb-msb-vis"]/div[4]/a #xpath change for simple couts

for (i in 1:10){
remDr$navigate("http://google.com")
webElem=remDr$findElement(using="id","lst-ib") #look for the id of element using inspect
webElem$sendKeysToElement(list("How old is Obama?\n"))
Sys.sleep(1) #number of seconds to delay
webElem = remDr$findElement(using="xpath","//*[@id='hdtb-msb-vis']/div[3]/a")
webElem$clickElement()
}