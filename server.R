#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(sf)
library(readr)
library(ggplot2)
library(tidyverse)
library(scales)


#csv files
characters = read_csv("data/characters.csv")
episodes = read_csv("data/episodes.csv")
scenes = read_csv("data/scenes.csv")
appearances = read_csv("data/appearances.csv")

#shp files
land = st_read("data/GoTRelease/Land.shp",crs=4326)
locations=st_read("data/GoTRelease/Locations.shp",crs=4326)
lakes=st_read("data/GoTRelease/Lakes.shp",crs=4326)
conts=st_read("data/GoTRelease/Continents.shp",crs=4326)
wall=st_read("data/GoTRelease/Wall.shp",crs=4326)
islands=st_read("data/GoTRelease/Islands.shp",crs=4326)
kingdoms=st_read("data/GoTRelease/Political.shp",crs=4326)
landscapes=st_read("data/GoTRelease/Landscape.shp",crs=4326)
roads=st_read("data/GoTRelease/Roads.shp",crs=4326)
rivers=st_read("data/GoTRelease/Rivers.shp",crs=4326)
scenes_locations=st_read("data/GoTRelease/ScenesLocations.shp",crs=4326)



# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  

  #the cumulative death count plot
  output$deathPlot <- renderPlot({
    # nombre de morts cumules et temps passe
    deaths = scenes %>% select(nbdeath,duration,location,episodeId) %>% 
      mutate(t=cumsum(duration),tdeath=cumsum(nbdeath))
    
    season_t = episodes %>% mutate(ld=lag(total_duration)) %>% 
      mutate(ld=if_else(is.na(ld),0,ld), td = cumsum(ld)) %>% 
      filter(episodeNum==1) %>% pull(td)
    
    # geom_line + labels personalisés
    ggplot(deaths) + geom_line(aes(x=t/3600,y=tdeath)) +
      scale_x_continuous("",expand = c(0,0),breaks = season_t/3600,
                         labels =   paste("Season",1:8))+
      scale_y_continuous("Cumulative number of deaths", expand=c(0,0))+
      theme_bw()+
      theme(axis.text.x=element_text(angle=90))
      #+ggtitle("Evolution of the number of deaths over time")
    
  })
  
  #appearance plot
  output$appearancePlot <- renderPlot({
    
    jstime = appearances %>% filter(name == input$name) %>% 
      left_join(scenes) %>% 
      group_by(episodeId) %>% 
      summarise(time=sum(duration))
    jstime = jstime %>% left_join(episodes %>% 
      select("episodeId", "seasonNum", "episodeNum"))
    
    if (input$numsaison != "All seasons"){
      jstime = jstime %>%
        filter(seasonNum == input$numsaison)
      
      ggplot(jstime) + 
        geom_line(aes(x=episodeNum,y=time))+
        scale_y_continuous(breaks= pretty_breaks())+
        theme_bw()+
        xlab("episode")+ylab("time (sec)")+
        ggtitle( paste("Attendance time per episode of ", input$name, sep=""))
      
      
    } else {
      season_ap = jstime %>% group_by(seasonNum) %>% 
        summarise(x=min(episodeId))
      
      ggplot(jstime) + 
        geom_line(aes(x=episodeId,y=time))+
        scale_x_continuous("",expand = c(0,0),breaks = season_ap$x,
                           labels = paste("Season",season_ap$seasonNum))+
        theme_bw()+
        ylab("time (sec)")+
        theme(axis.text.x=element_text(angle=90))+
        ggtitle( paste("Attendance time per episode of ", input$name, sep=""))
    }
    
    
  })
  
  output$appearencePSPlot <- renderPlot({
    screenTimePerSeasons = appearances %>% left_join(scenes) %>% 
      left_join(episodes) %>% 
      group_by(name,seasonNum) %>% 
      summarise(screenTime=sum(duration)) %>% 
      arrange(desc(screenTime)) 
    screenTimeTotal = screenTimePerSeasons %>% 
      group_by(name) %>% 
      summarise(screenTimeTotal=sum(screenTime))
    mainCharacters = screenTimeTotal %>% 
      filter(screenTimeTotal>60*60) %>% 
      arrange(screenTimeTotal) %>% 
      mutate(nameF=factor(name,levels = name))
    data = screenTimePerSeasons %>% left_join(mainCharacters) %>% filter(!is.na(nameF))
    
    ggplot(data)+
      geom_bar(aes(y=nameF,x=screenTime/60,fill=factor(seasonNum,level=8:1)),stat="identity")+
      scale_fill_brewer("Season",palette = "Spectral")+theme_bw()+
      geom_text(data=mainCharacters,aes(y=nameF,x=screenTimeTotal/60+5,label=paste(round(screenTimeTotal/60),'min')),hjust = "left")+
      scale_x_continuous("Onset time (min)",breaks = seq(0,750,by=120),limits = c(0,780),expand = c(0,1))+
      ylab("")
      #+ggtitle("Cumulative appearance time per character and season")
    
    
  })
  


  
  output$mapGOT <- renderPlot({
    colforest="#c0d7c2"
    colriver="#7ec9dc"
    colriver="#87cdde"
    colland="ivory"
    borderland = "ivory3"  
    
    if (input$houses == "main_char"){ #main charachters
      if(is.null(input$members_main)){
        ggplot()+geom_sf(data=land,fill=colland,col=borderland,size=0.1)+
          geom_sf(data=islands,fill=colland,col="ivory3")+
          geom_sf(data=landscapes %>% filter(type=="forest"),fill=colforest,col=colforest)+
          geom_sf(data=rivers,col=colriver)+
          geom_sf(data=lakes,col=colriver,fill=colriver)+
          geom_sf(data=wall,col="black",size=1)+
          geom_sf(data = locations %>% filter(size>4,name!='Tolos'), color = "black", size = 0.1) +
          xlab("")+ylab("")+
          geom_sf_text(data= locations %>% filter(size>4,name!='Tolos'),aes(label=name),size=2.5,family="Palatino", fontface="italic")+
          theme_minimal()+coord_sf(expand = 0,ndiscr = 0) +
          theme(panel.background = element_rect(fill = colriver,color=NA))
      } else {
        landpol = st_union(st_geometry(land)) 
        islandpol = st_union(st_geometry(islands))
        backpol=st_union(landpol,islandpol)
        background = st_as_sf(data.frame(name=input$members_main,geometry=rep(backpol,length(input$members_main))))
        
        loc_time=appearances %>% filter(name %in% input$members_main) %>% left_join(scenes) %>% group_by(location,name) %>% summarize(duration=sum(duration,na.rm=TRUE)) 
        loc_time_mc = scenes_locations %>% left_join(loc_time)
        
        ggplot()+geom_sf(data=background,color=borderland,fill=colland)+
          geom_sf(data=loc_time_mc%>% filter(!is.na(duration)),aes(size=duration/60,color=name))+
          geom_sf_text(data=loc_time_mc%>% filter(duration>60*60),aes(label=location),color="#000000",vjust="bottom",family="Palatino", fontface="italic")+
          coord_sf(expand = 0,ndiscr = 0)+
          theme(panel.background = element_rect(fill = colriver,color=NA))+
          xlab("")+ylab("")+
          facet_wrap(~name)
      }
    } else if (input$houses == "targaryen_char"){ #targaryen house
      if(is.null(input$members_targ)){
        ggplot()+geom_sf(data=land,fill=colland,col=borderland,size=0.1)+
          geom_sf(data=islands,fill=colland,col="ivory3")+
          geom_sf(data=landscapes %>% filter(type=="forest"),fill=colforest,col=colforest)+
          geom_sf(data=rivers,col=colriver)+
          geom_sf(data=lakes,col=colriver,fill=colriver)+
          geom_sf(data=wall,col="black",size=1)+
          geom_sf(data = locations %>% filter(size>4,name!='Tolos'), color = "black", size = 0.1) +
          xlab("")+ylab("")+
          geom_sf_text(data= locations %>% filter(size>4,name!='Tolos'),aes(label=name),size=2.5,family="Palatino", fontface="italic")+
          theme_minimal()+coord_sf(expand = 0,ndiscr = 0) +
          theme(panel.background = element_rect(fill = colriver,color=NA))
      } else {
        landpol = st_union(st_geometry(land)) 
        islandpol = st_union(st_geometry(islands))
        backpol=st_union(landpol,islandpol)
        background = st_as_sf(data.frame(name=input$members_targ,geometry=rep(backpol,length(input$members_targ))))
        
        loc_time=appearances %>% filter(name %in% input$members_targ) %>% left_join(scenes) %>% group_by(location,name) %>% summarize(duration=sum(duration,na.rm=TRUE)) 
        loc_time_mc = scenes_locations %>% left_join(loc_time)
        
        ggplot()+geom_sf(data=background,color=borderland,fill=colland)+
          geom_sf(data=loc_time_mc%>% filter(!is.na(duration)),aes(size=duration/60,color=name))+
          geom_sf_text(data=loc_time_mc%>% filter(duration>60*60),aes(label=location),color="#000000",vjust="bottom",family="Palatino", fontface="italic")+
          coord_sf(expand = 0,ndiscr = 0)+
          xlab("")+ylab("")+
          facet_wrap(~name)
      }
    } else if (input$houses == "stark_char"){ #stark house
      if(is.null(input$members_stark)){
        ggplot()+geom_sf(data=land,fill=colland,col=borderland,size=0.1)+
          geom_sf(data=islands,fill=colland,col="ivory3")+
          geom_sf(data=landscapes %>% filter(type=="forest"),fill=colforest,col=colforest)+
          geom_sf(data=rivers,col=colriver)+
          geom_sf(data=lakes,col=colriver,fill=colriver)+
          geom_sf(data=wall,col="black",size=1)+
          geom_sf(data = locations %>% filter(size>4,name!='Tolos'), color = "black", size = 0.1) +
          xlab("")+ylab("")+
          geom_sf_text(data= locations %>% filter(size>4,name!='Tolos'),aes(label=name),size=2.5,family="Palatino", fontface="italic")+
          theme_minimal()+coord_sf(expand = 0,ndiscr = 0) +
          theme(panel.background = element_rect(fill = colriver,color=NA))
      } else {
        landpol = st_union(st_geometry(land)) 
        islandpol = st_union(st_geometry(islands))
        backpol=st_union(landpol,islandpol)
        background = st_as_sf(data.frame(name=input$members_stark,geometry=rep(backpol,length(input$members_stark))))
        
        loc_time=appearances %>% filter(name %in% input$members_stark) %>% left_join(scenes) %>% group_by(location,name) %>% summarize(duration=sum(duration,na.rm=TRUE)) 
        loc_time_mc = scenes_locations %>% left_join(loc_time)
        
        ggplot()+geom_sf(data=background,color=borderland,fill=colland)+
          geom_sf(data=loc_time_mc%>% filter(!is.na(duration)),aes(size=duration/60,color=name))+
          geom_sf_text(data=loc_time_mc%>% filter(duration>60*60),aes(label=location),color="#000000",vjust="bottom",family="Palatino", fontface="italic")+
          coord_sf(expand = 0,ndiscr = 0)+
          xlab("")+ylab("")+
          facet_wrap(~name)
      }
    } else if (input$houses == "lannister_char"){ #lannister house
      if(is.null(input$members_lann)){
        ggplot()+geom_sf(data=land,fill=colland,col=borderland,size=0.1)+
          geom_sf(data=islands,fill=colland,col="ivory3")+
          geom_sf(data=landscapes %>% filter(type=="forest"),fill=colforest,col=colforest)+
          geom_sf(data=rivers,col=colriver)+
          geom_sf(data=lakes,col=colriver,fill=colriver)+
          geom_sf(data=wall,col="black",size=1)+
          geom_sf(data = locations %>% filter(size>4,name!='Tolos'), color = "black", size = 0.1) +
          xlab("")+ylab("")+
          geom_sf_text(data= locations %>% filter(size>4,name!='Tolos'),aes(label=name),size=2.5,family="Palatino", fontface="italic")+
          theme_minimal()+coord_sf(expand = 0,ndiscr = 0) +
          theme(panel.background = element_rect(fill = colriver,color=NA))
      } else {
        landpol = st_union(st_geometry(land)) 
        islandpol = st_union(st_geometry(islands))
        backpol=st_union(landpol,islandpol)
        background = st_as_sf(data.frame(name=input$members_lann,geometry=rep(backpol,length(input$members_lann))))
        
        loc_time=appearances %>% filter(name %in% input$members_lann) %>% left_join(scenes) %>% group_by(location,name) %>% summarize(duration=sum(duration,na.rm=TRUE)) 
        loc_time_mc = scenes_locations %>% left_join(loc_time)
        
        ggplot()+geom_sf(data=background,color=borderland,fill=colland)+
          geom_sf(data=loc_time_mc%>% filter(!is.na(duration)),aes(size=duration/60,color=name))+
          geom_sf_text(data=loc_time_mc%>% filter(duration>60*60),aes(label=location),color="#000000",vjust="bottom",family="Palatino", fontface="italic")+
          coord_sf(expand = 0,ndiscr = 0)+
          xlab("")+ylab("")+
          facet_wrap(~name)
      }
    } else if (input$houses == "greyjoy_char"){ #greyjoy house
      if(is.null(input$members_grey)){
        ggplot()+geom_sf(data=land,fill=colland,col=borderland,size=0.1)+
          geom_sf(data=islands,fill=colland,col="ivory3")+
          geom_sf(data=landscapes %>% filter(type=="forest"),fill=colforest,col=colforest)+
          geom_sf(data=rivers,col=colriver)+
          geom_sf(data=lakes,col=colriver,fill=colriver)+
          geom_sf(data=wall,col="black",size=1)+
          geom_sf(data = locations %>% filter(size>4,name!='Tolos'), color = "black", size = 0.1) +
          xlab("")+ylab("")+
          geom_sf_text(data= locations %>% filter(size>4,name!='Tolos'),aes(label=name),size=2.5,family="Palatino", fontface="italic")+
          theme_minimal()+coord_sf(expand = 0,ndiscr = 0) +
          theme(panel.background = element_rect(fill = colriver,color=NA))
      } else {
        landpol = st_union(st_geometry(land)) 
        islandpol = st_union(st_geometry(islands))
        backpol=st_union(landpol,islandpol)
        background = st_as_sf(data.frame(name=input$members_grey,geometry=rep(backpol,length(input$members_grey))))
        
        loc_time=appearances %>% filter(name %in% input$members_grey) %>% left_join(scenes) %>% group_by(location,name) %>% summarize(duration=sum(duration,na.rm=TRUE)) 
        loc_time_mc = scenes_locations %>% left_join(loc_time)
        
        ggplot()+geom_sf(data=background,color=borderland,fill=colland)+
          geom_sf(data=loc_time_mc%>% filter(!is.na(duration)),aes(size=duration/60,color=name))+
          geom_sf_text(data=loc_time_mc%>% filter(duration>60*60),aes(label=location),color="#000000",vjust="bottom",family="Palatino", fontface="italic")+
          coord_sf(expand = 0,ndiscr = 0)+
          xlab("")+ylab("")+
          facet_wrap(~name)
      }
    }
    
  })
  
  

})
