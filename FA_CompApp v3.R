library(shiny)
library(shinyMobile)
library(apexcharter)
library(shinyWidgets)
library(hockeyR)
library(rlang)
library(sportyR)
library(dplyr)
library(ggimage)
library(ggplot2)
library(tidyverse)
library(data.table)
library(ggrepel)
library(ggdark)
library(gt)
library(gtExtras)
library(formattable)
library(thematic)
library(shinydashboard)
library(nhlapi)
library(highcharter)
library(nhlapi)
library(dplyr)
library(plotly)
library(rvest)
library(lubridate)
library(stringr)
library(profvis)


gt_theme_dark <- function (gt_object, ...) 
{
  stopifnot(`'gt_object' must be a 'gt_tbl', have you accidentally passed raw data?` = "gt_tbl" %in% 
              class(gt_object))
  gt_object %>% tab_options(heading.align = "left", heading.border.bottom.style = "none", 
                            table.background.color = "#000000", table.font.color.light = "white", 
                            table.border.top.style = "none", table.border.bottom.color = "#000000", 
                            table.border.left.color = "#000000", table.border.right.color = "#000000", 
                            table_body.border.top.style = "none", table_body.border.bottom.color = "#000000", 
                            column_labels.border.top.style = "none", column_labels.background.color = "#000000", 
                            column_labels.border.bottom.width = 3, column_labels.border.bottom.color = "white", 
                            data_row.padding = px(7), ...) %>% tab_style(style = cell_text(color = "white", 
                                                                                           font = google_font("Source Sans Pro"), transform = "uppercase"), 
                                                                         locations = cells_column_labels(everything())) %>% tab_style(style = cell_text(font = google_font("Libre Franklin"), 
                                                                                                                                                        weight = 800), locations = cells_title(groups = "title")) %>% 
    tab_style(style = cell_text(font = google_font("Source Sans Pro"), 
                                weight = 400), locations = cells_body())
}
# test <- merge(skater_stats23 %>% 
#   filter(player=="Connor McDavid"),
#   cm_stats,
#   by.x=c"seasonStart",by.y="seasonStart

#cm_stats <- nhl_players_seasons(playerIds = c(8478402), seasons = c(2019:2021))
# 
# stat <- cm_stats %>% 
#   dplyr::pull(stat.evenTimeOnIce)


cyear <- 2023
zyear <- cyear-3

expirytype <- c("UFA","RFA")

## Temp Import Solution ##
#skater_stats23 <- fread(file="sznminussix.csv")

skater_stats1 <- fread(file="skater_statsbind.csv")

three_szn_agg <- fread(file="three_szn_agg.csv")
  
skater_stats2 <- rbind(get_skater_stats_hr(2023),
        skater_stats1)%>% 
  mutate(seasonStart=as.numeric(sub("(^[^-]+)-.*","\\1",season)))
  
skater_stats23 <- skater_stats2 %>%
  select(player,season:time_on_ice,blocks:seasonStart) %>% 
  group_by(player,season,age,position,link,player_id,seasonStart) %>%
  summarise_all(sum)
  

vars <- fread(file="cfscrape.csv")%>% 
  dplyr::filter(EXP..YEAR >= zyear, EXP..YEAR <= cyear, season >= zyear, season <= cyear) %>% 
  dplyr::mutate(fa_ind=ifelse(EXP..YEAR==season,"T","F"))



# skater_stats23 <- skater_stats23 %>% 
#   mutate(seasonStart=as.numeric(sub("(^[^-]+)-.*","\\1",season)))

skater_stats_master <- skater_stats23 %>%
  dplyr::select(player:hits) %>%
  dplyr::mutate(GP=games_played, G=goals, A=assists,  Pts=points,
                '+/-'=plus_minus, PIMS=penalty_minutes, SOG=shots_on_goal,'avg TOI'=round(time_on_ice/games_played,1),
                g_pg=goals/games_played, a_pg=assists/games_played, p_pg=points/games_played,
                pim_pg=penalty_minutes/games_played, sog_pg=(shots_on_goal/games_played), blk_pg=(blocks/games_played), hits_pg=(hits/games_played),
                g_per20=(goals/time_on_ice)*20, a_per20=(assists/time_on_ice)*20, pt_per20=(points/time_on_ice)*20,
                pim_per20=(penalty_minutes/time_on_ice)*20,sog_per20=(shots_on_goal/time_on_ice)*20,blk_per20=(blocks/time_on_ice)*20,hits_per20=(hits/time_on_ice)*20,
                g_82=(goals/games_played)*82, a_82=(assists/games_played)*82, p_82=(points/games_played)*82,
                pim_82=(penalty_minutes/games_played)*82, sog_82=(shots_on_goal/games_played)*82, blk_82=(blocks/games_played)*82, hits_82=(hits/games_played)*82,
                'ES G'=goals_even_strength, '% of G @ ES'=round((goals_even_strength/goals),2), 'ES A'=assists_even_strength, "% of A @ ES"=round((assists_even_strength/assists),2), 'ES Pts'=goals_even_strength+assists_even_strength, '% of Pts @ ES'=round(((goals_even_strength+assists_even_strength)/points),2),
                evg_pg=goals_even_strength/games_played, eva_pg=assists_even_strength/games_played, evp_pg=(goals_even_strength + assists_even_strength)/games_played,
                evg_per20=(goals_even_strength/time_on_ice)*20, eva_per20=(assists_even_strength/time_on_ice)*20, evpt_per20=((goals_even_strength + assists_even_strength)/time_on_ice)*20,
                evg_82=(goals_even_strength/games_played)*82, eva_82=(assists_even_strength/games_played)*82, evp_82=((goals_even_strength+assists_even_strength)/games_played)*82) %>%
  dplyr::mutate('g pg'=round(g_pg,2), 'a pg'=round(a_pg,2),'p pg'=round(p_pg,2),
                'pim pg'=round(pim_pg,2),'sog pg'=round(sog_pg,2),'blk pg'=round(blk_pg,2),'hits pg'=round(hits_pg,2),
                'ev g pg'=round(evg_pg,2), 'ev a pg'=round(eva_pg,2),'ev pts pg'=round(evp_pg,2),
                'g per20'=round(g_per20,2),'a per20'=round(a_per20,2),'pts per20'=round(pt_per20,2),
                'pim per20'=round(pim_per20,2),'sog per20'=round(sog_per20,2),'blk per20'=round(blk_per20,2),'hits per20'=round(hits_per20,2),
                'ev g per20'=round(evg_per20,2),'ev a per20'=round(eva_per20,2),'ev pts per20'=round(evpt_per20,2),
                'g 82'=round(g_82,2), 'a 82'=round(a_82,2),'pts 82'=round(p_82,2),
                'pim 82'=round(pim_82,2),'sog 82'=round(sog_82,2),'blk 82'=round(blk_82,2),'hits 82'=round(hits_82,2),
                'EV g 82'=round(evg_82,2), 'EV a 82'=round(eva_82,2),'EV pts 82'=round(evp_82,2),pos_group=ifelse(position!='D','F','D'))


skater_stats_master <- skater_stats_master %>%
  dplyr::select(player:position,blocks:'avg TOI','ES G':'% of Pts @ ES','g pg':pos_group)%>% 
  mutate(season=as.numeric(str_extract(season,"[^-]+"))+1, PlayerSeason=paste0(player," ",season))

pos <- skater_stats_master %>% 
  dplyr::pull(pos_group) %>% 
  unique() %>% 
  sort()

players <- vars %>%  
  dplyr::pull(PLAYER) %>%  
  unique() %>% 
  sort()


loginserver <- function (input, output, session, user, password) 
{
  ns <- session$ns
  modId <- strsplit(ns(""), "-")[[1]][1]
  shiny::observeEvent(input$login, {
    if (input$login_user == user &
        input$login_password == password) {
      updateF7Login(id = modId, user = input$login_user, 
                    password = input$login_password)
    } else {
      shinyalert::shinyalert("Not correct password or username", type = "error")
    }
  })
}

shinyMobile_options <- list(
  theme = "auto",
  dark = TRUE,
  filled = FALSE,
  color = "green",
  iosTranslucentBars = FALSE,
  navbar = list(
    iosCenterTitle = TRUE,
    hideOnPageScroll = TRUE
  ),
  toolbar = list(
    hideOnPageScroll = FALSE
  )
)





shinyApp(
  ui = f7Page(
    title = "Welcome",
    options = shinyMobile_options,
    f7Login(id = "loginPage", title = "Welcome to the NHL Free Agent Market Analysis Tool"),
    f7TabLayout(
      panels = tagList(
        f7Panel(title = "Player Inputs", side = "left", theme = "dark", 
                
                f7Slider(
                  inputId = "yearfinder",
                  label= "Platform Year(s)",
                  min=2020,
                  max=2023,
                  value=c(2020,2022),
                  step=1
                ),
                
                
                f7SmartSelect(
                  inputId = 'expirytype',
                  label="Expiry Type",
                  choices=expirytype,
                  selected="all/",
                  multiple = FALSE,
                  openIn = "popup"
                )

                ,
                
                f7SmartSelect(
                  inputId = 'pos',
                  label="Position",
                  choices=pos,
                  selected=pos[2],
                  multiple = TRUE,
                  openIn = "popup"
                ),
                
                "Choose Player to Highlight:",
                uiOutput("playerchoices"),
                
                "Choose Comparables:",
                uiOutput("compchoices")
                
                #,
                
                #uiOutput("expirychoices")
              
                # ,
                # 
                # f7Radio('expirytype'
                #         ,"Expiry Type",
                #         choices=expirytype,
                #         selected="all/")
                
                , effect = "cover"),
        f7Panel(title = "Comparable Filters", side = "right", theme = "dark", 
                
                f7Slider(
                  inputId = "agefinder",
                  label= "Age (7/1)",
                  min=18,
                  max=45,
                  value=c(20,40),
                  step=1
                ),
          
                
                f7Slider(
                  inputId = "gpfinder",
                  label= "Games Played",
                  min=0,
                  max=90,
                  value=c(50,82),
                  step=2
                ),

                f7Slider(
                  inputId = "ptsfinder",
                  label= "Points",
                  min=0,
                  max=180,
                  value=c(50,82),
                  step=2
                ),
                
                
                f7Slider(
                  inputId = "pgpfinder",
                  label= "Points per Game",
                  min=0,
                  max=2.5,
                  value=c(0,2.5),
                  step=0.1
                ),

                f7Slider(
                  inputId = "ptsp20finder",
                  label= "Points per20",
                  min=0,
                  max=2.5,
                  value=c(0,2.5),
                  step=0.1
                )

                , effect = "cover")
      ),
      navbar = f7Navbar(
        title = "NHL Free Agent Market Analysis Tool",
        subtitle = "Created by Batt Analytics",
        hairline = TRUE,
        shadow = TRUE,
        leftPanel = TRUE,
        rightPanel = TRUE,
        subNavbar = f7SubNavbar(
          f7Button(label = "Website"),
          f7Button(label = "CapFriendly", href="https://www.capfriendly.com/browse/active"),
          f7Button(label = "HockeyReference", href="https://www.hockey-reference.com/")
        )
      ),
      f7Tabs(
        animated = TRUE,
        #swipeable = TRUE,
        f7Tab(
          tabName = "Tab3",
          title="Comparison Tool",
          icon = f7Icon("person_3_fill"),
          f7Row(
            
            f7Col(
              
              shiny::uiOutput("exlist"),
              shiny::uiOutput("exlist2"),
              shiny::uiOutput("exlist3"),
              shiny::uiOutput("exlist4"),
              shiny::uiOutput("exlist5"),
              shiny::uiOutput("exlist6"),
              shiny::uiOutput("exlist7")
              
            ) 
          
          )
          ,
          f7Shadow(
            intensity = 10,
            hover = TRUE,
            f7Card(
              title = "Predictions/Model",
              gt_output("compsummary"),
              f7Row(
              f7Col(f7Stepper(inputId = "Cap", label = "Salary Cap (MM)", value= 83.5 , min = 83.5, max=90.0, step = 0.5)),
              
              f7Col(f7Stepper(inputId = "CHpct", label = "Cap Hit %", value= 5.0, min = 0.0, max=20.0, step = 0.1))
              ),
              br(),
              f7Row(
                f7Col(
                  
                ),
                f7Col(shiny::uiOutput("capbutton")),
                f7Col(
                  
                )
              )
              )
          )
        ),
        
        f7Tab(
          tabName = "Tab2",
          title="Stats",
          icon = f7Icon("graph_circle_fill"),
          
          f7Block(
            
           f7Tabs(
            f7Tab(
              tabName="Platformyr",
              title="Platform Yr",
            f7Tabs(
              
              f7Tab(
                tabName = "Standard",
                title="Standard",
                
                f7Row(
                  f7Tabs(
                    f7Tab(
                      tabName="BoxScore",
                      title="Box Score",
                      gt_output("highlightboxscoredata"),
                      gt_output("boxscoredata")
                    ),
                    f7Tab(
                      tabName="EvenStrength",
                      title="Even Strength", 
                      gt_output("highlightevboxscoredata"),
                      gt_output("evboxscoredata")
                    )
                    
                    
                  )),
                f7Row(
                  f7Tabs(
                    f7Tab(
                      tabName="GPvPts",
                      title="GP v Pts"
                      ,plotlyOutput("boxscoreplot")
                    ),
                    f7Tab(
                      tabName="GvAsts",
                      title="G v Asts"
                      ,plotlyOutput("goalvassistplot")
                    ),
                    f7Tab(
                      tabName="EVGvAsts",
                      title="ES G v Asts"
                      ,plotlyOutput("evgoalvassistplot")
                    )
                    
                    
                  ))
                
              ),
              #f7Tab(
                #tabName = "PerG60",
                #title="PG60"
              #),
              f7Tab(
                tabName = "PG",
                title="Per Game",
                
                f7Row(
                  f7Tabs(
                    f7Tab(
                      tabName="BoxScorePG",
                      title="Box Score PG",
                      gt_output("hlpergameboxscoredata"),
                      gt_output("pergameboxscoredata")
                    )
                    ,
                    f7Tab(
                      tabName="EvenStrengthPG",
                      title="Even Strength PG",
                      gt_output("hlevpergameboxscoredata"),
                      gt_output("evpergameboxscoredata")
                    )
                    
                    
                  )),
                f7Row(
                  f7Tabs(
                    f7Tab(
                      tabName="PtspergamevGP",
                      title="Pts per game v GP"
                      ,plotlyOutput("pergamegpvpts")
                    ),
                    f7Tab(
                      tabName="GvAstsPG",
                      title="G v Asts per game"
                      ,plotlyOutput("pergamegva")
                    ),
                    f7Tab(
                      tabName="evGvAstsPG",
                      title="ES G v Asts per game"
                      ,plotlyOutput("evpergamegva")
                    )
                    
                    
                  ))
              ),
              f7Tab(
                tabName = "P20",
                title="per20",
                
                f7Row(
                  f7Tabs(
                    f7Tab(
                      tabName="BoxScoreP20",
                      title="Box Score per20",
                      gt_output("hlper20boxscoredata"),
                      gt_output("per20boxscoredata")
                    
                    ),
                    f7Tab(
                      tabName="EvenStrengthP20",
                      title="Even Strength per20",
                      gt_output("hlevper20boxscoredata"),
                      gt_output("evper20boxscoredata")
                    )
                    
                    
                  )),
                f7Row(
                  f7Tabs(
                    f7Tab(
                      tabName="ptsp20toi",
                      title="Pts per 20 v TOI"
                      ,plotlyOutput("p20toivspts")
                    ),
                    f7Tab(
                      tabName="GvAstsP20",
                      title="G v Asts per20"
                      ,plotlyOutput("p20gva")
                    ),
                    f7Tab(
                      tabName="evGvAstsP20",
                      title="ES G v Asts per20"
                      ,plotlyOutput("evp20gva")
                    )
                    
                  ))
              ),
              
              f7Tab(
                tabName = "82pace",
                title="82 gm pace",
                
                f7Row(
                  f7Tabs(
                    f7Tab(
                      tabName="BoxScore82pace",
                      title="Box Score 82 game pace",
                      gt_output("hl82gmpaceboxscore"),
                      gt_output("82gmpaceboxscore")
                    ),
                    f7Tab(
                      tabName="EvenStrength82pace",
                      title="Even Strength 82 game pace",
                      gt_output("hl82gmpaceboxscoreev"),
                      gt_output("82gmpaceboxscoreev")
                    )
        
                  )),
                f7Row(
                  f7Tabs(
                    f7Tab(
                      tabName="ptsvgp82pace",
                      title="pts v GP 82 gm pace"
                      ,plotlyOutput("pacegpvpts")
                      #need team GP to date over player GP to date
                    ),
                    f7Tab(
                      tabName="Gvasts82pace",
                      title="G v Asts 82 gm pace"
                      ,plotlyOutput("pacegva")
                    ),
                    f7Tab(
                      tabName="evGvasts82pace",
                      title="ES G v Asts 82 gm pace"
                      ,plotlyOutput("evpacegva")
                      
                    )
                    
                    
                  ))
              )
              
            )
          ),
          f7Tab(
            tabName="threesznagg",
            title="3 Season Agg.",
            "STILL IN DEVELOPMENT"
          )
        )
        )
      ),
        f7Tab(
          tabName = "Tab1",
          title = "Bio",
          icon = f7Icon("person"),
          active = FALSE,
          gt_output("hlbios"),
          gt_output("fabios"),
          
          tags$head(
            tags$script(
              'Shiny.addCustomMessageHandler("ui-tweak", function(message) {
                var os = message.os;
                var skin = message.skin;
                if (os === "md") {
                  $("html").addClass("md");
                  $("html").removeClass("ios");
                  $(".tab-link-highlight").show();
                } else if (os === "ios") {
                  $("html").addClass("ios");
                  $("html").removeClass("md");
                  $(".tab-link-highlight").hide();
                }
                if (skin === "dark") {
                 $(".view-main").addClass("theme-dark");
                } else {
                  $(".view-main").addClass("theme-dark");
                }
               });
              '
            )
          )
          
          
          ),
        
        f7Tab(
          tabName = "ContractTab",
          title="Contracts",
          icon = f7Icon("money_dollar_circle_fill"),
          f7Shadow(
            intensity = 10,
            hover = TRUE,
            f7Card(
              title = "Players coming to market this offseason & last three offseasons (filtered by Pos Group & Exp. Type)",
              gt_output("facontracts"))
        ))
        
      )
    )
  ),
  server = function(input, output, session) {
    
    correct.login <- list("user"=c("stanley","IHC"), "password"="cup")
    callModule(loginserver, id = "loginPage",
                user = correct.login$user, password = correct.login$password)
    
    capnames2 <- reactive(
      capnames() %>% 
        dplyr::filter(EXPIRY %in% input$expirytype) %>%
        dplyr::filter(pos_group %in% input$pos)  
    )
    
    
    #list using sliders to get comp list
    complist <- reactive(
      capnames() %>%
        dplyr::mutate('Platform Year'=EXP..YEAR,'AGE (7/1)'=AGE,'Rslt Term'=RsltTerm, 'Rslt CH%'=RsltCHpct) %>%
        dplyr::filter(EXPIRY %in% input$expirytype) %>%
        dplyr::filter(pos_group %in% input$pos) %>%
        dplyr::filter(!!as.name('Platform Year') == cyear) %>%
        dplyr::filter(GP >= input$gpfinder[1], GP <= input$gpfinder[2]) %>%
        dplyr::filter(Pts >= input$ptsfinder[1], Pts <= input$ptsfinder[2]) %>%
        dplyr::filter(!!as.name('p pg') >= input$pgpfinder[1], !!as.name('p pg') <= input$pgpfinder[2]) %>%
        dplyr::filter(!!as.name('pts per20') >= input$ptsp20finder[1], !!as.name('pts per20') <= input$ptsp20finder[2]) %>%
        dplyr::filter(!!as.name('AGE (7/1)') >= input$agefinder[1], !!as.name('AGE (7/1)') <= input$agefinder[2])
    ) 
     
    #excluding highlight player
    complist2 <- reactive(
      capnames() %>%
        dplyr::mutate('Platform Year'=EXP..YEAR,'AGE (7/1)'=AGE, 'Rslt Term'=RsltTerm, 'Rslt CH%'=RsltCHpct) %>%
        dplyr::filter(!player %in% input$player) %>%
        dplyr::filter(EXPIRY %in% input$expirytype) %>%
        dplyr::filter(pos_group %in% input$pos) %>%
        dplyr::filter(!!as.name('Platform Year') >= input$yearfinder[1], !!as.name('Platform Year') <= input$yearfinder[2]) %>%
        dplyr::filter(GP >= input$gpfinder[1], GP <= input$gpfinder[2]) %>%
        dplyr::filter(Pts >= input$ptsfinder[1], Pts <= input$ptsfinder[2]) %>%
        dplyr::filter(!!as.name('p pg') >= input$pgpfinder[1], !!as.name('p pg') <= input$pgpfinder[2]) %>%
        dplyr::filter(!!as.name('pts per20') >= input$ptsp20finder[1], !!as.name('pts per20') <= input$ptsp20finder[2]) %>%
        dplyr::filter(!!as.name('AGE (7/1)') >= input$agefinder[1], !!as.name('AGE (7/1)') <= input$agefinder[2])
    ) 
    
    
    #players coming to market filter by position group for plots
    highlightpos <- reactive(
      capnames() %>% 
        dplyr::filter(pos_group %in% input$pos) 
    )
    
    #players coming to market for plot highlight and single row table
    highlightplayer <- reactive(
      complist() %>% 
        dplyr::filter(player %in% input$player)
    )
    #select comps for first page
    highlightcomps <- reactive(
      complist2() %>% 
        dplyr::filter(player %in% input$compplayer)
    )
    
    mtl_dmen <- reactive(
      complist2()
    )
    
    ax72 <- reactive(
      highlightplayer()
    )
    
    
    
    output$expirychoices <-  renderUI({
      expirytype <- c("all/","UFA","RFA")
      
      f7Flex(
      prettyRadioButtons(
        inputId = "expirychoices",
        label = "Expiry Type",
        thick = TRUE,
        inline = TRUE,
        selected = expirytype[1],
        choices = expirytype,
        animation = "pulse",
        status = "info"
      ))
      
      
    })
    outputOptions(output, "expirychoices", suspendWhenHidden = FALSE)
    
    
    output$playerchoices <-  renderUI({
       playerlist <- complist()%>% 
          dplyr::pull(player) %>% 
          sort()
      
      f7SmartSelect(
        inputId = 'player',
        label="Player:",
        choices=playerlist,
        selected=playerlist[1],
        multiple = FALSE,
        openIn = "popup"
      )
      
    })
    outputOptions(output, "playerchoices", suspendWhenHidden = FALSE)
    
    output$compchoices <-  renderUI({
      comps <- complist2()%>% 
        dplyr::pull(player) %>% 
        sort()
      
      f7SmartSelect(
        inputId = 'compplayer',
        label="Comparable(s):",
        choices=comps,
        selected=comps[1:9],
        multiple = TRUE,
        openIn = "popup"
      )
      
    })
    outputOptions(output, "compchoices", suspendWhenHidden = FALSE)
    
    output$compsummary <- render_gt({

      highlightcomps() %>%
        dplyr::select(RsltTerm, RsltCHpct) %>% 
        dplyr::summarise('Avg Length'=round(mean(RsltTerm,na.rm=TRUE),2),'Avg Cap Hit%'=round(mean(RsltCHpct,na.rm=TRUE),2)) %>%
        gt() %>%
        tab_options(table.width=pct(80)) %>%
        cols_align(align='center', columns = everything()) %>%
        gt_theme_538() %>%
        gt_theme_dark() %>%
        tab_source_note(source_note ="data from CapFriendly")
    })

    
    output$boxscoredata <- render_gt({
      
      complist2() %>% 
          dplyr::select('Platform Year',player,'AGE (7/1)',GP:'+/-',PIMS,SOG,'avg TOI':hits,'Rslt Term','RsltCHpct') %>% 
          dplyr::arrange(desc(Pts)) %>% 
          gt() %>% 
        tab_options(table.width=pct(80)) %>%
          cols_align(align='center', columns = everything()) %>% 
          gt_theme_538() %>% 
          gt_theme_dark() %>% 
        tab_source_note(source_note ="data from CapFriendly & HockeyReference")
      })
    
    
    output$highlightboxscoredata <- render_gt({
      
      highlightplayer() %>% 
        dplyr::select('Platform Year',player,'AGE (7/1)',GP:'+/-',PIMS,SOG,'avg TOI':hits,'Rslt Term','RsltCHpct') %>% 
        dplyr::arrange(desc(Pts)) %>% 
        gt() %>% 
        tab_options(table.width=pct(80)) %>%
        cols_align(align='center', columns = everything()) %>% 
        gt_theme_538() %>% 
        gt_theme_dark() 
    })
    
    
    output$evboxscoredata <-  render_gt({
      
      complist2() %>% 
        dplyr::select('Platform Year',player,'AGE (7/1)', GP,'ES G':'% of Pts @ ES','Rslt Term','RsltCHpct') %>% 
        dplyr::arrange(desc(!!as.name('ES Pts'))) %>% 
        gt() %>% 
        tab_options(table.width=pct(80)) %>% 
        fmt_percent(columns= c('% of G @ ES', "% of A @ ES", '% of Pts @ ES'), decimals=0) %>% 
        cols_align(align='center', columns = everything()) %>% 
        gt_theme_538() %>% 
        gt_theme_dark()  %>% 
        tab_source_note(source_note ="data from CapFriendly & HockeyReference")
        #gt_color_rows('ES Pts')
      
    })
    
    output$highlightevboxscoredata <-  render_gt({
      
      highlightplayer() %>% 
        dplyr::select('Platform Year',player,'AGE (7/1)', GP,'ES G':'% of Pts @ ES','Rslt Term','RsltCHpct') %>% 
        dplyr::arrange(desc('ES Pts')) %>% 
        gt() %>% 
        tab_options(table.width=pct(80)) %>%
        fmt_percent(columns= c('% of G @ ES', "% of A @ ES", '% of Pts @ ES'), decimals=0) %>% 
        cols_align(align='center', columns = everything()) %>% 
        gt_theme_538() %>% 
        gt_theme_dark() 
    })
    
    output$boxscoreplot <-  renderPlotly({

      ggplotly(highlightpos() %>%
                 ggplot(aes(x=GP, y=Pts, label=player)) +
                 geom_point(color= "white", alpha= .75, size=2) +
                 geom_point(data = mtl_dmen(), color = 'red', size=5) +
                 geom_point(data = ax72(), shape=23, size=5, fill= 'blue' )+
                 labs(x= "GP", y="Points", title= "Player Production", caption = "data from HockeyReference.com") +
                 dark_theme_bw()
               )
      })

    output$goalvassistplot <- renderPlotly({
      ggplotly(highlightpos() %>%
        ggplot(aes(x=A, y=G, label=player)) +
        geom_point(color= "white", alpha= .75, size=2) +
        geom_point(data = mtl_dmen(), color = 'red', size=5) +
        geom_point(data = ax72(), shape= 23, size=5, fill= 'blue' )+
        labs(x= "Assists", y="Goals", title= "Player Production", caption = "data from HockeyReference.com") +
        dark_theme_bw())

    })

    output$evgoalvassistplot <- renderPlotly({
      ggplotly(highlightpos() %>%
        ggplot(aes(x=!!as.name('ES A'), y=!!as.name('ES G'), label=player)) +
        geom_point(color= "white", alpha= .75, size=2) +
        geom_point(data = mtl_dmen(), color = 'red', size=5) +
        geom_point(data = ax72(), shape= 23, size=5, fill= 'blue' )+
        labs(x= "ES Assists", y="ES Goals", title= "Even Strength Production", caption = "data from HockeyReference.com") +
        dark_theme_bw())
    })
    
    output$pergameboxscoredata <-  render_gt({
      
      complist2() %>% 
        dplyr::select('Platform Year',player,'AGE (7/1)','GP','g pg':'sog pg','avg TOI','blk pg','hits pg','Rslt Term','RsltCHpct') %>% 
        dplyr::arrange(desc(!!as.name('p pg'))) %>% 
        gt() %>% 
        tab_options(table.width=pct(80)) %>%
        cols_align(align='center', columns = everything()) %>%
        gt_theme_538() %>% 
        gt_theme_dark() %>% 
        tab_source_note(source_note ="data from CapFriendly & HockeyReference")
    })
    
    output$hlpergameboxscoredata <-  render_gt({
      
      highlightplayer() %>% 
        dplyr::select('Platform Year',player,'AGE (7/1)','GP','g pg':'sog pg','avg TOI','blk pg','hits pg','Rslt Term','RsltCHpct') %>% 
        dplyr::arrange(desc('p pg')) %>% 
        gt() %>% 
        tab_options(table.width=pct(80)) %>%
        cols_align(align='center', columns = everything()) %>%
        gt_theme_538() %>% 
        gt_theme_dark() 
    })  
    
    output$evpergameboxscoredata <-  render_gt({
      
      complist2() %>% 
        dplyr::select('Platform Year',player,'AGE (7/1)','GP','ev g pg':'ev pts pg','avg TOI','Rslt Term','RsltCHpct') %>% 
        dplyr::arrange(desc(!!as.name('ev pts pg'))) %>% 
        gt() %>% 
        tab_options(table.width=pct(80)) %>%
        cols_align(align='center', columns = everything()) %>%
        gt_theme_538() %>% 
        gt_theme_dark() %>% 
        tab_source_note(source_note ="data from CapFriendly & HockeyReference")
    })
    
    output$hlevpergameboxscoredata <-  render_gt({
      
      highlightplayer() %>% 
        dplyr::select('Platform Year',player,'AGE (7/1)','GP','ev g pg':'ev pts pg','avg TOI','Rslt Term','RsltCHpct') %>% 
        dplyr::arrange(desc('evp pg')) %>% 
        gt() %>% 
        tab_options(table.width=pct(80)) %>%
        cols_align(align='center', columns = everything()) %>%
        gt_theme_538() %>% 
        gt_theme_dark()
    })
    
    output$pergamegpvpts <- renderPlotly({
      ggplotly(highlightpos() %>%
        ggplot(aes(x=GP, y=!!as.name('p pg'), label=player)) +
        geom_point(color= "white", alpha= .75, size=2) +
        geom_point(data = mtl_dmen(), color = 'red', size=5) +
        geom_point(data = ax72(), shape= 23, size=5, fill= 'blue' )+
        labs(x= "GP", y="Pts per Game", title= "Per Game Production", caption = "data from HockeyReference.com") +
        dark_theme_bw())
    })

    output$pergamegva <- renderPlotly({
      ggplotly(highlightpos() %>%
        ggplot(aes(x=!!as.name('a pg'), y=!!as.name('g pg'), label=player)) +
        geom_point(color= "white", alpha= .75, size=2) +
        geom_point(data = mtl_dmen(), color = 'red', size=5) +
        geom_point(data = ax72(), shape= 23, size=5, fill= 'blue' )+
        labs(x= "Assists per Game", y="Goals per Game", title= "Per Game Production", caption = "data from HockeyReference.com") +
        dark_theme_bw())
    })

    output$evpergamegva <- renderPlotly({
      ggplotly(highlightpos() %>%
        ggplot(aes(x=!!as.name('ev a pg'), y=!!as.name('ev g pg'), label=player)) +
        geom_point(color= "white", alpha= .75, size=2) +
        geom_point(data = mtl_dmen(), color = 'red', size=5) +
        geom_point(data = ax72(), shape= 23, size=5, fill= 'blue' )+
        labs(x= "EV Assists per Game", y="EV Goals per Game", title= "Even Strength Per Game Production", caption = "data from HockeyReference.com") +
        dark_theme_bw())
    })
    
    output$per20boxscoredata<-   render_gt({
      complist2() %>% 
        dplyr::select('Platform Year',player,'AGE (7/1)','GP','g per20':'sog per20','avg TOI','blk per20','hits per20','Rslt Term','RsltCHpct') %>% 
        dplyr::arrange(desc(!!as.name('pts per20'))) %>% 
        gt() %>%
        tab_options(table.width=pct(80)) %>%
        gt_theme_538() %>% 
        gt_theme_dark() %>% 
        tab_source_note(source_note ="data from CapFriendly & HockeyReference")
    })
    
    output$hlper20boxscoredata<-   render_gt({
      highlightplayer() %>% 
        dplyr::select('Platform Year',player,'AGE (7/1)','GP','g per20':'sog per20','avg TOI','blk per20','hits per20','Rslt Term','RsltCHpct') %>% 
        dplyr::arrange(desc('pts per20')) %>% 
        gt() %>% 
        tab_options(table.width=pct(80)) %>%
        cols_align(align='center', columns = everything()) %>%
        gt_theme_538() %>% 
        gt_theme_dark() 
    })
    
    #needs EV TOI!!!!!!
    output$evper20boxscoredata<-   render_gt({
      complist2() %>% 
        dplyr::select('Platform Year',player,'AGE (7/1)','GP','ev g per20':'ev pts per20','Rslt Term','RsltCHpct') %>% 
        dplyr::arrange(desc(!!as.name('ev pts per20'))) %>% 
        gt() %>% 
        tab_options(table.width=pct(80)) %>%
        cols_align(align='center', columns = everything()) %>%
        gt_theme_538() %>% 
        gt_theme_dark() %>% 
        tab_source_note(source_note ="data from CapFriendly & HockeyReference /n currently showing all strengths TOI")
    })
    
    #needs EV TOI!!!!!!
    output$hlevper20boxscoredata<-   render_gt({
      highlightplayer() %>% 
        dplyr::select('Platform Year',player,'AGE (7/1)','GP','ev g per20':'ev pts per20','Rslt Term','RsltCHpct') %>% 
        dplyr::arrange(desc(!!as.name('ev pts per20'))) %>% 
        gt() %>% 
        tab_options(table.width=pct(80)) %>%
        cols_align(align='center', columns = everything()) %>%
        gt_theme_538() %>% 
        gt_theme_dark() 
    })
    
    output$p20toivspts <- renderPlotly({
      ggplotly(highlightpos() %>%
        ggplot(aes(x=!!as.name('avg TOI'), y=!!as.name('pts per20'), label=player)) +
        geom_point(color= "white", alpha= .75, size=2) +
        geom_point(data = mtl_dmen(), color = 'red', size=5) +
        geom_point(data = ax72(), shape= 23, size=5, fill= 'blue' )+
        theme(axis.text.x=element_text(angle = -90, vjust = 0.5, hjust=1))+
        labs(x= "TOI", y="Points per20", title= "Player Efficiency", caption = "data from HockeyReference.com") +
        dark_theme_bw() )
    })

    output$p20gva <- renderPlotly({
      ggplotly(highlightpos() %>%
        ggplot(aes(x=!!as.name('a per20'), y=!!as.name('g per20'), label=player)) +
        geom_point(color= "white", alpha= .75, size=2) +
        geom_point(data = mtl_dmen(), color = 'red', size=5) +
        geom_point(data = ax72(), shape= 23, size=5, fill= 'blue' )+
        labs(x= "Assists per20", y="Goals per20", title= "Player Efficiency", caption = "data from HockeyReference.com") +
        dark_theme_bw() )
    })

    #needs EV TOI!!
    output$evp20gva <- renderPlotly({
      ggplotly(highlightpos() %>%
        ggplot(aes(x=!!as.name('ev a per20'), y=!!as.name('ev g per20'), label=player)) +
        geom_point(color= "white", alpha= .75, size=2) +
        geom_point(data = mtl_dmen(), color = 'red', size=5) +
        geom_point(data = ax72(), shape= 23, size=5, fill= 'blue' )+
        labs(x= "ES Assists per20", y="ES Goals per20", title= "Even Strength Player Efficiency", caption = "data from HockeyReference.com") +
        dark_theme_bw())
    })

    output$'82gmpaceboxscore' <- render_gt({
      
      complist2() %>% 
        dplyr::select('Platform Year',player,'AGE (7/1)','GP','GP','g 82':'sog 82', 'avg TOI','blk 82','hits 82','Rslt Term','RsltCHpct') %>% 
        dplyr::arrange(desc(!!as.name('pts 82'))) %>% 
        gt() %>% 
        tab_options(table.width=pct(80)) %>%
        cols_align(align='center', columns = everything()) %>%
        gt_theme_538() %>% 
        gt_theme_dark() %>% 
        tab_source_note(source_note ="data from CapFriendly & HockeyReference")
    })
    
    
    output$'hl82gmpaceboxscore' <- render_gt({
      
      highlightplayer() %>% 
        dplyr::select('Platform Year',player,'AGE (7/1)','GP','GP','g 82':'sog 82', 'avg TOI','blk 82','hits 82','Rslt Term','RsltCHpct') %>% 
        dplyr::arrange(desc(!!as.name('pts 82'))) %>% 
        gt() %>% 
        tab_options(table.width=pct(80)) %>%
        cols_align(align='center', columns = everything()) %>%
        gt_theme_538() %>% 
        gt_theme_dark() 
    })
    
    output$'82gmpaceboxscoreev' <- render_gt({
      
      complist2() %>% 
        dplyr::select('Platform Year',player,'AGE (7/1)','GP','GP','EV g 82':'EV pts 82','Rslt Term','RsltCHpct') %>% 
        dplyr::arrange(desc(!!as.name('EV pts 82'))) %>% 
        gt() %>% 
        tab_options(table.width=pct(80)) %>%
        cols_align(align='center', columns = everything()) %>%
        gt_theme_538() %>% 
        gt_theme_dark() %>% 
        tab_source_note(source_note ="data from CapFriendly & HockeyReference")
    })
    
    output$'hl82gmpaceboxscoreev' <- render_gt({
      
      highlightplayer() %>% 
        dplyr::select('Platform Year',player,'AGE (7/1)','GP','GP','EV g 82':'EV pts 82','Rslt Term','RsltCHpct') %>% 
        dplyr::arrange(desc(!!as.name('EV pts 82'))) %>% 
        gt() %>% 
        tab_options(table.width=pct(80)) %>%
        cols_align(align='center', columns = everything()) %>%
        gt_theme_538() %>% 
        gt_theme_dark() 
    })
    
    #need to calculate gp pace
    output$pacegpvpts <- renderPlotly({
      ggplotly(highlightpos() %>%
        ggplot(aes(x=GP, y=!!as.name('pts 82'), label=player)) +
        geom_point(color= "white", alpha= .75, size=2) +
        geom_point(data = mtl_dmen(), color = 'red', size=5) +
        geom_point(data = ax72(), shape= 23, size=5, fill= 'blue' )+
        labs(x= "GP", y="Points 82gm", title= "Full Season Scoring Pace", caption = "data from HockeyReference.com") +
        dark_theme_bw())
    })

    #need to calculate gp pace
   output$pacegva <- renderPlotly({
     ggplotly(highlightpos() %>%
        ggplot(aes(x=!!as.name('a 82'), y=!!as.name('g 82'), label=player)) +
        geom_point(color= "white", alpha= .75, size=2) +
        geom_point(data = mtl_dmen(), color = 'red', size=5) +
        geom_point(data = ax72(), shape=23, size=5, fill= 'blue' )+
        labs(x= "Assists 82gm", y="Goals 82gm", title= "Full Season Scoring Pace", caption = "data from HockeyReference.com") +
        dark_theme_bw())
    })

   #need to calculate gp pace
   output$evpacegva <- renderPlotly({
     ggplotly(highlightpos() %>%
       ggplot(aes(x=!!as.name('EV a 82'), y=!!as.name('EV g 82'), label=player)) +
       geom_point(color= "white", alpha= .75, size=2) +
       geom_point(data = mtl_dmen(), color = 'red', size=5) +
       geom_point(data = ax72(), shape=23, size=5, fill= 'blue' )+
       labs(x= "ES Assists 82gm", y="ES Goals 82gm", title= "Full Season Even Strength Scoring Pace", caption = "data from HockeyReference.com") +
       dark_theme_bw())
   })

    # join to skater_stats on name and stats year- 2nd CF table w resulting term & AAV (yr +1)
  
   
    capnames <- reactive({
      
      
      
      RsltCH <- gsub("[\\$,]", "", vars$RsltCH)
      vars$RsltCH <- as.numeric(RsltCH)
      
      RsltCHpct <- gsub("%$","",vars$RsltCHpct)
      vars$RsltCHpct <- as.numeric(RsltCHpct)
      
      vars2 <- vars %>% 
        dplyr::filter(fa_ind == "T")
      
      vars3 <- merge(vars2,skater_stats_master,by.x="PlayerSeason",by.y="PlayerSeason")
      
      return(vars3)
      
    })
    
    output$fabios <- render_gt({
      
      complist2() %>% 
        dplyr::mutate(Player=PLAYER,
                      'Platform Year'=season.y,
                      'Age on 7/1'=AGE,
                      DOB=DATE.OF.BIRTH,
                      'Birth Country'=COUNTRY,
                      'Draft Details'=DRAFTED,
                      Shoots=HANDED
                      ) %>% 
        dplyr::select('Platform Year',Player,'Age on 7/1', DOB, 'Birth Country', HEIGHT, WEIGHT, Shoots, POS, 'Draft Details') %>% 
        gt()%>% 
        cols_align(align='center', columns = everything()) %>% 
        tab_options(table.width=pct(80)) %>%
        gt_theme_538() %>% 
        gt_theme_dark() %>% 
        tab_source_note(source_note ="data from CapFriendly & HockeyReference")
      
    })    
    
    output$hlbios <- render_gt({
      
      highlightplayer() %>% 
        dplyr::mutate(Player=PLAYER,
                      'Platform Year'=season.y,
                      'Age on 7/1'=AGE,
                      DOB=DATE.OF.BIRTH,
                      'Birth Country'=COUNTRY,
                      'Draft Details'=DRAFTED,
                      Shoots=HANDED
        ) %>% 
        dplyr::select('Platform Year',Player,'Age on 7/1', DOB, 'Birth Country', HEIGHT, WEIGHT, Shoots, POS, 'Draft Details') %>% 
        gt()%>% 
        cols_align(align='center', columns = everything()) %>% 
        tab_options(table.width=pct(80)) %>%
        gt_theme_538() %>% 
        gt_theme_dark() %>% 
        tab_header(title="Player Bio & Information")
    })
    
    output$facontracts <- render_gt({
      
      capnames2() %>% 
        dplyr::mutate(Player=PLAYER,
                      Length=LENGTH,
                      'Platform Year'=season.y,
                      'Expiry Age (7/1)'=AGE,
                      'FA Type'= EXPIRY,
                      'Rslt Term'=RsltTerm,
                      'Rslt Cap Hit%'=RsltCHpct) %>% 
        dplyr::select('Platform Year', Player, position, 'Expiry Age (7/1)', 
                      'FA Type', 'GP', 'G', 'A', 'Pts', 'Rslt Term','Rslt Cap Hit%' 
                      ) %>% 
        gt()%>% 
        cols_align(align='center', columns = everything()) %>% 
        tab_options(table.width=pct(80)) %>%
        gt_theme_538() %>% 
        gt_theme_dark() %>%
        #gt_color_rows('PTS') %>% 
        tab_header(title="Resulting Cap Hits") %>% 
        tab_source_note(source_note ="Contract data from CapFriendly")
      
    })
    #need to make reactive w number of comps
    
    output$exlist <- renderUI({f7List(
      mode = "media",
      f7ListItem(
        title = highlightplayer()$player[1]
        ,
        subtitle =paste0("Pos: ",highlightplayer()$POS[1]," | ",
                         highlightplayer()$'Platform Year'[1]," ",highlightplayer()$EXPIRY[1]),
        
        HTML(paste0("GP: ",highlightplayer()$GP[1],"  ",
               "G: ",highlightplayer()$G[1],"  ",
               "A: ",highlightplayer()$A[1],"  ",
               "Pts: ",highlightplayer()$Pts[1],br(),
               "Rslt Term: ",highlightplayer()$'Rslt Term'[1], " ",
               "Rslt CH%: ",highlightplayer()$'Rslt CH%'[1])),
        style = "height: 120px;"
        
      )
    )
    })
    
    output$exlist2 <- renderUI({f7List(
      mode = "media",
      f7ListItem(
        title = highlightcomps()$player[1]
        ,
        subtitle =paste0("Pos: ",highlightcomps()$POS[1]," | ",
                         highlightcomps()$'Platform Year'[1]," ",highlightcomps()$EXPIRY[1]),
        
        HTML(paste0("GP: ",highlightcomps()$GP[1],"  ",
               "G: ",highlightcomps()$G[1],"  ",
               "A: ",highlightcomps()$A[1],"  ",
               "Pts: ",highlightcomps()$Pts[1], br(),
               "Rslt Term: ",highlightcomps()$'Rslt Term'[1], " ",
               "Rslt CH%: ",highlightcomps()$'Rslt CH%'[1])),
        style = "height: 120px;"
        
      )
    )
    })
    
    output$exlist3 <- renderUI({f7List(
      mode = "media",
      f7ListItem(
        title = highlightcomps()$player[2]
        ,
        subtitle =paste0("Pos: ",highlightcomps()$POS[2]," | ",
                         highlightcomps()$'Platform Year'[2]," ",highlightcomps()$EXPIRY[2]),
        
        HTML(paste0("GP: ",highlightcomps()$GP[2],"  ",
               "G: ",highlightcomps()$G[2],"  ",
               "A: ",highlightcomps()$A[2],"  ",
               "Pts: ",highlightcomps()$Pts[2],br(),
               "Rslt Term: ",highlightcomps()$'Rslt Term'[2], " ",
               "Rslt CH%: ",highlightcomps()$'Rslt CH%'[2])),
        style = "height: 120px;"
        
      )
    )
    })
  
    output$exlist4 <- renderUI({f7List(
      mode = "media",
      f7ListItem(
        title = highlightcomps()$player[3]
        ,
        subtitle =paste0("Pos: ",highlightcomps()$POS[3]," | ",
                         highlightcomps()$'Platform Year'[3]," ",highlightcomps()$EXPIRY[3]),
        
        HTML(paste0("GP: ",highlightcomps()$GP[3],"  ",
               "G: ",highlightcomps()$G[3],"  ",
               "A: ",highlightcomps()$A[3],"  ",
               "Pts: ",highlightcomps()$Pts[3],br(),
               "Rslt Term: ",highlightcomps()$'Rslt Term'[3], " ",
               "Rslt CH%: ",highlightcomps()$'Rslt CH%'[3])),
        style = "height: 120px;"
        
      )
    )
    })
    
    output$exlist5 <- renderUI({f7List(
      mode = "media",
      f7ListItem(
        title = highlightcomps()$player[4]
        ,
        subtitle =paste0("Pos: ",highlightcomps()$POS[4]," | ",
                         highlightcomps()$'Platform Year'[4]," ",highlightcomps()$EXPIRY[4]),
        
        HTML(paste0("GP: ",highlightcomps()$GP[4],"  ",
               "G: ",highlightcomps()$G[4],"  ",
               "A: ",highlightcomps()$A[4],"  ",
               "Pts: ",highlightcomps()$Pts[4],br(),
               "Rslt Term: ",highlightcomps()$'Rslt Term'[4], " ",
               "Rslt CH%: ",highlightcomps()$'Rslt CH%'[4])),
        style = "height: 120px;"
        
      )
    )
    })
    
    output$exlist6 <- renderUI({f7List(
      mode = "media",
      f7ListItem(
        title = highlightcomps()$player[5]
        ,
        subtitle =paste0("Pos: ",highlightcomps()$POS[5]," | ",
                         highlightcomps()$'Platform Year'[5]," ",highlightcomps()$EXPIRY[5]),
        
        HTML(paste0("GP: ",highlightcomps()$GP[5],"  ",
               "G: ",highlightcomps()$G[5],"  ",
               "A: ",highlightcomps()$A[5],"  ",
               "Pts: ",highlightcomps()$Pts[5],br(),
               "Rslt Term: ",highlightcomps()$'Rslt Term'[5], " ",
               "Rslt CH%: ",highlightcomps()$'Rslt CH%'[5])),
        style = "height: 120px;"
        
      )
    )
    })
  
    output$exlist7 <- renderUI({f7List(
      mode = "media",
      f7ListItem(
        title = highlightcomps()$player[6]
        ,
        subtitle =paste0("Pos: ",highlightcomps()$POS[6]," | ",
                         highlightcomps()$'Platform Year'[6]," ",highlightcomps()$EXPIRY[6]),
        
        HTML(paste0("GP: ",highlightcomps()$GP[6],"  ",
               "G: ",highlightcomps()$G[6],"  ",
               "A: ",highlightcomps()$A[6],"  ",
               "Pts: ",highlightcomps()$Pts[6],br(),
               "Rslt Term: ",highlightcomps()$'Rslt Term'[6], " ",
               "Rslt CH%: ",highlightcomps()$'Rslt CH%'[6])),
        style = "height: 120px;"
        
      )
    )
    })  
      
    output$capbutton <-   renderUI({
      
      f7Button(inputId = "test",
               label=paste0("$",(10000*input$Cap)*(input$CHpct)),
               color="red")
      
    })  
    
    # send the theme to javascript
    observe({
      session$sendCustomMessage(
        type = "ui-tweak",
        message = list(os = input$theme, skin = input$color)
      )
    })
    
    
    
    
  }
)
