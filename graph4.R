econDV2::Object(position100)
position100$source[[1]] <- "https://www.ris.gov.tw/app/portal/346"

sf_taiwan_simplified$台灣本島[[2]] |>
  mp$sf$make_background_map()

position100$background[[1]] <- function(){
  sf_taiwan_simplified$台灣本島[[2]] |>
    sf::st_cast("MULTIPOLYGON") |>
    mp$sf$make_background_map(
      color="white",
      size=0.1 #input$s
    )
}

#position100$background[[1]]()

position100$ecodata <- read_excel("C:/Users/user/Desktop/100+2.xlsx")

position100$inner_data <- {
  sf_taiwan_simplified$台灣本島[[2]] |> 
    dplyr::inner_join(
      position100$ecodata , by=c("map_id"="map_id")
    )
}

position100$inner_data <- position100$inner_data[,-c(4,5)]

#View(position100$inner_data)

position100$cut_inner_data <- position100$inner_data
position100$cut_inner_data$人數 |> cut(c(0,20,50,75,100,161),ordered_result = T) -> .cut
levels(.cut) <- c("0-20","20-50","50-75","75-100","100-161")
position100$cut_inner_data$人數 <- .cut


position100$plot[[1]] <- function(){
  position100$background[[1]]()+geom_sf(
    data=position100$cut_inner_data,
    mapping = aes(
      fill=人數,
      label=map_id
    ),
    size=0.1,
    color="white"
  )+scale_fill_brewer(type="seq")
}

position100$plot$final <- position100$plot[[1]]()+theme_void()+labs(
  title="109年底100歲以上人口分佈",
  caption="Source:https://www.ris.gov.tw/app/portal/346",
  x="",
  y="" 
)+theme(
  plot.caption.position = "plot"
)

View(position100$plot$final)

plotly::ggplotly(position100$plot$final)


