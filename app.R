library(shiny)
library(dplyr)
library(readr)
library(ggplot2)
library(ggpubr)
library(plotly)
library(rsconnect)


ihsg<-as_tibble(read.csv("https://raw.githubusercontent.com/Danafr00/R_dashboard/main/JKSE.csv"))
covid <- as_tibble(read.csv("https://raw.githubusercontent.com/Danafr00/R_dashboard/main/covid-indo.csv"))

# change the type of column
ihsg$Date <- as.Date(ihsg$Date, "%m/%d/%Y")
ihsg$lab <- as.Date(ihsg$lab, "%m/%d/%Y")
ihsg$lab2 <- as.Date(ihsg$lab2, "%m/%d/%Y")
covid$Date <- as.Date(covid$Date, "%m/%d/%Y")

# left join cov in ihsg
df<-merge(x=ihsg,y=covid,by="Date",all.x=TRUE)

# Define UI for random distribution app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Hubungan Kasus Covid-19 di Indonesia dengan IHSG"),
    
    # Main panel for displaying outputs ----
    mainPanel(
      verticalLayout(tabPanel("t"), tabPanel("a")),
      # Output: Tabset w/ plot, summary, and table ----
      tabsetPanel(type = "tabs",
                  tabPanel("The Story",
                           p("Ini merupakan dashboard interactive tentang kasus Covid-19 di Indonesia dengan IHSG. Melalui dashboard ini, kita akan mencari tahu apakah covid-19 memiliki korelasi (bisa sebab-akibat atau tidak) terhadap pergerakan IHSG."),
                           p("Data yang digunakan untuk analisis adalah kasus covid-19 di Indonesia dan IHSG harian beserta volumenya dari tanggal 1 Januari 2020 hingga 15 Maret 2022, kita akan melihat antara korelasi dari data-data tersebut. Korelasi disini tidak berarti pasti terdapat hubungan sebab-akibat, tetapi melihat bagaimana kedua variabel tersebut dekat dengan variabel lainnya karena suatu perubahan."),
                           p("Pada dashboard ini, terdapat 2 hipotesis yang diuji, yaitu:"),
                           tags$ul(
                             tags$li("Berita kasus covid-19 baru masuk ke Indonesia (baik yang paling awal maupun varian baru) mempengaruhi IHSG."), 
                             tags$li("Berita gelombang 1,2, dan 3 kasus covid di Indonesia mempengaruhi IHSG."),
                             tags$li("Jumlah kasus harian ketika covid-19 baru masuk ke Indonesia (baik yang paling awal maupun varian baru) mempengaruhi IHSG."),
                             tags$li("Jumlah kasus harian pada gelombang 1,2, dan 3 kasus covid di Indonesia mempengaruhi IHSG.")),
                           p("Analisis yang dilakukan yaitu dengan eksplorasi data dan melihat trend kedua data pada setiap rentang waktu (tergantung kejadian yang terjadi) dan melakukan uji korelasi sederhana untuk melihat bagaimana kedua data berubah pada waktu tertentu."),
                           p("Asumsi yang diterapkan pada analisis ini yaitu sebagai berikut."), 
                           tags$ul(
                             tags$li("Untuk hipotesis pertama, berita terkait kasus baru bertahan hingga kurang lebih 2 minggu sejak kasus awal muncul."),
                             tags$li("Untuk hipotesis kedua, rentang waktu yang ditentukan diambil dari bulan kasus mulai naik hingga tanggal di mana kasus harian covid 19 di puncaknya.")),
                           p("Sekarang, kita coba untuk eksplor data yang ada dahulu.")),
                  tabPanel("Eksplor Data",
                           dateRangeInput(inputId = "date", label = "Date range",
                                          start = min(df$Date),
                                          end   = max(df$Date)),
                           plotlyOutput("cov"),
                           plotlyOutput("ihsg"),
                           plotlyOutput("volume"),
                           plotlyOutput("corr_cov")
                           ),
                  tabPanel("Awal Kasus Dunia", 
                           plotlyOutput("cov_1"),
                           br(),
                           p("Grafik di atas kosong karena pada Januar-Februari 2020 belum ada kasus covid di Indonesia."),
                           br(), 
                           plotlyOutput("ihsg_1"),
                           br(),
                           p("Meskipun belum ada kasus covid 19 yang terkonfirmasi di Indonesia, pergerakan IHSG mengalami penurunan yang cukup besar hingga 13%. Salah satu penyebabnya adalah kasus covid-19 yang sudah banyak menyebar di seluruh dunia."),
                           br(), 
                           plotlyOutput("corr_cov_1"), 
                           br(), 
                           p("Dikarenakan belum ada kasus terkonfirmasi di Indonesia, grafik korelasi di atas masih kosong.")),
                  tabPanel("Awal Masuk Indonesia", 
                           plotlyOutput("cov_2"), 
                           br(), 
                           p("Pada tanggal 2 Maret 2020 merupakan hari di mana kasus covid-19 pertama terkonfirmasi di Indonesia. Setelah tanggal tersebut, tidak terlihat kasus baru terkonfirmasi hingga tanggal 6 Maret yang mulai naik"),
                           br(),
                           plotlyOutput("ihsg_2"), 
                           br(), 
                           p("Dapat terlihat IHSG mengalami penurunan yang cukup signifikan dalam 2 minggu sejak kasus pertama terkonfirmasi, yakni sekitar 8%. Berita covid-19 sebagai virus baru mungkin menjadi salah satu penyebab turunnya iHSG."),
                           br(),
                           plotlyOutput("corr_cov_2"), 
                           br(), 
                           p("Kasus harian yang mulai naik diiringi dengan turunnya IHSG selama kurang lebih 2 minggu. Hal ini dapat dilihat keduanya cenderung memiliki korelasi negatif atauberbanding terbalik.")),
                  tabPanel("Gelombang 1 Covid-19", 
                           br(),
                           plotlyOutput("cov_3"), 
                           br(), 
                           p("Gelombang 1 covid-19 di Indonesia terjadi pada bulan November 2020 hingga Januari 2021. Hal ini dapat dilihat dari grafik kasus harian yang terus naik."),
                           br(),
                           plotlyOutput("ihsg_3"), 
                           br(), 
                           p("Uniknya, ketika gelombang 1 covid-19 terjadi, IHSG mengalami kenaikan dan mulai mengalami pemulihan. Salah satu alasannya adalah informasi terkait covid-19 yang mulai tersebar beserta penaganannya"),
                           br(),
                           plotlyOutput("corr_cov_3"), 
                           br(), 
                           p("Berdasarkan kenaikan kasus harian diiringi dengan kecenderungan IHSG untuk naik, ini membuat keduanya memiliki korelasi yang positif.")),
                  tabPanel("Awal Masuk Delta", 
                           plotlyOutput("cov_4"), 
                           br(), 
                           p("Pada tanggal 3 Mei 2021 merupakan hari di mana kasus covid-19 varian Delta pertama terkonfirmasi di Indonesia. Kasus harian pada minggu pertama sejak varian Delta sempat naik dan kemudian turun di minggu kedua"),
                           br(),
                           plotlyOutput("ihsg_4"), 
                           br(), 
                           p("Berita masuknya covid-19 varian Delta membuat investor berhati-hati. Selama 2 minggu pertama, IHSG cenderung turun sekitar 2% walaupun tidak sebanyak ketika kasus pertama terkonfirmasi."),
                           br(),
                           plotlyOutput("corr_cov_4"), 
                           br(), 
                           p("Hubungan antara kasus harian covid-19 dengan IHSG cenderung berkorelasi positif karena memiliki pergerakan yang tidak jauh berbeda pada rentang waktu tersebut.")),
                  tabPanel("Gelombang 2 Covid-19", 
                           plotlyOutput("cov_5"), 
                           br(),
                           p("Gelombang 2 covid-19 di Indonesia terjadi pada bulan pertengahan Mei hingga Juni 2021. Hal ini dapat dilihat dari grafik kasus harian yang terus naik yang puncaknya di akhir Juni 2021."),
                           br(),
                           plotlyOutput("ihsg_5"), 
                           br(), 
                           p("Lagi-lagi, ketika gelombang 2 covid-19 terjadi, IHSG cenderung mengalami kenaikan selama rentang waktu gelombang 2 terjadi."),
                           br(),
                           plotlyOutput("corr_cov_5"), 
                           br(), 
                           p("Dikarenakan kenaikan kasus harian diiringi dengan kecenderungan IHSG yang ikut naik, keduanya pada rentang waktu ini memiliki korelasi positif.")),
                  tabPanel("Awal Masuk Omicron", 
                           plotlyOutput("cov_6"), 
                           br(), 
                           p("Pada tanggal 15 Desember 2021 merupakan hari di mana kasus covid-19 varian Omicron masuk ke Indonesia. Kasus harian covid-19 pada tanggal tersebut cenderung stagnan."),
                           br(),
                           plotlyOutput("ihsg_6"), 
                           br(),
                           p("Berita masuknya covid-19 varian Omicron sempat membuat IHSG turun dan kembali naik. Secara keseluruhan, pada 2 minggu pertama sejak kasus terkonfirmasi, IHSG cenderung mengalami penurunan walaupun sedikit. Penurunan ini lebih rendah dibandingkan varian Delta."),
                           br(), 
                           plotlyOutput("corr_cov_6"), 
                           br(), 
                           p("Hubungan antara kasus harian covid-19 dengan IHSG cenderung berkorelasi positif karena memiliki pergerakan yang tidak jauh berbeda dan stagnan pada rentang waktu tersebut.")),
                  tabPanel("Gelombang 3 Covid-19", 
                           plotlyOutput("cov_7"), 
                           br(), 
                           p("Gelombang 3 covid-19 di Indonesia terjadi pada pertengahan bulan Januari 2022 dan puncaknya pada pertengahan Februari 2022. Hal ini dapat dilihat berdasarkan grafik yang terus naik hingga sekitar 18 Februari 2022."),
                           br(),
                           plotlyOutput("ihsg_7"), 
                           br(), 
                           p("Ketika gelombang 3 covid-19 terjadi, IHSG masih cenderung mengalami kenaikan selama rentang waktu gelombang 3. Hal ini cukup menarik karena secara keseluruhan, IHSG justru naik ketika terjadi gelombang covid-19."),
                           br(),
                           plotlyOutput("corr_cov_7"), 
                           br(), 
                           p("Berdasarkan pergerakan kedua grafik sebelumnya, pada rentang waktu gelombang 3 covid-19, jumlah kasus harian covid-19 dan IHSG memiliki korelasi positif.")),
                  tabPanel("Kesimpulan", 
                           p("Berdasarkan beberapa visualisasi dan penjelasan pada tab sebelumnya, dapat disimpulkan sebagai berikut."),
                           tags$ul(
                             tags$li("Berita kasus covid-19 baru masuk ke Indonesia cukup membuat IHSG cenderung mengalami penurunan (berdasarkan 3 kejadian covid-19 yang masuk ke Indonesia)."), 
                             tags$li("Pada saat Berita gelombang 1,2, dan 3 kasus covid di Indonesia, IHSG cenderung mengalami kenaikan."),
                             tags$li("Tidak ada korelasi yang pasti terkait jumlah kasus harian ketika covid-19 baru masuk ke Indonesia dengan IHSG."),
                             tags$li("Terdapat korelasi positif terkait jumlah kasus harian pada gelombang 1,2, dan 3 kasus covid di Indonesia dengan iHSG.")),
                           p("Perlu diingat, bahwa dashboard ini hanya menyajikan korelasi antara 2 variabel, yaitu kasus harian covid-19 dan IHSG. Korelasi tersebut tidak pasti bahwa keduanya memiliki sebab-akibat. Banyak kemungkinan hubungan kausalitas yang terjadi dan kompleks di suatu pasar saham. Oleh karena itu perlu ditekankan disini makna korelasi pada dashboard ini, yang tidak lain adalah kekuatan dan arah hubungan linier dari dua veriabel."),
                           p("Berdasarkan analisis yang diberikan, ada beberapa hal yang dapat digaris bawahi dalam mengambil keputusan yakni sebagai berikut."),
                           tags$ul(
                             tags$li("Ketika berita kasus covid-19 ataupun virus baru muncul, investor perlu berhati-hati karena pada saat itu IHSG cenderung mengalami penurunan."),
                             tags$li("Ketika terjadi suatu gelombang pandemi suatu virus, perlu diperhatikan beberapa hal karena pada kondisi saat itu, IHSG belum tentu mengalami penurunan.")
                             )
                           )
      )
    )
  )

# Define server logic for random distribution app ----
server <- function(input, output) {

  plot_line_ihsg <- function (tgl1, tgl2){
    df %>% subset(Date>=tgl1 & Date<tgl2) %>%
      ggplot(aes(x = Date, y = Close)) +
      geom_line() +
      geom_point(aes(size = Volume / 1e9), alpha = 0.5) +
      scale_y_log10() +
      scale_x_date(date_labels = "%b %Y") +
      scale_size_area() +
      labs(title = "Pergerakan IHSG", x="Tanggal", y="IHSG") +
      theme(plot.title = element_text(hjust = 0.5, vjust = -1))
  }
  
  plot_line_cov <- function (tgl1, tgl2){
    df %>% subset(Date>=tgl1 & Date<tgl2) %>%
      ggplot(aes(x = Date, y = new_cases)) +
      geom_line() +
      scale_y_log10() +
      scale_x_date(date_labels = "%b %Y") +
      labs(title = "Pergerakan Kasus Harian Covid-19 di Indonesia", x="Tanggal", y="Jumlah Kasus Harian") +
      theme(plot.title = element_text(hjust = 0.5, vjust = -1))
  }
  
  plot_cor_cov <- function(tgl1, tgl2){
    df %>%
      subset(Date>=tgl1 & Date<tgl2) %>%
      mutate(direction = ifelse(Close > Open, "Bullish", "Bearish"))%>%
      ggscatter(x = "new_cases", y = "Close", add = "reg.line") +
      geom_path(colour = "grey") +
      geom_point(aes(colour = direction), alpha = 0.5) +
      geom_text(aes(label=lab), hjust = 0, size = 3, nudge_x = 0.015) +
      labs(title = "Korelasi Kasus Harian Covid-19 dengan IHSG", x="Jumlah Kasus Harian", y="IHSG") +
      theme(plot.title = element_text(hjust = 0.5, vjust = -1),
            legend.position = "bottom")
  }
  
  
  output$cov <- renderPlotly({
    data <- plot_line_cov(input$date[1], input$date[2])})
  
  output$ihsg <- renderPlotly({
    data <- df %>% subset(Date>=input$date[1] & Date<input$date[2]) %>%
      ggplot(aes(x = Date, y = Close)) +
      geom_line() +
      scale_y_log10() +
      scale_x_date(date_labels = "%b %Y") +
      scale_size_area() +
      labs(title = "Pergerakan IHSG", x="Tanggal", y="IHSG") +
      theme(plot.title = element_text(hjust = 0.5, vjust = -1))
    })
  
  output$volume <-renderPlotly({
    data <- ggplotly(
        ggplot(df %>% subset(Date>=input$date[1] & Date<input$date[2])) +
        geom_col(aes(x=Date, y=Volume, fill=jenis)) +
        labs(x="Tanggal", y="Volume", title="Volume Transaksi Harian IHSG") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1),  # rotate x axis text
            plot.title = element_text(hjust = 0.5, vjust = -1),
            legend.position = "bottom") +
        scale_x_date(date_breaks =  '1 months', 
                   labels = scales::date_format("%d-%b-%y"))
      ) %>% layout(legend = list(orientation = "h", y=-0.4))
    })
  
  output$corr_cov <- renderPlotly({
    data <- plot_cor_cov(input$date[1], input$date[2])})
  
  output$cov_1 <- renderPlotly({
    data <- plot_line_cov("2020-01-01", "2020-03-01")})
  
  output$ihsg_1 <- renderPlotly({
    data <- plot_line_ihsg("2020-01-01", "2020-03-01")})
  
  output$corr_cov_1 <- renderPlotly({
    data <- plot_cor_cov("2020-01-01", "2020-03-01")})
  
  
  output$cov_2 <- renderPlotly({
    data <- plot_line_cov("2020-03-01", "2020-03-16")})
  
  output$ihsg_2 <- renderPlotly({
    data <- plot_line_ihsg("2020-03-01", "2020-03-16")})
  
  output$corr_cov_2 <- renderPlotly({
    data <- plot_cor_cov("2020-03-01", "2020-03-16")})
  
  
  output$cov_3 <- renderPlotly({
    data <- plot_line_cov("2020-11-01", "2021-02-01")})
  
  output$ihsg_3 <- renderPlotly({
    data <- plot_line_ihsg("2020-11-01", "2021-02-01")})
  
  output$corr_cov_3 <- renderPlotly({
    data <- plot_cor_cov("2020-11-01", "2021-02-01")})
  
  
  output$cov_4 <- renderPlotly({
    data <- plot_line_cov("2021-05-01", "2021-05-18")})
  
  output$ihsg_4 <- renderPlotly({
    data <- plot_line_ihsg("2021-05-01", "2021-05-18")})
  
  output$corr_cov_4 <- renderPlotly({
    data <- plot_cor_cov("2021-05-01", "2021-05-18")})
  
  
  output$cov_5 <- renderPlotly({
    data <- plot_line_cov("2021-05-18", "2021-07-01")})
  
  output$ihsg_5 <- renderPlotly({
    data <- plot_line_ihsg("2021-05-18", "2021-07-01")})
  
  output$corr_cov_5 <- renderPlotly({
    data <- plot_cor_cov("2021-05-18", "2021-07-01")})
  
  
  output$cov_6 <- renderPlotly({
    data <- plot_line_cov("2021-12-15", "2022-01-01")})
  
  output$ihsg_6 <- renderPlotly({
    data <- plot_line_ihsg("2021-12-15", "2022-01-01")})
  
  output$corr_cov_6 <- renderPlotly({
    data <- plot_cor_cov("2021-12-15", "2022-01-01")})
  
  
  output$cov_7 <- renderPlotly({
    data <- plot_line_cov("2022-01-20", "2022-02-20")})
  
  output$ihsg_7 <- renderPlotly({
    data <- plot_line_ihsg("2022-01-20", "2022-02-20")})
  
  output$corr_cov_7 <- renderPlotly({
    data <- plot_cor_cov("2022-01-20", "2022-02-20")})
 
  
}

## Masukkan parameter dari fungsi shinyApp
shinyApp(ui, server)
