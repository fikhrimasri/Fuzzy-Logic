kompetensi.low <- function(x){
  ifelse(x < 50, 1,
         ifelse((x >= 50 & x <= 60), (60 - x)/10,
                0)) 
}

kompetensi.mid <- function(x){
  ifelse((x < 50 | x > 70), 0,
         ifelse((x >= 50 & x < 60), (x - 50)/10,
                (70 - x)/10)) 
}

kompetensi.high <- function(x){
  ifelse(x < 60, 0, 
         ifelse((x >= 60 & x < 70), (x - 60)/10,
                1)) 
}

kepribadian.low <- function(x){
  ifelse(x < 50, 1,
         ifelse((x >= 50 & x <= 60), (60 - x)/10,
                0)) 
}

kepribadian.mid <- function(x){
  ifelse((x < 50 | x > 70), 0,
         ifelse((x >= 50 & x < 60), (x - 50)/10,
                (70 - x)/10)) 
}

kepribadian.high <- function(x){
  ifelse(x < 60, 0, 
         ifelse((x >= 60 & x < 70), (x - 60)/10,
                1)) 
}

hitung.kompetensi <- function(column){
  list <- list("kol" = kompetensi.low(column), "kom" = kompetensi.mid(column),
               "koh" = kompetensi.high(column))
  return(list)
}

hitung.kepribadian <- function(column){
  list <- list("kel" = kepribadian.low(column), "kem" = kepribadian.mid(column),
               "keh" = kepribadian.high(column))
  return(list)
}

inference <- function(input){
  ya <- matrix(c(pmin(input$kol, input$keh), pmin(input$koh, input$kem), 
                       pmin(input$kom, input$keh), pmin(input$koh, input$keh)), 
                     ncol = 4)
  
  
  tidak <- matrix(c(pmin(input$kol, input$kel), pmin(input$kol, input$kem), 
                       pmin(input$kom, input$kel), pmin(input$kom, input$kem),
                       pmin(input$koh, input$kel)), 
                     ncol = 5)
  
  predicate.y <- c(rowMax(ya))
  predicate.n <- c(rowMax(tidak))
  
  return(list("predy" = predicate.y, "predn" = predicate.n))
}

# Defuzzyfication Sugeno Model
calc.acc <- function(output){
  acceptance <- 
    ((output$predy*100) + (output$predn*50))/
    (output$predy + output$predn)
  
  return(acceptance)
}

plot.kompetensi <- function(){
  data.kompetensi <- data.frame(
    Nilai <- kompetensi.low(seq(0, 100, by=0.1)),
    b <- kompetensi.mid(seq(0, 100, by=0.1)),
    c <- kompetensi.high(seq(0, 100, by=0.1)),
    competence <- seq(0, 100, by=0.1)
  )
  
  kompetensi.plot <- plot_ly(data.kompetensi, y = ~Nilai, x = ~competence, name = 'kompetensi low', type='scatter', mode = 'lines')%>%
    add_trace(y = ~b, name = 'kompetensi mid', mode = 'lines')%>%
    add_trace(y = ~c, name = 'kompetensi high', mode = 'lines')
  
  
  kompetensi.plot
}

plot.kepribadian <- function(){
  data.kepribadian <- data.frame(
    Nilai <- kepribadian.low(seq(0, 100, by=0.1)),
    b <- kepribadian.mid(seq(0, 100, by=0.1)),
    c <- kepribadian.high(seq(0, 100, by=0.1)),
    personality <- seq(0, 100, by=0.1)
  )
  
  kepribadian.plot <- plot_ly(data.kepribadian, y = ~Nilai, x = ~personality, name = 'kepribadian low', type='scatter', mode = 'lines')%>%
    add_trace(y = ~b, name = 'kepribadian mid', mode = 'lines')%>%
    add_trace(y = ~c, name = 'kepribadian high', mode = 'lines')
  
  
  kepribadian.plot
}

library(qlcMatrix)
library(plotly)

data <- read.csv("Dataset.csv")

kompetensi.column <- c(data$Kompetensi)
kepribadian.column <- c(data$Kepribadian)

fuzzy.kompetensi <- hitung.kompetensi(kompetensi.column)
fuzzy.kepribadian <- hitung.kepribadian(kepribadian.column)
# fuzzy.kompetensi
# fuzzy.kepribadian
fuzzy.input <- c(fuzzy.kompetensi, fuzzy.kepribadian)
fuzzy.output <- inference(fuzzy.input)

Hasil <- calc.acc(fuzzy.output)

result <- data.frame(
  kompetensi = kompetensi.column,
  kepribadian = kepribadian.column,
  Hasil = Hasil
)
# result[result$Acceptance > 65,]

result$diterima <- result$Hasil > 65
result$diterima[result$diterima == TRUE] <- "Ya"
result$diterima[result$diterima == FALSE] <- "Tidak"
result.diterima = result$diterima[21:30]
result
# result.diterima
# View(result)
write.csv(result.diterima,"Prediction.csv")

plot.kompetensi()
plot.kepribadian()
