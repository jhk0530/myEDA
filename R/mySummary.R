#' @export
#' @import ggplot2 dplyr reshape2
#'
#'

mySummary <- function(v){
  if(is.factor(v)){
    print( table(v) )
    v <- table(v)
    v <- v %>% reshape2::melt() # 데이터를 옆에서 아래로 바꾼다
    colnames(v) <- c('value','count') #

    return(
      ggplot2::ggplot(v, aes(x = value, y = count, fill = value)) +
        geom_bar(stat = 'identity')
    )
  }

  if(is.numeric(v)){

    print(summary(v))
    v <- table(v)

    v <- v %>% reshape2::melt()

    colnames(v) <- c('value','count')
    return(
      ggplot2::ggplot(v , aes(x = value)) +
            geom_histogram(
              binwidth = 1,
              fill = '#192a56',
              color = '#f5f6fa')
    )

  }

}

