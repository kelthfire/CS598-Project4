get_user_ratings = function(value_list) {
  dat = data.table(MovieID = sapply(strsplit(names(value_list), "_"), 
                                    function(x) ifelse(length(x) > 1, x[[2]], NA)),
                   Rating = unlist(as.character(value_list)))
  dat = dat[!is.null(Rating) & !is.na(MovieID)]
  dat[Rating == " ", Rating := 0]
  dat[, ':=' (MovieID = as.numeric(MovieID), Rating = as.numeric(Rating))]
  dat = dat[Rating > 0]
}

## Load the ratings data
myurl = "https://liangfgithub.github.io/MovieData/"
ratings = read.csv(paste0(myurl, 'ratings.dat?raw=true'), 
                   sep = ':',
                   colClasses = c('integer', 'NULL'), 
                   header = FALSE)
colnames(ratings) = c('UserID', 'MovieID', 'Rating', 'Timestamp')


# Convert to Real Ratings Matrix
i = paste0('u', ratings$UserID)
j = paste0('m', ratings$MovieID)
x = ratings$Rating
tmp = data.frame(i, j, x, stringsAsFactors = T)
Rmat = sparseMatrix(as.integer(tmp$i), as.integer(tmp$j), x = tmp$x)
rownames(Rmat) = levels(tmp$i)
colnames(Rmat) = levels(tmp$j)
Rmat = new('realRatingMatrix', data = Rmat)

# Get the recommender functions

# UBCF
rec_UBCF = Recommender(Rmat, method = 'UBCF',
                       parameter = list(normalize = 'Z-score', 
                                        method = 'Cosine', 
                                        nn = 25))

# Popular
rec_popular = Recommender(Rmat, method = 'POPULAR')





# read in data for loading movie info and images
myurl = "https://liangfgithub.github.io/MovieData/"
movies = readLines(paste0(myurl, 'movies.dat?raw=true'))
movies = strsplit(movies, split = "::", fixed = TRUE, useBytes = TRUE)
movies = matrix(unlist(movies), ncol = 3, byrow = TRUE)
movies = data.frame(movies, stringsAsFactors = FALSE)
colnames(movies) = c('MovieID', 'Title', 'Genres')
movies$MovieID = as.integer(movies$MovieID)
movies$Title = iconv(movies$Title, "latin1", "UTF-8")

small_image_url = "https://liangfgithub.github.io/MovieImages/"
movies$image_url = sapply(movies$MovieID, 
                          function(x) paste0(small_image_url, x, '.jpg?raw=true'))

# read in data for loading genre recommendations
baseurl = "https://kelthfire.github.io/CS598-Project4/"
top_in_genre = read.csv(paste0(baseurl,"top_in_genre.csv?raw=true"),row.names = 1)



genre_list = c("Action", "Adventure", "Animation", 
               "Children's", "Comedy", "Crime",
               "Documentary", "Drama", "Fantasy",
               "Film-Noir", "Horror", "Musical", 
               "Mystery", "Romance", "Sci-Fi", 
               "Thriller", "War", "Western")



shinyServer(function(input, output, session) {
  
  
  
  # show the books to be rated
  output$ratings <- renderUI({
    num_rows <- 20
    num_movies <- 6 # movies per row
    
    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        list(box(width = 2,
                 div(style = "text-align:center", img(src = movies$image_url[(i - 1) * num_movies + j], height = 150)),
                 #div(style = "text-align:center; color: #999999; font-size: 80%", books$authors[(i - 1) * num_books + j]),
                 div(style = "text-align:center", strong(movies$Title[(i - 1) * num_movies + j])),
                 div(style = "text-align:center; font-size: 150%; color: #f0ad4e;", ratingInput(paste0("select_", movies$MovieID[(i - 1) * num_movies + j]), label = "", dataStop = 5)))) #00c0ef
      })))
    })
  })
  
  

  # Calculate recommendations when the sbumbutton is clicked
  dfGenre <- eventReactive(input$btnGenre, {
    withBusyIndicatorServer("btnGenre", { # showing the busy indicator
      # hide the rating container
      useShinyjs()
      jsCode <- "document.querySelector('[data-widget=collapse]').click();"
      runjs(jsCode)
      
      # Get Genre
      selectedGenre = input$genreDropdown
      gi = which(genre_list==selectedGenre)
      
      user_predicted_ids = matrix(0,10)
      for(mi in 1:10){
        user_predicted_ids[mi] = which(movies$MovieID == top_in_genre[mi,gi])
      }
      
      recom_results_genre <- data.table(Rank = 1:10, 
                                  MovieID = movies$MovieID[user_predicted_ids], 
                                  Title = movies$Title[user_predicted_ids],
                                  URL = movies$image_url[user_predicted_ids])
      
    }) # still busy
    
  }) # clicked on button
  
  
  # display the recommendations
  output$resultsGenre <- renderUI({
    num_rows <- 2
    num_movies <- 5
    recom_result_genre <- dfGenre()
    
    
    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        box(width = 2, status = "success", solidHeader = TRUE, title = paste0("Rank ", (i - 1) * num_movies + j),
            
            div(style = "text-align:center", 
                a(img(src = recom_result_genre$URL[(i - 1) * num_movies + j], height = 150))
            ),
            div(style="text-align:center; font-size: 100%", 
                strong(recom_result_genre$Title[(i - 1) * num_movies + j])
            )
            
        )        
      }))) # columns
    }) # rows
    
  }) # renderUI function
  
    
  # Calculate recommendations when the sbumbutton is clicked
  df <- eventReactive(input$btn, {
    withBusyIndicatorServer("btn", { # showing the busy indicator
      # hide the rating container
      useShinyjs()
      jsCode <- "document.querySelector('[data-widget=collapse]').click();"
      runjs(jsCode)
      
      # get the user's rating data
      value_list <- reactiveValuesToList(input)
      user_ratings <- get_user_ratings(value_list)
      
      
      # Create a new user
      nm = length(unique(ratings$MovieID))
      nur = nrow(user_ratings)
      
      
      i = c(rep("u88888",nm),rep("u99999",nur))
      j = c(paste0("m",unique(ratings$MovieID)),paste0("m", user_ratings$MovieID))
      x = c(rep(5,nm),user_ratings$Rating)
      tmp = data.frame(i, j, x, stringsAsFactors = T)
      newUser = sparseMatrix(as.integer(tmp$i), as.integer(tmp$j), x = tmp$x)
      rownames(newUser) = levels(tmp$i)
      colnames(newUser) = levels(tmp$j)
      newUser = new('realRatingMatrix', data = newUser)
      
      
      recomUBCF = predict(rec_UBCF, 
                      newUser[2,], type = 'ratings')
      
      
      recomPOP = predict(rec_popular, 
                       newUser[2,], type = 'ratings')
      
      matUBCF = as(recomUBCF, 'matrix')
      matPOP = as(recomPOP, 'matrix')
      final = matrix(data=NA,nrow = 1, ncol = ncol(recomUBCF))
      
      
      for (it in 1:length(matUBCF)){
        
        final[1,it] = ifelse(is.na(matUBCF[1,it]), ifelse(is.na(matPOP[1,it]), 2.5, matPOP[1,it]), matUBCF[1,it])
        
      }
      
      colnames(final) = colnames(matUBCF)
      
      # Remove already rated items
      final[1,paste0("m", user_ratings$MovieID)] = 0
      
      top10scores = order(final,decreasing=TRUE)[1:10]
      
      user_predicted_ids = as.integer(gsub("m","",colnames(final)[top10scores]))
      recom_results <- data.table(Rank = 1:10, 
                                  MovieID = movies$MovieID[user_predicted_ids], 
                                  Title = movies$Title[user_predicted_ids], 
                                  URL = movies$image_url[user_predicted_ids])
      
      
    }) # still busy
    
  }) # clicked on button
  
  
  # display the recommendations
  output$results <- renderUI({
    num_rows <- 2
    num_movies <- 5
    recom_result <- df()
    
    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        box(width = 2, status = "success", solidHeader = TRUE, title = paste0("Rank ", (i - 1) * num_movies + j),
            
            div(style = "text-align:center", 
                a(img(src = recom_result$URL[(i - 1) * num_movies + j], height = 150))
            ),
            div(style="text-align:center; font-size: 100%", 
                strong(recom_result$Title[(i - 1) * num_movies + j])
            )
            
        )        
      }))) # columns
    }) # rows
    
  }) # renderUI function
  
}) # server function