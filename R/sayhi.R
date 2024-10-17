sayhi <- function(a){
    response <- readline(prompt = "Would you like to say hello? (Yes/No): ")
    if (tolower(response) == "yes"){
        print("Hello World")
    }else if (tolower(response) == "no"){
        cat("Continuing without saying hello.\n")
    }else{
        cat("Invalid response. Please answer Yes or No.\n")
    }
}
