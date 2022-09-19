# Function to test if x is na or null
# Overcomes logical(0) errors when using is.na
# Shared by Backlin 29-Oct-2013
# https://stackoverflow.com/questions/19655579/a-function-that-returns-true-on-na-null-nan-in-r

is.blank <- function(x, false.triggers=FALSE){
    if(is.function(x)) return(FALSE) # Some of the tests below trigger
                                     # warnings when used on functions
    return(
        is.null(x) ||                # Actually this line is unnecessary since
        length(x) == 0 ||            # length(NULL) = 0, but I like to be clear
        all(is.na(x)) ||
        all(x=="") ||
        (false.triggers && all(!x))
    )
}