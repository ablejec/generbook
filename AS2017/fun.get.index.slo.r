#added Sept2011	
#support function to generate the correct order in the index of authors
#generates the {\index{Last Name, Initials}}  string
#if any of the characters listed in the my.char argument are present, it changes them in my.new.char
#this function is useful if some characters that are not correctly alphabetically sorted by LaTeX are present in the names of the authors
# example  \index{Zzizzek@?i?ek, S} example	for my.char="?" and my.new.char="Zz"

#2012, added some turkish characters "?"
.fun.get.index.slo <- function (my.lastname, my.initials, my.char = c("Ĺˇ", "Ĺľ", "ÄŤ",
    "Ä‡", "Ĺ ", "Ĺ˝", "ÄŚ", "Ä†"), my.new.char = c("szz", "zzz",
    "czz", "czzz", "SZZ", "ZZZ", "CZZ", "CZZZ"))
{
    my.splitted <- unlist(strsplit(as.character(my.lastname),
        split = NULL))
    my.char.present <- my.char %in% my.splitted
    if (any(my.char.present)) {
        for (ii in which(my.char.present)) {
            my.splitted[which(my.splitted %in% my.char[ii])] <- my.new.char[ii]
        }
        my.splitted <- sapply(list(my.splitted), paste, collapse = "")
        my.index <- paste("\\index{", my.splitted, "@", my.lastname,
            ", ", my.initials, "}", sep = "")
    }
    else my.index <- paste("\\index{", my.lastname, ", ", my.initials,
        "}", sep = "")
    my.index
}
