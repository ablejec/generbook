#' Function that generates the program of the conference
#' @param my.program.file: name of the tab-delimited file where the program is saved
#' @param my.filename: name of the file with the abstract database
#' @param dirAbstracts: directory where the abstracts will be stored, it must exist
#' @param abstract.id: colums where the abstracts to be included in each session are reported
#' @param author.lastname: columns that contain the last name of the authors
#' @param author.first: columns that contain the first name of the authors
#' @param accept: column that contains the acceptance decision, must be " Yes" for the acceptance of the abstract
#' @param pres.title: column that contains the title of the presentation
#' @param id: abstract id column in the database of abstracts
#' @return empty return; TeX files are generated and stored dirAbstracts directory
#' @export
#' @examples 
#' set.seed(1)
generate.program <- 
function (my.program.file, my.filename, dirAbstracts, abstract.id = c(10:14), 
    author.lastname = seq(23, by = 6, length.out = 7), author.firstname = seq(24, 
        by = 6, length.out = 7), accept = 17, pres.title = 18, 
    id = 75, outfile.name = "program.tex") 
{
    init.wd = getwd()
    my.program <- read.delim(my.program.file, sep = "\t")
    my.program <- my.program[!is.na(my.program[, 1]) & my.program[, 
        1] != "", ]
    num.rows <- dim(my.program)[1]
    my.order <- order(my.program$Day * 100 + my.program$TimeBegin)
    my.program <- my.program[my.order, ]
    number.days <- unique(my.program$Day)
    my.time <- my.program$Day * 100 + my.program$TimeBegin
    num.rows <- dim(my.program)[1]
    my.data <- read.delim(my.filename, sep = "\t")
    num.authors <- apply(my.data[, author.lastname], 1, function(x) sum(!is.na(x) & 
        x != ""))
    my.data <- my.data[num.authors != 0, ]
    my.data <- my.data[my.data[, accept] == "Yes", ]
    num.authors <- apply(my.data[, author.lastname], 1, function(x) sum(!is.na(x) & 
        x != ""))
    number.abstracts <- as.numeric(apply(my.program[, abstract.id], 
        1, function(x) sum(!is.na(x))))
    setwd(dirAbstracts)
    zz <- file(outfile.name, "w")
    cat("\\clearpage \n \\pagestyle{fancy}\n\t\t% ------------------------------------------------- Next day -----\n\t\t\\renewcommand{\\Date}{}\n\t\t\\addtocontents{toc}{\\hfill\\textbf{\\Date}\\\\}\n\t\t%% ------------------------------------------- Session start\n\t\t\\renewcommand{\\Session}{}\n\t\t\\SetHeader{\\textbf{", 
        as.character(my.program$DayLong[1]), "}}{}", "\n", file = zz, 
        sep = "")
    for (i in 1:num.rows) {
        cat(i, "\n")
        if (my.program$TimeBegin[i] > 10) 
            time.begin <- strsplit(as.character(my.program$TimeBegin[i]), 
                "")[[1]][1:5]
        else time.begin <- strsplit(as.character(my.program$TimeBegin[i]), 
            "")[[1]][1:4]
        if (!is.na(my.program$TimeEnd[i])) {
            if (my.program$TimeEnd[i] > 10) 
                time.end <- strsplit(as.character(my.program$TimeEnd[i]), 
                  "")[[1]][1:5]
            else time.end <- strsplit(as.character(my.program$TimeEnd[i]), 
                "")[[1]][1:4]
        }
        else time.end <- NA
        cat("\\PrSectionHeader{", time.begin, sep = "", file = zz)
        if (!is.na(time.end[1])) 
            cat("--", time.end, "}", sep = "", file = zz)
        else cat("}", file = zz, sep = "")
        cat("{", as.character(my.program$Name[i]), "}", file = zz, 
            sep = "")
        if (my.program$Room[i] != "") 
            cat("{(", as.character(my.program$Room[i]), ")}", 
                file = zz, sep = "")
        else cat("{}", file = zz, sep = "")
        if (my.program$Chair[i] != "") 
            cat("{Chair: ", as.character(my.program$Chair[i]), 
                "}", file = zz, sep = "")
        else cat("{}", file = zz, sep = "")
        cat("\n", file = zz)
        if (number.abstracts[i] > 0) {
            cat("\\begin{enumerate}\n", file = zz)
            for (ii in 1:number.abstracts[i]) {
                cat(ii, " ")
                my.index <- which(my.data[, id] == my.program[i, 
                  abstract.id[ii]])
                tmp <- paste(my.data[my.index, author.firstname[1]], 
                  " ", my.data[my.index, author.lastname[1]], 
                  sep = "")
                if (num.authors[my.index] > 1) 
                  for (j in 2:num.authors[my.index]) {
                    if (j < num.authors[my.index]) 
                      tmp <- paste(tmp, paste(my.data[my.index, 
                        author.firstname[j]], " ", my.data[my.index, 
                        author.lastname[j]], sep = ""), sep = ", ")
                    else tmp <- paste(tmp, paste(my.data[my.index, 
                      author.firstname[j]], " ", my.data[my.index, 
                      author.lastname[j]], sep = ""), sep = " and ")
                  }
                autori <- tmp
                cat("\\PrTalk{", as.character(my.data[my.index, 
                  pres.title]), "} \\newline {", autori, "}", 
                  sep = "", file = zz)
            }
            cat("\\end{enumerate}\n", file = zz)
        }
        if (i != num.rows) {
            if (my.program$Day[i + 1] != my.program$Day[i]) {
                cat("\\clearpage \n \\pagestyle{fancy}\n\t\t% ------------------------------------------------- Next day -----\n\t\t\\renewcommand{\\Date}{}\n\t\t\\addtocontents{toc}{\\hfill\\textbf{\\Date}\\\\}\n\t\t%% ------------------------------------------- Session start\n\t\t\\renewcommand{\\Session}{}\n\t\t\\SetHeader{\\textbf{", 
                  as.character(my.program$DayLong[i + 1]), "}}{}", 
                  "\n", file = zz, sep = "")
            }
        }
    }
    close(zz)
    session.id <- which(number.abstracts > 0)
    zz <- file("abstracts.tex", "w")
    cat("\\noindent\\\\\n\n\\thispagestyle{empty}\n \\begin{center}\n  \\Large\n   % \\textbf{Program} \\\\ [0.5cm]\n   \\begin{flushright}\n   \\vspace{17cm} {\\Huge \\em{ \\textbf{ABSTRACTS}}} \\\\ [0.5cm]\n   \\end{flushright}\n   \\normalsize\n \\end{center}\n%\\noindent  \\hrulefill \\\\[0.5cm]\n\\small\n\\clearpage\n\n\n\\pagestyle{fancy}\n\\renewcommand{\\Date}{}\n\\addtocontents{toc}{\\hfill\\textbf{\\Date}\\\\}\n\n%% ------------------------------------------- Session start\n\\renewcommand{\\Session}{}\n\\SetHeader{}{\\textbf{Program Overview}}\n%%--------------------------------------------\n\n\n\n\n\n\n\n\n\n %\\begin{center}\n %  \\Large\n %   \\textbf{Abstracts} \\\\ [0.5cm]\n %  \\normalsize\n%  \\begin{flushright}\n%   \\vspace{17cm} {\\Huge \\em{ \\textbf{ABSTRACTS}}} \\\\ [0.5cm]\n%   \\end{flushright}\n%   \\normalsize\n \n \n% \\end{center}\n%\\noindent\\  %\\hrulefill \\\\\n\\small\n\\clearpage \n\n\\pagestyle{fancy}\n\n\\renewcommand{\\Date}{", 
        as.character(my.program$DayShort[session.id[1]]), "}\n\\addtocontents{toc}{\\hfill\\textbf{\\Date}\\\\}\n\n\\renewcommand{\\Session}{", 
        as.character(my.program$Name[session.id[1]]), "}\n\\Section{\\Session}\n\\SetHeader{\\Date}{\\Session}", 
        file = zz, sep = "")
    for (i in 1:length(session.id)) {
        cat(i, "\n")
        for (j in 1:number.abstracts[session.id[i]]) {
            cat("\\input{", my.program[session.id[i], abstract.id[j]], 
                ".tex}\n", file = zz, sep = "")
        }
        cat("\\clearpage\n", file = zz)
        if (i != length(session.id)) {
            cat("\\renewcommand{\\Session}{", as.character(my.program$Name[session.id[i + 
                1]]), "}\n\\Section{\\Session}\n\\SetHeader{\\Date}{\\Session}", 
                file = zz, sep = "")
            if (my.program$Day[session.id[i]] != my.program$Day[session.id[i + 
                1]]) 
                cat("\\renewcommand{\\Date}{", as.character(my.program$DayShort[session.id[i + 
                  1]]), "}\n\\addtocontents{toc}{\\hfill\\textbf{\\Date}\\\\}", 
                  file = zz, sep = "")
        }
    }
    close(zz)
    setwd(init.wd)
}
