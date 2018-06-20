#' Function that generates the program overview file
#' @param my.program.file: name of the tab-delimited file where the program is saved
#' @param dirAbstracts: directory where the abstracts will be stored, it must exist
#' @param abstract.id: colums where the abstracts to be included in each session are reported
#' @export
#' @examples 
#' set.seed(1)
generate.programOverview <-
function (my.program.file, dirAbstracts, abstract.id = c(10:14)) 
{
    init.wd = getwd()
    my.program <- read.delim(my.program.file, sep = "\t")
    my.program <- my.program[!is.na(my.program[, 1]) & my.program[, 
        1] != "", ]
    num.rows <- dim(my.program)[1]
    setwd(dirAbstracts)
    my.order <- order(my.program$Day * 100 + my.program$TimeBegin)
    my.program <- my.program[my.order, ]
    number.days <- unique(my.program$Day)
    room.names <- unique(my.program$Room[my.program$Room != ""])
    number.rooms <- length(room.names)
    my.time <- my.program$Day * 100 + my.program$TimeBegin
    number.abstracts <- as.numeric(apply(my.program[, abstract.id], 
        1, function(x) sum(!is.na(x))))
    which.split <- ifelse(nchar(as.character(my.program$Name)) >= 
        25, 1, 0)
    where.split <- rep(NA, num.rows + 1)
    names.split <- vector("list", num.rows + 1)
    names.split[[num.rows + 1]] <- ""
    where.split[num.rows + 1] <- 2
    my.program.name <- c(as.character(my.program$Name), "")
    for (i in c(1:num.rows)[which.split == 1]) {
        names.split[[i]] <- tmp <- strsplit(as.character(my.program$Name[i]), 
            split = " ")
        where.split[i] <- which(cumsum(unlist(lapply(tmp, nchar))) >= 
            25)[1]
        if (cumsum(unlist(lapply(tmp, nchar)))[length(tmp[[1]])] <= 
            25) 
            where.split[i] <- length(tmp[[1]])
    }
    for (i in c(1:num.rows)[which.split == 0]) {
        names.split[[i]] <- tmp <- as.character(my.program$Name[i])
    }
    for (i in c(1:num.rows)[which.split == 0]) {
        tmp <- strsplit(as.character(my.program$Name[i]), split = " ")
        where.split[i] <- length(tmp)[[1]] + 1
    }
    which.split <- c(which.split, 0)
    my.done <- rep(FALSE, num.rows)
    zz <- file("programOverview.tex", "w")
    cat("\\noindent\\\\\n\n\\thispagestyle{empty}\n \\begin{center}\n  \\Large\n   % \\textbf{Program} \\\\ [0.5cm]\n   \\begin{flushright}\n   \\vspace{17cm} {\\Huge \\em{ \\textbf{PROGRAM}}} \\\\ [0.5cm]\n   \\end{flushright}\n   \\normalsize\n \\end{center}\n%\\noindent  \\hrulefill \\\\[0.5cm]\n\\small\n\\clearpage\n\n\n\\pagestyle{fancy}\n\\renewcommand{\\Date}{}\n\\addtocontents{toc}{\\hfill\\textbf{\\Date}\\\\}\n\n%% ------------------------------------------- Session start\n\\renewcommand{\\Session}{}\n\\SetHeader{}{\\textbf{Program Overview}}\n%%--------------------------------------------\n\n\n\\vspace*{-1.0cm}\n\\begin{center} %end of the first cat, header of the file and beginning of the table\n\\begin{tabular}{|l|| l |", 
        file = zz)
    cat(rep("c", number.rooms, sep = " "), "|}\\hline", file = zz)
    cat("&&", paste(room.names, collapse = "&"), "\\\\\\hline\\hline\n", 
        file = zz)
    day.within <- 0
    day.changed <- FALSE
    for (i in 1:num.rows) {
        cat(i, "\n")
        if (my.done[i] == FALSE) {
            if (i > 1) {
                if (my.program$DayTable[i] != my.program$DayTable[i - 
                  1]) 
                  day.within <- 0
                if (day.changed == T) 
                  my.line <- "\\\\\\hline\\hline"
                else my.line <- paste("\\\\\\cline{2-", number.rooms + 
                  2, "}")
                cat(my.line, "\n", file = zz)
            }
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
            if (day.within == 0) 
                tmp.day <- as.character(my.program$DayTable[i])
            else tmp.day <- ""
            day.within <- day.within + 1
            which.same.time <- which(my.time[i] == my.time)
            if (my.program$Room[i] == "") {
                tmp.session <- paste("\\multicolumn{", number.rooms, 
                  "}{c|}{\\cellcolor[gray]{0.9}", my.program$Name[i], 
                  "}")
                tmp.time <- paste(paste(time.begin, collapse = ""), 
                  ifelse(!is.na(time.end[1]), paste(" --", paste(time.end, 
                    collapse = "")), ""), sep = "")
                cat(paste(tmp.day, tmp.time, tmp.session, sep = "&"), 
                  file = zz)
                if (i < num.rows) 
                  day.changed <- ifelse(my.program$Day[i] != 
                    my.program$Day[i + 1], TRUE, FALSE)
            }
            else {
                which.room <- vector("list", number.rooms)
                which.index <- numeric(number.rooms)
                for (ii in 1:number.rooms) {
                  tmp <- which(my.program$Room == room.names[ii])
                  iii <- tmp[is.element(tmp, which.same.time)]
                  my.done[iii] <- TRUE
                  which.index[ii] <- ifelse(length(iii) == 1, 
                    iii, num.rows + 1)
                  my.max <- max(which.index[!is.element(which.index, 
                    num.rows + 1)])
                  if (i < num.rows) 
                    day.changed <- ifelse(my.program$Day[i] != 
                      my.program$Day[my.max + 1], TRUE, FALSE)
                }
                if (any(which.split[which.index] == 1)) {
                  tmp.session <- paste(paste(unlist(lapply(which.index, 
                    function(j) paste(unlist(names.split[[j]])[1:(where.split[j] - 
                      1)], collapse = " "))), collapse = "&"), 
                    "\\\\")
                  tmp.session.2 <- paste(paste(unlist(lapply(which.index, 
                    function(j) paste(unlist(names.split[[j]])[-c(1:(where.split[j] - 
                      1))], collapse = " "))), collapse = "&"), 
                    sep = "")
                  tmp.time <- paste("\\multirow{2}{*}{", paste(time.begin, 
                    collapse = ""), ifelse(!is.na(time.end[1]), 
                    paste(" --", paste(time.end, collapse = ""), 
                      "}"), "}"), sep = "")
                  cat(paste(tmp.day, tmp.time, tmp.session, sep = "&"), 
                    file = zz)
                  cat("\n", file = zz)
                  cat(paste("", "", tmp.session.2, sep = "&"), 
                    file = zz)
                  cat("\n", file = zz)
                }
                else {
                  tmp.session <- paste(paste(my.program.name[which.index], 
                    collapse = "&"), sep = "")
                  tmp.time <- paste(paste(time.begin, collapse = ""), 
                    ifelse(!is.na(time.end[1]), paste(" --", 
                      paste(time.end, collapse = "")), ""), sep = "")
                  cat(paste(tmp.day, tmp.time, tmp.session, sep = "&"), 
                    file = zz)
                  my.max <- max(which.index[!is.element(which.index, 
                    num.rows + 1)])
                  if (i < num.rows) 
                    day.changed <- ifelse(my.program$Day[i] != 
                      my.program$Day[my.max + 1], TRUE, FALSE)
                }
                cat("\n", file = zz)
            }
        }
    }
    cat("\\\\\\hline", "\n", "\\end{tabular} \\end{center}\n\\clearpage", 
        file = zz)
    close(zz)
    setwd(init.wd)
}
