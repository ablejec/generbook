#' Function that generates a separate TeX file for each of the abstracts
#' @param my.filename tab delimited database that contains the abstract submission (contribution data base), full path must be given or it must be stored in the working directory of R
#' @param dirAbstracts directory where the abstracts will be stored, it must exist
#' @param author.lastname columns that contain the last name of the authors in the contribution data base (numbers must be gives, i.e. if only two authors are allowed and the last names are stored in the 5th and 10th column of the contribution database, author.lastname=c(5,10))
#' @param author.firstname columns that contain thefirst name of the authors
#' @param author.institution columns that contain the institution of the authors
#' @param author.city columns that contain the city of the authors
#' @param author.country columns that contain the country of the authors
#' @param author.email columns that contain the e-mails of the authors
#' @param author.presenting columns that contain an indicator T/F indicating if the author will present the work
#' @param pres.title column containing the title of the presentation
#' @param pres.abstract column containing the abstract text
#' @param accept column that contains the acceptance decision, must be (exaclty) "Yes" for the accepted abstracts
#' @param topic1 column that contains the first selected topic
#' @param topic2 column that contains the second selected topic
#' @param id column that contains the abstract ID
#' @param notes column that contains the notes added by the reviewers
#' @param ref2 opinion of reviewer2
#' @param ref3 opinion of reviewer3
#' @param ref4 opinion of reviewer4
#' @param accept.all if set to TRUE, all the abstracts included in the database are "accepted", useful to generate the first draft of the abstract book
#' @param noNotes don't write notes, set to TRUE if notes should be omitted, useful for the version published for authors
#' @param style "AS2012" (used for the book of abstracts of Applied Statistics 2012) or "AS2011" (used for the book of abstracts of Applied Statistics 2011), the styles differ in how the e-mails and affiliations of the authors are formatted
#' @param notesDay column that contains the notes of the organizers about the (possible) day of presentation,
#' @param notesPayment column that contains the notes of the organizers about the payment/registration information about the presentation
#' @param verbose FALSE: indicates if some output should be written on screen while executing the function
#' @return TeX files are generated and stored dirAbstracts directory, My.data: data set with submitted data; PA: index indicating the presenting author for each submission.
#' @export
#' @examples
#' set.seed(1)
generate.abstracts <-
function (my.filename, dirAbstracts, author.lastname = seq(23,
    by = 6, length.out = 7), author.firstname = seq(24, by = 6,
    length.out = 7), author.institution = seq(26, by = 6, length.out = 7),
    author.city = seq(27, by = 6, length.out = 7), author.country = seq(3,
        by = 1, length.out = 7) + 1, author.email = seq(25, by = 6,
        length.out = 7), author.presenting = seq(22, by = 6,
        length.out = 7), pres.title = 18, pres.abstract = 21,
    accept = 17, topic1 = 19, topic2 = 3, id = 2, notes = 73,
    ref2 = 77, ref3 = 78, ref4 = 79, duplicated = NULL, accept.all = F,
    noNotes = F, style = "AS2012", notesDay = 81, notesPayment = 82,
    verbose = FALSE)
{
    initial.wd = getwd()
    my.data <- read.delim(my.filename, sep = "\t")
    setwd(dirAbstracts)
    num.authors <- apply(my.data[, author.lastname], 1, function(x) sum(!is.na(x) &
        x != ""))
    my.data <- my.data[num.authors != 0, ]
    my.data[, author.email] = apply(my.data[, author.email],
        2, function(x) as.character(x))
    for (i in 1:nrow(my.data)) for (j in 1:length(author.email)) my.data[i,
        author.email[j]] = sub(pattern = "_", replacement = "\\\\_",
        my.data[i, author.email[j]])
    if (accept.all != T)
        my.data <- my.data[my.data[, accept] == "Yes", ]
    num.authors <- apply(my.data[, author.lastname], 1, function(x) sum(!is.na(x) &
        x != ""))
    presenting.author <- as.numeric(unlist(apply(my.data[, author.presenting],
        1, function(x) which(x == "Yes")[1])))
    presenting.author[is.na(presenting.author)] <- 1
    titolo = autori = affiliazioni = abstract = rep("", dim(my.data)[1])
    if (style == "AS2012") {
        for (i in 1:dim(my.data)[1]) {
            if (verbose == TRUE)
                cat("ID=", i)
            titolo[i] <- paste("{", my.data[i, pres.title], "}",
                sep = "")
            TMP <- strsplit(as.character(my.data[, author.firstname[1]]),
                c(" "))
            number.of.names <- unlist(lapply(TMP, length))
            authors.initials <- toupper(strsplit(TMP[[i]][1],
                "")[[1]][1])
            if (number.of.names[i] > 1)
                for (j in 2:number.of.names[i]) authors.initials <- paste(authors.initials,
                  toupper(strsplit(TMP[[i]][j], "")[[1]][1]),
                  sep = "")
            inst.city = paste(unlist((my.data[i, author.institution[1:num.authors[i]]])),
                unlist((my.data[i, author.city[1:num.authors[i]]])))
            how.many.inst = length(unique(inst.city))
            one.inst.true = how.many.inst == 1
            if (one.inst.true)
                index.inst = rep(1, num.authors[i])
            else {
                index.inst = as.numeric(factor(inst.city, levels = unique(inst.city)))
            }
            if (num.authors[i] == 1) {
                tmp <- paste(my.data[i, author.firstname[1]],
                  " ", my.data[i, author.lastname[1]], .fun.get.index.slo(my.data[i,
                    author.lastname[1]], authors.initials), sep = "")
                tmp.mails = paste("\\Email{", ifelse(!is.na(my.data[i,
                  author.email[1]]), paste(my.data[i, author.email[1]]),
                  ""), "}\n", sep = "")
            }
            else {
                if (presenting.author[i] == 1) {
                  if (one.inst.true) {
                    tmp <- paste("\\Presenting{", my.data[i,
                      author.firstname[1]], " ", my.data[i, author.lastname[1]],
                      "}", .fun.get.index.slo(my.data[i, author.lastname[1]],
                        authors.initials), sep = "")
                  }
                  else tmp <- paste("\\Presenting{", my.data[i,
                    author.firstname[1]], " ", my.data[i, author.lastname[1]],
                    "}", "$^1$", .fun.get.index.slo(my.data[i,
                      author.lastname[1]], authors.initials),
                    sep = "")
                }
                else {
                  if (one.inst.true) {
                    tmp <- paste(my.data[i, author.firstname[1]],
                      " ", my.data[i, author.lastname[1]], .fun.get.index.slo(my.data[i,
                        author.lastname[1]], authors.initials),
                      sep = "")
                  }
                  else {
                    tmp <- paste(my.data[i, author.firstname[1]],
                      " ", my.data[i, author.lastname[1]], "$^1$",
                      .fun.get.index.slo(my.data[i, author.lastname[1]],
                        authors.initials), sep = "")
                  }
                }
            }
            if (num.authors[i] > 1)
                for (j in 2:num.authors[i]) {
                  TMP <- strsplit(as.character(my.data[, author.firstname[j]]),
                    c(" "))
                  number.of.names <- unlist(lapply(TMP, length))
                  authors.initials <- toupper(strsplit(TMP[[i]][1],
                    "")[[1]][1])
                  if (number.of.names[i] > 1)
                    for (jj in 2:number.of.names[i]) authors.initials <- paste(authors.initials,
                      toupper(strsplit(TMP[[i]][jj], "")[[1]][1]),
                      sep = "")
                  if (j < num.authors[i]) {
                    if (one.inst.true) {
                      if (presenting.author[i] == j)
                        tmp <- paste(tmp, ", \\Presenting{",
                          paste(my.data[i, author.firstname[j]],
                            " ", my.data[i, author.lastname[j]],
                            "}", .fun.get.index.slo(my.data[i,
                              author.lastname[j]], authors.initials),
                            sep = ""))
                      else tmp <- paste(tmp, paste(my.data[i,
                        author.firstname[j]], " ", my.data[i,
                        author.lastname[j]], .fun.get.index.slo(my.data[i,
                        author.lastname[j]], authors.initials),
                        sep = ""), sep = ", ")
                    }
                    else {
                      if (presenting.author[i] == j)
                        tmp <- paste(tmp, ", \\Presenting{",
                          paste(my.data[i, author.firstname[j]],
                            " ", my.data[i, author.lastname[j]],
                            "}$^", index.inst[j], "$", .fun.get.index.slo(my.data[i,
                              author.lastname[j]], authors.initials),
                            sep = ""), sep = "")
                      else tmp <- paste(tmp, paste(my.data[i,
                        author.firstname[j]], " ", my.data[i,
                        author.lastname[j]], "$^", index.inst[j],
                        "$", .fun.get.index.slo(my.data[i, author.lastname[j]],
                          authors.initials), sep = ""), sep = ", ")
                    }
                  }
                  else {
                    if (one.inst.true) {
                      if (presenting.author[i] == j)
                        tmp <- paste(tmp, " and \\Presenting{",
                          paste(my.data[i, author.firstname[j]],
                            " ", my.data[i, author.lastname[j]],
                            "}", .fun.get.index.slo(my.data[i,
                              author.lastname[j]], authors.initials),
                            sep = ""), sep = "")
                      else tmp <- paste(tmp, paste(my.data[i,
                        author.firstname[j]], " ", my.data[i,
                        author.lastname[j]], .fun.get.index.slo(my.data[i,
                        author.lastname[j]], authors.initials),
                        sep = ""), sep = " and ")
                    }
                    else {
                      if (presenting.author[i] == j)
                        tmp <- paste(tmp, "and \\Presenting{",
                          paste(my.data[i, author.firstname[j]],
                            " ", my.data[i, author.lastname[j]],
                            "}$^", index.inst[j], "$", .fun.get.index.slo(my.data[i,
                              author.lastname[j]], authors.initials),
                            sep = ""), sep = "")
                      else tmp <- paste(tmp, paste(my.data[i,
                        author.firstname[j]], " ", my.data[i,
                        author.lastname[j]], "$^", index.inst[j],
                        "$", .fun.get.index.slo(my.data[i, author.lastname[j]],
                          authors.initials), sep = ""), sep = " and ")
                    }
                  }
                }
            autori[i] <- paste("{", tmp, "}", sep = "")
            if (num.authors[i] == 1) {
                tmp <- paste("\\Afilliation{", ifelse(my.data[i,
                  author.institution[1]] != "", paste(my.data[i,
                  author.institution[1]], ", ", sep = ""), ""),
                  ifelse(my.data[i, author.city[1]] != "", paste(my.data[i,
                    author.city[1]], ", ", sep = ""), ""), ifelse(my.data[i,
                    author.country[1]] != "", paste(my.data[i,
                    author.country[1]]), ""), "}\n", sep = "")
            }
            else {
                if (one.inst.true) {
                  tmp <- paste("\\Afilliation{", ifelse(my.data[i,
                    author.institution[1]] != "", paste(my.data[i,
                    author.institution[1]], ", ", sep = ""),
                    ""), ifelse(my.data[i, author.city[1]] !=
                    "", paste(my.data[i, author.city[1]], ", ",
                    sep = ""), ""), ifelse(my.data[i, author.country[1]] !=
                    "", paste(my.data[i, author.country[1]]),
                    ""), "}\n", sep = "")
                  tmp.mails = paste("\\Email{", ifelse(!is.na(my.data[i,
                    author.email[1]]), paste(my.data[i, author.email[1]]),
                    ""), "}\n", sep = "")
                  for (j in 2:num.authors[i]) {
                    tmp.mails <- paste(tmp.mails, "$\\Email{",
                      ifelse(!is.na(my.data[i, author.email[j]]),
                        paste(my.data[i, author.email[j]]), ""),
                      "}\n", sep = "")
                  }
                }
                else {
                  tmp <- paste("\\Afilliation{$^", 1, "$", ifelse(my.data[i,
                    author.institution[1]] != "", paste(my.data[i,
                    author.institution[1]], ", ", sep = ""),
                    ""), ifelse(my.data[i, author.city[1]] !=
                    "", paste(my.data[i, author.city[1]], ", ",
                    sep = ""), ""), ifelse(my.data[i, author.country[1]] !=
                    "", paste(my.data[i, author.country[1]]),
                    ""), "}\n ", sep = "")
                  if (num.authors[i] > 1) {
                    for (j in unique(index.inst)[-1]) {
                      which.author.use = which(j == index.inst)[1]
                      tmp <- paste(tmp, "\\Afilliation{$^", j,
                        "$", ifelse(my.data[i, author.institution[which.author.use]] !=
                          "", paste(my.data[i, author.institution[which.author.use]],
                          ", ", sep = ""), ""), ifelse(my.data[i,
                          author.city[which.author.use]] != "",
                          paste(my.data[i, author.city[which.author.use]],
                            ", ", sep = ""), ""), ifelse(my.data[i,
                          author.country[which.author.use]] !=
                          "", paste(my.data[i, author.country[which.author.use]]),
                          ""), "}\n", sep = "")
                    }
                  }
                }
                tmp.mails = paste("\\Email{", ifelse(!is.na(my.data[i,
                  author.email[1]]), paste(my.data[i, author.email[1]]),
                  ""), "}", sep = "")
                for (j in 2:num.authors[i]) tmp.mails = paste(tmp.mails,
                  paste("\\Email{", ifelse(!is.na(my.data[i,
                    author.email[j]]), paste(my.data[i, author.email[j]]),
                    ""), "}", sep = ""), sep = ", ")
            }
            tmp = paste(tmp, tmp.mails, sep = "\n")
            affiliazioni[i] <- paste("{", tmp, "}", sep = "")
            abstract[i] <- paste("{", my.data[i, pres.abstract],
                "}", sep = "")
            if (noNotes == F)
                temp <- paste("{Topic1: ", my.data[i, topic1],
                  ", Topic2: ", my.data[i, topic2], ". Abstract ID: ",
                  my.data[i, id], ". Accepted: ", my.data[i,
                    accept], ". Notes: ", my.data[i, notes],
                  ". Ref1: ", my.data[i, ref2], ". Ref2: ", my.data[i,
                    ref3], ". Ref3: ", my.data[i, ref4], "}",
                  sep = "")
            else temp <- paste("{Abstract ID: ", my.data[i, id],
                ". Topic1: ", my.data[i, topic1], ", Topic2: ",
                my.data[i, topic2], ".}", sep = "")
            zz <- file(paste(my.data[i, id], ".tex", sep = ""),
                "w")
            cat("\\A", titolo[i], autori[i], affiliazioni[i],
                temp, abstract[i], sep = "\n", file = zz)
            close(zz)
        }
    }
    else {
        for (i in 1:dim(my.data)[1]) {
            if (verbose == TRUE)
                cat("ID=", i)
            titolo[i] <- paste("{", my.data[i, pres.title], "}",
                sep = "")
            TMP <- strsplit(as.character(my.data[, author.firstname[1]]),
                c(" "))
            number.of.names <- unlist(lapply(TMP, length))
            authors.initials <- toupper(strsplit(TMP[[i]][1],
                "")[[1]][1])
            if (number.of.names[i] > 1)
                for (j in 2:number.of.names[i]) authors.initials <- paste(authors.initials,
                  toupper(strsplit(TMP[[i]][j], "")[[1]][1]),
                  sep = "")
            one.inst.true <- length(unique(unlist((my.data[i,
                author.institution[1:num.authors[i]]])))) ==
                1 & length(unique(unlist((my.data[i, author.city[1:num.authors[i]]])))) ==
                1
            if (num.authors[i] == 1) {
                tmp <- paste(my.data[i, author.firstname[1]],
                  " ", my.data[i, author.lastname[1]], .fun.get.index.slo(my.data[i,
                    author.lastname[1]], authors.initials), sep = "")
            }
            else {
                if (presenting.author[i] == 1)
                  tmp <- paste("\\Presenting{", my.data[i, author.firstname[1]],
                    " ", my.data[i, author.lastname[1]], "}",
                    "$^1$", .fun.get.index.slo(my.data[i, author.lastname[1]],
                      authors.initials), sep = "")
                else tmp <- paste(my.data[i, author.firstname[1]],
                  " ", my.data[i, author.lastname[1]], "$^1$",
                  .fun.get.index.slo(my.data[i, author.lastname[1]],
                    authors.initials), sep = "")
                if (num.authors[i] > 1)
                  for (j in 2:num.authors[i]) {
                    TMP <- strsplit(as.character(my.data[, author.firstname[j]]),
                      c(" "))
                    number.of.names <- unlist(lapply(TMP, length))
                    authors.initials <- toupper(strsplit(TMP[[i]][1],
                      "")[[1]][1])
                    if (number.of.names[i] > 1)
                      for (jj in 2:number.of.names[i]) authors.initials <- paste(authors.initials,
                        toupper(strsplit(TMP[[i]][jj], "")[[1]][1]),
                        sep = "")
                    if (j < num.authors[i]) {
                      if (presenting.author[i] == j)
                        tmp <- paste(tmp, ", \\Presenting{",
                          paste(my.data[i, author.firstname[j]],
                            " ", my.data[i, author.lastname[j]],
                            "}$^", j, "$", .fun.get.index.slo(my.data[i,
                              author.lastname[j]], authors.initials),
                            sep = ""))
                      else tmp <- paste(tmp, paste(my.data[i,
                        author.firstname[j]], " ", my.data[i,
                        author.lastname[j]], "$^", j, "$", .fun.get.index.slo(my.data[i,
                        author.lastname[j]], authors.initials),
                        sep = ""), sep = ", ")
                    }
                    else {
                      if (presenting.author[i] == j)
                        tmp <- paste(tmp, "and \\Presenting{",
                          paste(my.data[i, author.firstname[j]],
                            " ", my.data[i, author.lastname[j]],
                            "}$^", j, "$", .fun.get.index.slo(my.data[i,
                              author.lastname[j]], authors.initials),
                            sep = ""), sep = "")
                      else tmp <- paste(tmp, paste(my.data[i,
                        author.firstname[j]], " ", my.data[i,
                        author.lastname[j]], "$^", j, "$", .fun.get.index.slo(my.data[i,
                        author.lastname[j]], authors.initials),
                        sep = ""), sep = " and ")
                    }
                  }
            }
            autori[i] <- paste("{", tmp, "}", sep = "")
            if (num.authors[i] == 1) {
                tmp <- paste("\\Afilliation{", ifelse(my.data[i,
                  author.institution[1]] != "", paste(my.data[i,
                  author.institution[1]], ", ", sep = ""), ""),
                  ifelse(my.data[i, author.city[1]] != "", paste(my.data[i,
                    author.city[1]], ", ", sep = ""), ""), ifelse(my.data[i,
                    author.country[1]] != "", paste(my.data[i,
                    author.country[1]]), ""), "}; \\Email{",
                  ifelse(!is.na(my.data[i, author.email[1]]),
                    paste(my.data[i, author.email[1]]), ""),
                  "}\n", sep = "")
            }
            else {
                if (one.inst.true) {
                  tmp <- paste("\\Afilliation{", ifelse(my.data[i,
                    author.institution[1]] != "", paste(my.data[i,
                    author.institution[1]], ", ", sep = ""),
                    ""), ifelse(my.data[i, author.city[1]] !=
                    "", paste(my.data[i, author.city[1]], ", ",
                    sep = ""), ""), ifelse(my.data[i, author.country[1]] !=
                    "", paste(my.data[i, author.country[1]]),
                    ""), "}\n\n\t\t\t\t\t$^1$\\Email{", ifelse(!is.na(my.data[i,
                    author.email[1]]), paste(my.data[i, author.email[1]]),
                    ""), "}\n", sep = "")
                  for (j in 2:num.authors[i]) {
                    tmp <- paste(tmp, "$^", j, "$\\Email{", ifelse(!is.na(my.data[i,
                      author.email[j]]), paste(my.data[i, author.email[j]]),
                      ""), "}\n", sep = "")
                  }
                }
                else {
                  tmp <- paste("\\Afilliation{$^", 1, "$", ifelse(my.data[i,
                    author.institution[1]] != "", paste(my.data[i,
                    author.institution[1]], ", ", sep = ""),
                    ""), ifelse(my.data[i, author.city[1]] !=
                    "", paste(my.data[i, author.city[1]], ", ",
                    sep = ""), ""), ifelse(my.data[i, author.country[1]] !=
                    "", paste(my.data[i, author.country[1]]),
                    ""), "}; \\Email{", ifelse(!is.na(my.data[i,
                    author.email[1]]), paste(my.data[i, author.email[1]]),
                    ""), "}\n", sep = "")
                  for (j in 2:num.authors[i]) {
                    tmp <- paste(tmp, "\\Afilliation{$^", j,
                      "$", ifelse(my.data[i, author.institution[j]] !=
                        "", paste(my.data[i, author.institution[j]],
                        ", ", sep = ""), ""), ifelse(my.data[i,
                        author.city[j]] != "", paste(my.data[i,
                        author.city[j]], ", ", sep = ""), ""),
                      ifelse(my.data[i, author.country[j]] !=
                        "", paste(my.data[i, author.country[j]]),
                        ""), "}; \\Email{", ifelse(!is.na(my.data[i,
                        author.email[j]]), paste(my.data[i, author.email[j]]),
                        ""), "}\n", sep = "")
                  }
                }
            }
            affiliazioni[i] <- paste("{", tmp, "}", sep = "")
            abstract[i] <- paste("{", my.data[i, pres.abstract],
                "}", sep = "")
            if (noNotes == F)
                temp <- paste("{Topic1: ", my.data[i, topic1],
                  ", Topic2: ", my.data[i, topic2], ". Abstract ID: ",
                  my.data[i, id], ". Accepted: ", my.data[i,
                    accept], ". Notes: ", my.data[i, notes],
                  ". Ref1: ", my.data[i, ref2], ". Ref2: ", my.data[i,
                    ref3], ". Ref3: ", my.data[i, ref4], "}",
                  sep = "")
            else temp <- paste("{Abstract ID: ", my.data[i, id],
                ". Topic1: ", my.data[i, topic1], ", Topic2: ",
                my.data[i, topic2], ".}", sep = "")
            zz <- file(paste(my.data[i, id], ".tex", sep = ""),
                "w")
            cat("\\A", titolo[i], autori[i], affiliazioni[i],
                temp, abstract[i], sep = "\n", file = zz)
            close(zz)
        }
    }
    zz <- file(paste("abstractList.tex", sep = ""), "w")
    for (i in my.data[, id]) cat("\\input{", i, ".tex}\n", sep = "",
        file = zz)
    close(zz)
    zz <- file(paste("abstractListAccepted.tex", sep = ""), "w")
    for (i in my.data[my.data[, accept] == "Yes", id]) cat("\\input{",
        i, ".tex}\n", sep = "", file = zz)
    close(zz)
    all.topics <- sort(unique(levels(my.data[, topic1]), levels(my.data[,
        topic2])))
    all.topics <- all.topics[all.topics != "---" | all.topics ==
        ""]
    zz <- file("abstractListByTopic.tex", "w")
    for (ii in 1:length(all.topics)) {
        which.abstracts = unique(which(my.data[, topic1] == all.topics[ii] |
            my.data[, topic1] == all.topics[ii]))
        cat("{\\bf \\Large ", as.character(all.topics[ii]), "}\\\\\\\\",
            sep = " ", file = zz)
        for (i in which.abstracts) cat(titolo[i], "\\\\", autori[i],
            "\\\\", affiliazioni[i], paste(" ID=", my.data[i,
                id], "; Day=", my.data[i, notesDay], "Payment=",
                my.data[i, notesPayment], "\\\\ Topics={\\small ",
                as.character(my.data[i, topic1]), as.character(my.data[i,
                  topic2]), "} \\\\\\\\", sep = " "), sep = " ",
            file = zz)
        cat("\\\\ \\clearpage", file = zz)
    }
    close(zz)
    my.data.accepted = my.data[my.data[, accept] == "Yes", ]
    all.topics = sort(unique(levels(my.data.accepted[, topic1]),
        levels(my.data.accepted[, topic2])))
    all.topics = all.topics[all.topics != "---" | all.topics ==
        ""]
    zz <- file("abstractListByTopicAccepted.tex", "w")
    for (ii in 1:length(all.topics)) {
        which.abstracts = unique(which(my.data.accepted[, topic1] ==
            all.topics[ii] | my.data.accepted[, topic1] == all.topics[ii]))
        cat("{\\bf \\Large ", as.character(all.topics[ii]), "}\\\\\\\\",
            sep = " ", file = zz)
        for (i in which.abstracts) cat(titolo[my.data[, accept] ==
            "Yes"][i], "\\\\", autori[my.data[, accept] == "Yes"][i],
            "\\\\", affiliazioni[my.data[, accept] == "Yes"][i],
            paste(" ID=", my.data.accepted[i, id], "; Day=",
                my.data.accepted[i, notesDay], "Payment=", my.data.accepted[i,
                  notesPayment], "\\\\ Topics={\\small ", as.character(my.data.accepted[i,
                  topic1]), as.character(my.data.accepted[i,
                  topic2]), "} \\\\\\\\", sep = " "), sep = " ",
            file = zz)
        cat("\\\\ \\clearpage", file = zz)
    }
    close(zz)
    setwd(initial.wd)
    return(list(My.data = my.data, PA = presenting.author))
}
