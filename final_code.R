###################################################################################################
# Design Name: Web Scrapping using R
# Authors: Suhas Yelluru | Viral Patel | Pushkar Ghalaut 
# Create Date: Nov 2nd 2016 ; 23:35
# Description: This code retrieves information from an online Journal 
#              based on the constraint years specified by the user. 
###################################################################################################

function (start_date, end_date) 
{
  library(RCurl)
  library(XML)
  library(httr)
  library(stringr)
  library(xml2)
  library(xlsx)
  input = "http://journals.plos.org/plosgenetics/volume"
  htmlMain <- getURL(input, followlocation = TRUE)
  docMain = htmlParse(htmlMain)
  link16 = xpathSApply(docMain, "//li[@id='2016']/ul/li/a/@href")
  link15 = xpathSApply(docMain, "//li[@id='2015']/ul/li/a/@href")
  link14 = xpathSApply(docMain, "//li[@id='2014']/ul/li/a/@href")
  link13 = xpathSApply(docMain, "//li[@id='2013']/ul/li/a/@href")
  link12 = xpathSApply(docMain, "//li[@id='2012']/ul/li/a/@href")
  link11 = xpathSApply(docMain, "//li[@id='2011']/ul/li/a/@href")
  link10 = xpathSApply(docMain, "//li[@id='2010']/ul/li/a/@href")
  link9 = xpathSApply(docMain, "//li[@id='2009']/ul/li/a/@href")
  link8 = xpathSApply(docMain, "//li[@id='2008']/ul/li/a/@href")
  link7 = xpathSApply(docMain, "//li[@id='2007']/ul/li/a/@href")
  link6 = xpathSApply(docMain, "//li[@id='2006']/ul/li/a/@href")
  link5 = xpathSApply(docMain, "//li[@id='2005']/ul/li/a/@href")
  ArticleLink16 <- list()
  ArticleLink15 <- list()
  ArticleLink14 <- list()
  ArticleLink13 <- list()
  ArticleLink12 <- list()
  ArticleLink11 <- list()
  ArticleLink10 <- list()
  ArticleLink09 <- list()
  ArticleLink08 <- list()
  ArticleLink07 <- list()
  ArticleLink06 <- list()
  ArticleLink05 <- list()
  l16 <- list()
  l15 <- list()
  l14 <- list()
  l13 <- list()
  l12 <- list()
  l11 <- list()
  l10 <- list()
  l09 <- list()
  l08 <- list()
  l07 <- list()
  l06 <- list()
  l05 <- list()
  AllArticleLinks <- list()
  year = start_date
  while (year <= end_date) {
    if (year == 2016) {
      for (i in 1:length(link16)) {
        l16 <- c(l16, paste0("http://journals.plos.org", 
                             link16[[i]]))
        html16 <- getURL(l16[[i]], followlocation = TRUE)
        doc16 <- htmlParse(html16)
        temp16 <- xpathSApply(doc16, "//h3/a/@href")
        temp16_1 <- paste0("http://journals.plos.org", 
                           temp16)
        ArticleLink16 <- c(ArticleLink16, temp16_1)
      }
      AllArticleLinks[[length(AllArticleLinks) + 1]] <- ArticleLink16
      year = year + 1
    }
    if (year == 2015) {
      for (i in 1:length(link15)) {
        l15 <- c(l15, paste0("http://journals.plos.org", 
                             link15[[i]]))
        html15 <- getURL(l15[[i]], followlocation = TRUE)
        doc15 <- htmlParse(html15)
        temp15 <- xpathSApply(doc15, "//h3/a/@href")
        temp15_1 <- paste0("http://journals.plos.org", 
                           temp15)
        ArticleLink15 <- c(ArticleLink15, temp15_1)
      }
      AllArticleLinks[[length(AllArticleLinks) + 1]] <- ArticleLink15
      year = year + 1
    }
    if (year == 2014) {
      for (i in 1:length(link14)) {
        l14 <- c(l14, paste0("http://journals.plos.org", 
                             link14[[i]]))
        html14 <- getURL(l14[[i]], followlocation = TRUE)
        doc14 <- htmlParse(html14)
        temp14 <- xpathSApply(doc14, "//h3/a/@href")
        temp14_1 <- paste0("http://journals.plos.org", 
                           temp14)
        ArticleLink14 <- c(ArticleLink16, temp14_1)
      }
      AllArticleLinks[[length(AllArticleLinks) + 1]] <- ArticleLink14
      year = year + 1
    }
    if (year == 2013) {
      for (i in 1:length(link13)) {
        l13 <- c(l13, paste0("http://journals.plos.org", 
                             link13[[i]]))
        html13 <- getURL(l13[[i]], followlocation = TRUE)
        doc13 <- htmlParse(html13)
        temp13 <- xpathSApply(doc13, "//h3/a/@href")
        temp13_1 <- paste0("http://journals.plos.org", 
                           temp13)
        ArticleLink13 <- c(ArticleLink13, temp13_1)
      }
      AllArticleLinks[[length(AllArticleLinks) + 1]] <- ArticleLink13
      year = year + 1
    }
    if (year == 2012) {
      for (i in 1:length(link12)) {
        l12 <- c(l12, paste0("http://journals.plos.org", 
                             link12[[i]]))
        html12 <- getURL(l12[[i]], followlocation = TRUE)
        doc12 <- htmlParse(html12)
        temp12 <- xpathSApply(doc12, "//h3/a/@href")
        temp12_1 <- paste0("http://journals.plos.org", 
                           temp12)
        ArticleLink12 <- c(ArticleLink12, temp12_1)
      }
      AllArticleLinks[[length(AllArticleLinks) + 1]] <- ArticleLink12
      year = year + 1
    }
    if (year == 2011) {
      for (i in 1:length(link11)) {
        l11 <- c(l11, paste0("http://journals.plos.org", 
                             link11[[i]]))
        html11 <- getURL(l11[[i]], followlocation = TRUE)
        doc11 <- htmlParse(html11)
        temp11 <- xpathSApply(doc11, "//h3/a/@href")
        temp11_1 <- paste0("http://journals.plos.org", 
                           temp11)
        ArticleLink11 <- c(ArticleLink11, temp11_1)
      }
      AllArticleLinks[[length(AllArticleLinks) + 1]] <- ArticleLink11
      year = year + 1
    }
    if (year == 2010) {
      for (i in 1:length(link10)) {
        l10 <- c(l10, paste0("http://journals.plos.org", 
                             link10[[i]]))
        html10 <- getURL(l10[[i]], followlocation = TRUE)
        doc10 <- htmlParse(html10)
        temp10 <- xpathSApply(doc10, "//h3/a/@href")
        temp10_1 <- paste0("http://journals.plos.org", 
                           temp10)
        ArticleLink10 <- c(ArticleLink10, temp10_1)
      }
      AllArticleLinks[[length(AllArticleLinks) + 1]] <- ArticleLink10
      year = year + 1
    }
    if (year == 2009) {
      for (i in 1:length(link9)) {
        l09 <- c(l09, paste0("http://journals.plos.org", 
                             link9[[i]]))
        html09 <- getURL(l09[[i]], followlocation = TRUE)
        doc09 <- htmlParse(html09)
        temp09 <- xpathSApply(doc09, "//h3/a/@href")
        temp09_1 <- paste0("http://journals.plos.org", 
                           temp09)
        ArticleLink09 <- c(ArticleLink09, temp09_1)
      }
      AllArticleLinks[[length(AllArticleLinks) + 1]] <- ArticleLink09
      year = year + 1
    }
    if (year == 2008) {
      for (i in 1:length(link8)) {
        l08 <- c(l08, paste0("http://journals.plos.org", 
                             link8[[i]]))
        html08 <- getURL(l08[[i]], followlocation = TRUE)
        doc08 <- htmlParse(html08)
        temp08 <- xpathSApply(doc08, "//h3/a/@href")
        temp08_1 <- paste0("http://journals.plos.org", 
                           temp08)
        ArticleLink08 <- c(ArticleLink08, temp08_1)
      }
      AllArticleLinks[[length(AllArticleLinks) + 1]] <- ArticleLink08
      year = year + 1
    }
    if (year == 2007) {
      for (i in 1:length(link7)) {
        l07 <- c(l07, paste0("http://journals.plos.org", 
                             link07[[i]]))
        html07 <- getURL(l07[[i]], followlocation = TRUE)
        doc07 <- htmlParse(html07)
        temp07 <- xpathSApply(doc07, "//h3/a/@href")
        temp07_1 <- paste0("http://journals.plos.org", 
                           temp07)
        ArticleLink07 <- c(ArticleLink07, temp07_1)
      }
      AllArticleLinks[[length(AllArticleLinks) + 1]] <- ArticleLink07
      year = year + 1
    }
    if (year == 2006) {
      for (i in 1:length(link6)) {
        l06 <- c(l06, paste0("http://journals.plos.org", 
                             link6[[i]]))
        html06 <- getURL(l06[[i]], followlocation = TRUE)
        doc06 <- htmlParse(html06)
        temp06 <- xpathSApply(doc06, "//h3/a/@href")
        temp06_1 <- paste0("http://journals.plos.org", 
                           temp06)
        ArticleLink06 <- c(ArticleLink06, temp06_1)
      }
      AllArticleLinks[[length(AllArticleLinks) + 1]] <- ArticleLink06
      year = year + 1
    }
    if (year == 2005) {
      for (i in 1:length(link5)) {
        l05 <- c(l05, paste0("http://journals.plos.org", 
                             link5[[i]]))
        html05 <- getURL(l05[[i]], followlocation = TRUE)
        doc05 <- htmlParse(html05)
        temp05 <- xpathSApply(doc05, "//h3/a/@href")
        temp05_1 <- paste0("http://journals.plos.org", 
                           temp05)
        ArticleLink05 <- c(ArticleLink05, temp05_1)
      }
    }
  }
  title = c()
  authors = c()
  AuthorAffiliations = c()
  CorrespondenceAuthor = c()
  Email = c()
  PublicationDate = c()
  abstract = c()
  keyword = c()
  ArticleText = c()
  for (i in 1:length(AllArticleLinks)) {
    flat <- unlist(AllArticleLinks[[i]])
    for (i in 1:10) {
      html <- getURL(flat[i], followlocation = TRUE)
      doc = htmlParse(html, asText = TRUE)
      Tit = xpathSApply(doc, "//*[@id='artTitle']", xmlValue)
      if (length(Tit) == 0) 
        Tit = "NA"
      title = c(title, toString(Tit))
      Auth = xpathSApply(doc, "//*[@id='floatAuthorList']", 
                         xmlValue)
      if (length(Auth) == 0) 
        Auth = "NA"
      authors = c(authors, toString(Auth))
      AuAffiliations = xpathSApply(doc, "//*[@data-js-tooltip='tooltip_trigger']", 
                                   xmlValue)
      AuAffiliations = gsub("\n| |\\* E-mail: (.*?)\n", 
                            "", AuAffiliations)
      if (length(AuAffiliations) == 0) 
        AuAffiliations = "NA"
      AuthorAffiliations = c(AuthorAffiliations, toString(AuAffiliations))
      authCorresponding = xpathSApply(doc, "//ul[@class='author-list clearfix']/li[last()]/a[@class='author-name']", 
                                      xmlValue)
      if (length(authCorresponding) == 0) 
        authCorresponding = "NA"
      authCorresponding = gsub("   |\n", "", authCorresponding)
      CorrespondenceAuthor = c(CorrespondenceAuthor, toString(authCorresponding))
      email <- xpathSApply(doc, "//ul[@class='author-list clearfix']/li[last()]/div/p/a", 
                           xmlValue)
      if (length(email) == 0) 
        email = "NA"
      Email = c(Email, toString(email))
      PDate = xpathSApply(doc, "//*[@id='artPubDate']", 
                          xmlValue)
      PDate = gsub("Published:\\s*", "", PDate)
      if (length(PDate) == 0) 
        PDate = "NA"
      PublicationDate = c(PublicationDate, toString(PDate))
      Abst = xpathSApply(doc, "//*[@class='abstract toc-section']/*[@title='Abstract']/../p", 
                         xmlValue)
      if (length(Abst) == 0) 
        Abst = "NA"
      abstract = c(abstract, toString(Abst))
      kword = xpathSApply(doc, "string(//*[@name='keywords']/@content)")
      if (length(kword) == 0) 
        kword = "NA"
      keyword = c(keyword, toString(kword))
      artText = xpathSApply(doc, "//*[@id='artText']", 
                            xmlValue)
      if (length(artText) == 0) 
        artText = "NA"
      artText = gsub("\n", "", artText)
      ArticleText = c(ArticleText, toString(artText))
    }
  }
  display = data.frame(Title = title, Authors = authors, Author_Affiliations = AuthorAffiliations, 
                       Correspondence_Author = CorrespondenceAuthor, Correspondence_Email = Email, 
                       Publication_Date = PublicationDate, Abstract = abstract, 
                       Keywords = keyword, Full_Paper = ArticleText)
  write.xlsx(x = display, file = "test.excelfile.xlsx", sheetName = "TestSheet", 
             row.names = FALSE)
  print(display)
  length(title)
  length(authors)
  length(AuthorAffiliations)
  length(CorrespondenceAuthor)
  length(Email)
  length(PublicationDate)
  length(abstract)
  length(keyword)
  length(ArticleText)
}
