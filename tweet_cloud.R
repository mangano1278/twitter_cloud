tweet<-function(text,n=500,lan="empty")
{
if (lan=="empty") {
mach_tweets = searchTwitter(text, n)
} else {
mach_tweets = searchTwitter(text, n, lang=lan)
}
mach_text = sapply(mach_tweets, function(x) x$getText())
mach_text <- iconv(mach_text,to="utf-8-mac")
mach_corpus = Corpus(VectorSource(mach_text))
tdm = TermDocumentMatrix(mach_corpus,control = list(removePunctuation = TRUE,stopwords=c("SMART",text,"and","ill","but","youre","dont","the","for","this","that","you","are","from","http","its","like"),removeNumbers = TRUE, tolower = TRUE))
m = as.matrix(tdm)
word_freqs = sort(rowSums(m), decreasing=TRUE)
dm = data.frame(word=names(word_freqs), freq=word_freqs)
png("wordcloud.png", width=1280,height=800)
wordcloud(dm$word, dm$freq, random.order=FALSE, colors=brewer.pal(8, "Dark2"),min.freq=5)
dev.off()
}


twitter_logon<-function(key,secret)
{
cred <- OAuthFactory$new(consumerKey=key, consumerSecret=secret,requestURL='https://api.twitter.com/oauth/request_token', accessURL='https://api.twitter.com/oauth/access_token', authURL='https://api.twitter.com/oauth/authorize')
cred$handshake(cainfo="cacert.pem")
registerTwitterOAuth(cred)
}
