Witajcie<br/><br/><br/>


Varia:<br/>
 - na Courserze rusza kurs <a href="https://www.coursera.org/learn/advanced-data-structures">Advanced Data Structures in Java</a>, zachęcam do zapisania się<br/>
 - trwają Call 4 Papers na dwie polskie konferencje: <a href="http://scalar-conf.com/">Scalar</a> (do 31 stycznia) oraz <a href="http://2016.geecon.org/">GeeCON</a> (do 19 lutego), zachęcam do wysyłania zgłoszeń<br/>

<br/><br/>
Poniżej lista nowych postów z ostatnich $days$ dni.<br/><br/>

$if(newPosts)$
Wpisy programistów:<br/>
$newPosts: {post|
 - <a href="$post.url$">$post.title$</a> by $post.authorLabel$<br/>
}$
<br/>
<br/>
$endif$

$if(newPostsFromCompanies)$
Wpisy z blogów firmowych:<br/>
$newPostsFromCompanies: {post|
 - <a href="$post.url$">$post.title$</a> by $post.authorLabel$<br/>
}$
<br/>
<br/>
$endif$

$if(blogsWithHomePage)$
Nowo dodane blogi: <br/>
$blogsWithHomePage.keys: { key |
 - <a href="$blogsWithHomePage.(key)$">$key.author$</a><br/>
}$
$endif$
-- <br/>
Miłej lektury,<br/>
Tomek Dziurko, JVM Bloggers v. 0.5.0<br/>
<a href="https://github.com/tdziurko/jvm-bloggers/">Fork me on GitHub</a><br/>




