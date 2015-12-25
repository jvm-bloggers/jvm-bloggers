Witajcie Świątecznie<br/><br/><br/>


Z okazji trwaących Świąt życzę Wam czystego kodu, prawdziwie zielonych buildów oraz samych<br/>
wartościowych blog postów.<br/><br/>

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
Projekt JVM Bloggers v. 0.5.0<br/>
<a href="https://github.com/tdziurko/jvm-bloggers/">Fork me on GitHub</a><br/>




