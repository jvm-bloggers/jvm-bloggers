Witajcie w 2016 roku!<br/><br/><br/>


Zanim przejdziemy do nowych wpisów, garść linków podsumowujących stary, 2015 rok:<br/>
 - <a href="http://mailer.dzone.com/display.php?M=495714&C=22a541220f1acabb18e2f2af59ec7194&S=6969&L=3&N=3821">DZone - Best of 2015</a><br/>
 - <a href="http://www.javacodegeeks.com/2015/12/top-10-javacodegeeks-posts-for-2015.html">Top 10 JavaCodeGeeks posts for 2015</a><br/>

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




