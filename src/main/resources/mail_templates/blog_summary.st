Witajcie <br/><br/><br/>


Poniżej lista nowych postów z ostatnich $days$ dni na blogach polskich programistów JVM:<br/><br/>
$newPosts: {post|
 - <a href="$post.url$">$post.title$</a> by $post.authorLabel$<br/>
}$
<br/>
<br/>

$if(personToBlogHomepage)$
Nowo dodane blogi: <br/><br/>
$personToBlogHomepage.keys: { key |
 - <a href="$personToBlogHomepage.(key)$">$key.author$</a><br/>
}$
$endif$
-- <br/>
Miłej lektury,<br/>
Projekt JVM Bloggers v. 0.5.0<br/>
<a href="https://github.com/tdziurko/jvm-bloggers/">Fork me on GitHub</a><br/>




