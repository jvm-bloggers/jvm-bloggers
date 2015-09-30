Witajcie <br/><br/><br/>


Poniżej lista nowych postów z ostatnich $days$ dni na blogach polskich programistów JVM:<br/><br/>
$newPosts: {post|
 - <a href="$post.url$">$post.title$</a> od $post.authorLabel$<br/>
}$
<br/>
<br/>

$if(personToBlogHomepage)$
Miło nam poinformować, że od tej pory będziemy Was także informować o postach na blogach: <br/><br/>
$personToBlogHomepage.keys: { key |
 - <a href="$personToBlogHomepage.(key)$">$key.name$</a><br/>
}$
$endif$
-- <br/>
Miłej lektury,<br/>
Projekt JVM Bloggers v. 0.2.0<br/>
<a href="https://github.com/tdziurko/jvm-bloggers/">Fork me on GitHub</a><br/>




