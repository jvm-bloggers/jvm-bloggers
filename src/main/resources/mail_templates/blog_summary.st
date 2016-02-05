Witajcie<br/><br/><br/>


Varia:<br/>
 - trwają Call 4 Papers na następujące konferencje: <a href="http://2016.geecon.org/">GeeCON</a> (do 19 lutego),
 <a href="http://2016.4developers.org.pl/">4Developers</a> (do 22 lutego) oraz
 <a href="http://devcrowd.pl/">DevCrowd</a> (do 1 marca), zachęcam do wysyłania zgłoszeń i dzielenia się wiedzą :)<br/>
 - bloguj regularnie i wygraj m.in. legendarne krzesło <b>Aeron</b>, równie legendarną klawiaturę <b>Das Keyboard</b> albo konsolę XBox! O co chodzi?<br/>
 Jeden z bardziej znanych programistów ze świata .Net-a Maciej Aniserowicz ogłasza konkurs, który polega na regularnym blogowaniu o swoim (już istniejącym lub dopiero startującym)
 projekcie open source. Więcej szczegółów oraz pełna lista nagród na stronie <a href="http://www.maciejaniserowicz.com/daj-sie-poznac/">Daj Się Poznać 2016</a>. Zachęcam do udziału!

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




