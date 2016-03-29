UPDATE metadata SET value = 'Witajcie<br><br><br>


 Varia:<br>
 - trwają Call 4 Papers na następujące konferencje: <br>
 -- <a href="http://2016.geecon.org/">GeeCON</a> (do 19 lutego),<br>
 -- <a href="http://2016.4developers.org.pl/">4Developers</a> (do 22 lutego)<br>
 -- <a href="http://devcrowd.pl/">DevCrowd</a> (do 1 marca)<br>
 -- <a href="http://warsaw.gr8days.pl/#/">Gr8Day Warsaw</a><br>
 -- <a href="http://chamberconf.pl/">ChamberConf</a><br> (do 14 lutego)
 Zachęcam do wysyłania zgłoszeń i dzielenia się wiedzą :)<br><br>
 - bloguj regularnie i wygraj m.in. legendarne krzesło <b>Aeron</b>, równie legendarną klawiaturę <b>Das Keyboard</b> albo konsolę XBox! O co chodzi?<br>
 Jeden z bardziej znanych programistów ze świata .Net-a Maciej Aniserowicz ogłasza konkurs, który polega na regularnym blogowaniu o swoim (już istniejącym lub dopiero startującym)
 projekcie open source. Więcej szczegółów oraz pełna lista nagród na stronie <a href="http://www.maciejaniserowicz.com/daj-sie-poznac/">Daj Się Poznać 2016</a>. Zachęcam do udziału!

 <br><br><br>
 Poniżej lista nowych postów z ostatnich $days$ dni.<br><br>

 $if(newPosts)$
 Wpisy programistów:<br>
 $newPosts: {post|
 - <a href="$post.url$">$post.title$</a> by $post.authorLabel$<br>
 }$
 <br>
 <br>
 $endif$

 $if(newPostsFromCompanies)$
 Wpisy z blogów firmowych:<br>
 $newPostsFromCompanies: {post|
 - <a href="$post.url$">$post.title$</a> by $post.authorLabel$<br>
 }$
 <br>
 <br>$endif$ $if(newVideoPosts)$ Wpisy z wideo blogów:<br>$newVideoPosts: {post| -&nbsp;<a href="http://localhost:8080/$post.url$">$post.title$</a>&nbsp;by $post.authorLabel$<br>}$&nbsp;<br>
 <br>
 $endif$

 $if(blogsWithHomePage)$
 Nowo dodane blogi: <br>
 $blogsWithHomePage.keys: { key |
 - <a href="$blogsWithHomePage.(key)$">$key.author$</a><br>
 }$
 $endif$
 -- <br>
 Miłej lektury,<br>
 Tomek Dziurko, JVM Bloggers v. 0.5.0<br>
 <a href="https://github.com/tdziurko/jvm-bloggers/">Fork me on GitHub</a><br>' WHERE name = 'MAILING_TEMPLATE';