UPDATE metadata SET value=
'Poniżej lista nowych postów z ostatnich $days$ dni.<br><br>

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
 <br>
 $endif$

 $if(newVideoPosts)$
 Nowe nagrania:<br>
 $newVideoPosts: {post|
 - <a href="$post.url$">$post.title$</a> by $post.authorLabel$<br>
 }$
 <br>
 <br>
 $endif$

 $if(newlyAddedBlogs)$
 Nowo dodane blogi: <br/>
 $newlyAddedBlogs: { blog |
 - <a href="$blog.url$">$blog.author$</a><br/>
 }$
 $endif$
 ' WHERE name='DEFAULT_MAILING_TEMPLATE';


UPDATE metadata SET value=
'Poniżej lista nowych postów z ostatnich $days$ dni.<br><br>

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
 <br>
 $endif$

 $if(newVideoPosts)$
 Nowe nagrania:<br>
 $newVideoPosts: {post|
 - <a href="$post.url$">$post.title$</a> by $post.authorLabel$<br>
 }$
 <br>
 <br>
 $endif$

 $if(newlyAddedBlogs)$
 Nowo dodane blogi: <br/>
 $newlyAddedBlogs: { blog |
 - <a href="$blog.url$">$blog.author$</a><br/>
 }$
 $endif$
 ' WHERE name='MAILING_TEMPLATE';

INSERT INTO metadata values(nextval('setting_seq'), 'MAILING_GREETING',
                            'Witajcie,<br/<br/><br/>');

INSERT INTO metadata values(nextval('setting_seq'), 'MAILING_SIGNATURE',
'Miłej lektury,<br/>
Tomek Dziurko, JVM Bloggers v. 0.9.0 - <a href="http://jvm-bloggers.com/pl/rss">RSS</a><br/>
<a href="https://github.com/tdziurko/jvm-bloggers/">Fork me on GitHub</a><br/>');

INSERT INTO metadata values(nextval('setting_seq'), 'HEADING_TEMPLATE', '');
INSERT INTO metadata values(nextval('setting_seq'), 'VARIA_TEMPLATE', '');