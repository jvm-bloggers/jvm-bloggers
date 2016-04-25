UPDATE metadata SET value=
'Witajcie<br><br><br>

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
 -- <br/>
 Miłej lektury,<br/>
 Tomek Dziurko, JVM Bloggers v. 0.5.0<br/>
 <a href="https://github.com/tdziurko/jvm-bloggers/">Fork me on GitHub</a><br/>' WHERE name='DEFAULT_MAILING_TEMPLATE';