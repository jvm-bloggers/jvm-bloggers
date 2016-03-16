INSERT INTO setting(id, name, value)
            VALUES (nextval('setting_seq'), 'DEFAULT_MAILING_TEMPLATE',

'Witajcie<br/><br/><br/>


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
$if(blogsWithHomePage.(key).available)$
- <a href="$blogsWithHomePage.(key).url$">$key.author$</a><br/>
$else$
- $key.author$ <br/>
$endif$
}$
$endif$
-- <br/>
Miłej lektury,<br/>
Tomek Dziurko, JVM Bloggers v. 0.5.0<br/>
<a href="https://github.com/tdziurko/jvm-bloggers/">Fork me on GitHub</a><br/>');