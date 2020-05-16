
UPDATE metadata SET value=
'Poniżej lista nowych postów z ostatnich $days$ dni.<br>'
||
'$if(newPersonalPosts)$&nbsp;<div>Wpisy programistów:<br>
$newPersonalPosts: {post|
- <a href="$post.url$">$post.title$</a> by $post.authorLabel$<br>
}$
$endif$'
||
'<div>$if(newCompanyPosts)$</div><div><br></div><div>Wpisy z blogów firmowych:<br>
$newCompanyPosts: {post|
- <a href="$post.url$">$post.title$</a> by $post.authorLabel$<br>
}$$endif$&nbsp;</div>'
||
'<div>$if(newPresentationPosts)$</div><div><br></div><div>Nowe prezentacje:<br>
$newPresentationPosts: {post|
- <a href="$post.url$">$post.title$</a> by $post.authorLabel$<br>
}$$endif$</div>'
||
'<div>$if(newPodcastPosts)$</div><div><br></div><div>Nowe podcasty:<br>
$newPodcastPosts: {post|
- <a href="$post.url$">$post.title$</a> by $post.authorLabel$<br>
}$$endif$</div>'
||
'<div>$if(newlyAddedBlogs)$</div><div><br></div><div>Nowo dodane blogi i kanały video: <br>
$newlyAddedBlogs: { blog |
- <a href="$blog.url$">$blog.author$</a><br>
}$$endif$</div></div>'
WHERE name='DEFAULT_MAILING_TEMPLATE' OR name='MAILING_TEMPLATE';