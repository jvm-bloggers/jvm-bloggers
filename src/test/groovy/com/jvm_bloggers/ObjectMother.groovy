package com.jvm_bloggers

import com.jvm_bloggers.entities.blog.Blog
import com.jvm_bloggers.entities.blog_post.BlogPost

import java.time.LocalDateTime

import static com.jvm_bloggers.entities.blog.BlogType.PERSONAL

class ObjectMother {

    static Blog aBlog(Map params = [:]) {
        String id = UUID.randomUUID().toString()
        [
                bookmarkableId    : "bookmarkableId $id".subSequence(0,29),
                author            : 'author',
                rss               : "http://example.com/feed/$id",
                url               : "http://example.com/$id",
                dateAdded         : LocalDateTime.now(),
                blogType          : PERSONAL,
                active            : true,
                moderationRequired: false,
                twitter           : '@blogging'
        ] + params as Blog
    }

    static BlogPost aBlogPost(Map params = [:]) {
        String id = UUID.randomUUID().toString()
        [
                publishedDate   : LocalDateTime.now(),
                blog            : aBlog(),
                title           : "title $id",
                url             : "http://example.com/posts/$id",
                approved        : true,
                description     : "Example description"
        ] + params as BlogPost
    }
}
