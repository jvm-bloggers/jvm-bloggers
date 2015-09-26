package pl.tomaszdziurko.jvm_bloggers.mailing;

import lombok.Data;
import pl.tomaszdziurko.jvm_bloggers.blog_posts.domain.BlogPost;
import pl.tomaszdziurko.jvm_bloggers.people.domain.Person;


@Data
class BlogPostForMailItem {

    private String title;
    private String url;
    private String authorLabel;

    public BlogPostForMailItem(BlogPost blogPost) {
        this.title = blogPost.getTitle();
        this.url = blogPost.getUrl();
        this.authorLabel = determineAuthorLabel(blogPost.getAuthor());
    }

    private String determineAuthorLabel(Person blogger) {
        if (blogger.getTwitter() != null) {
            return "<a href=\"https://twitter.com/" + blogger.getTwitter().substring(1) + "\">" + blogger.getName() + "</a>";
        } else {
            return blogger.getName();
        }
    }

}
