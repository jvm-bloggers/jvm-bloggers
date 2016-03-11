package pl.tomaszdziurko.jvm_bloggers.mailing;

import static pl.tomaszdziurko.jvm_bloggers.utils.UriUtmComponentsBuilder.DEFAULT_UTM_CAMPAING;
import static pl.tomaszdziurko.jvm_bloggers.utils.UriUtmComponentsBuilder.DEFAULT_UTM_SOURCE;

import com.google.common.base.Preconditions;
import lombok.Getter;
import pl.tomaszdziurko.jvm_bloggers.blog_posts.domain.BlogPost;
import pl.tomaszdziurko.jvm_bloggers.blogs.domain.Blog;
import pl.tomaszdziurko.jvm_bloggers.utils.UriUtmComponentsBuilder;

@Getter
class BlogPostForMailItem {
    
    private static final String UTM_MEDIUM = "newsletter";
    
    private String title;
    private String url;
    private String authorLabel;
    private Long issueNumber;

    public static Builder builder() {
        return new Builder();
    }
    
    public static class Builder {

        private final BlogPostForMailItem instance = new BlogPostForMailItem();

        public Builder from(BlogPost blogPost) {
            withTitle(blogPost.getTitle());
            withUrl(blogPost.getUrl());
            withAuthorLabel(blogPost.getBlog());
            return this;
        }
        
        public Builder withTitle(String title) {
            instance.title = title;
            return this;
        }

        public Builder withUrl(String url) {
            instance.url = url;
            return this;
        }
        
        public Builder withAuthorLabel(Blog blog) {
            instance.authorLabel = determineAuthorLabel(blog);
            return this;
        }
        
        public Builder withIssueNumber(long issueNumber) {
            instance.issueNumber = issueNumber;
            return this;
        }
        
        public Builder withDefaultUTMParameters() {
            Preconditions.checkState(
                instance.issueNumber != null,
                "Could not set default UTM parameters. Missing issueNumber."
            );
            
            instance.url = UriUtmComponentsBuilder.fromHttpUrl(instance.url)
                .withSource(DEFAULT_UTM_SOURCE)
                .withMedium(UTM_MEDIUM)
                .withCampaign(String.format("%s#%s", DEFAULT_UTM_CAMPAING, instance.issueNumber))
                .build();
            
            return this;
        }
        
        public BlogPostForMailItem build() {
            Preconditions.checkState(instance.title != null, "Tittle cannot be null");
            Preconditions.checkState(instance.url != null, "Url cannot be null");
            Preconditions.checkState(instance.authorLabel != null, "Author cannot be null");
            
            return instance;
        }

        private String determineAuthorLabel(Blog blogger) {
            if (blogger.getTwitter() != null) {
                return String.format(
                    "<a href=\"https://twitter.com/%s\">%s</a>", 
                    blogger.getTwitter().substring(1),
                    blogger.getAuthor()
                );
            } else {
                return blogger.getAuthor();
            }
        }
    }
}
