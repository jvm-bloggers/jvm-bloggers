package pl.tomaszdziurko.jvm_bloggers.mailing;

import com.google.common.base.Preconditions;
import java.text.SimpleDateFormat;
import java.util.Date;
import lombok.Getter;
import org.springframework.web.util.UriComponentsBuilder;
import pl.tomaszdziurko.jvm_bloggers.UTMParameters;
import pl.tomaszdziurko.jvm_bloggers.blog_posts.domain.BlogPost;
import pl.tomaszdziurko.jvm_bloggers.blogs.domain.Blog;

/**
 * {@link BlogPost} representation for email newsletter template purpose.
 */
@Getter
class BlogPostForMailItem {
    
    private static final String UTM_SOURCE = "jvmbloggers";
    private static final String UTM_MEDIUM = "newsletter";
    
    private String title;
    private String url;
    private String authorLabel;

    /**
     * Returns internal builder.
     * 
     * @return builder
     */
    public static Builder builder() {
        return new Builder();
    }
    
    /**
     * Builder for {@link BlogPostForMailItem}
     */
    public static class Builder {

        private final BlogPostForMailItem instance = new BlogPostForMailItem();

        public Builder from(BlogPost blogPost) {
            instance.title = blogPost.getTitle();
            instance.url = blogPost.getUrl();
            instance.authorLabel = determineAuthorLabel(blogPost.getBlog());
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
        
        public Builder withUrlParameter(String name, String value) {
            Preconditions.checkState(
                instance.url != null,
                "Url could not be null. Please set url first"
            );
            
            instance.url = UriComponentsBuilder
                .fromHttpUrl(instance.url)
                .queryParam(name, value)
                .build().toString();
            
            return this;
        }
        
        public Builder withDefaultUTMParameters() {
            withUrlParameter(UTMParameters.UTM_SOURCE_KEY, UTM_SOURCE);
            withUrlParameter(UTMParameters.UTM_MEDIUM_KEY, UTM_MEDIUM);
            withUrlParameter(UTMParameters.UTM_CAMPAING_KEY,
                String.format(
                    "%s#%s", UTM_SOURCE,
                    new SimpleDateFormat("yyyy-mm-dd").format(new Date())
                )
            );
            
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
