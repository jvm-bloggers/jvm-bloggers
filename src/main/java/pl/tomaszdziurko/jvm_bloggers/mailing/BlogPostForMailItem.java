package pl.tomaszdziurko.jvm_bloggers.mailing;

import com.google.common.base.Preconditions;
import java.text.SimpleDateFormat;
import java.util.Date;
import lombok.Getter;
import org.springframework.web.util.UriComponentsBuilder;
import pl.tomaszdziurko.jvm_bloggers.UTMParameters;
import pl.tomaszdziurko.jvm_bloggers.blog_posts.domain.BlogPost;
import pl.tomaszdziurko.jvm_bloggers.blogs.domain.Blog;

@Getter
class BlogPostForMailItem {
    
    private static final String UTM_SOURCE = "jvm-bloggers.com";
    private static final String UTM_CAMPAING = "jvm-bloggers";
    private static final String UTM_MEDIUM = "newsletter";
    
    private String title;
    private String url;
    private String authorLabel;

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
                    "%s#%s", UTM_CAMPAING,
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
