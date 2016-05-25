package pl.tomaszdziurko.jvm_bloggers.mailing;

import com.google.common.base.Preconditions;

import lombok.Getter;

import org.apache.commons.lang3.StringUtils;
import pl.tomaszdziurko.jvm_bloggers.blog_posts.domain.BlogPost;
import pl.tomaszdziurko.jvm_bloggers.blogs.domain.Blog;

@Getter
class BlogPostForMailItem {
    
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

        public BlogPostForMailItem build() {
            Preconditions.checkState(StringUtils.isNotBlank(instance.title),
                    "Title cannot be NULL nor empty.");
            Preconditions.checkState(StringUtils.isNotBlank(instance.url),
                    "Url cannot be NULL nor empty.");
            Preconditions.checkState(StringUtils.isNotBlank(instance.authorLabel),
                    "Author cannot be NULL nor empty.");
            return instance;
        }

        private String determineAuthorLabel(Blog blogger) {
            if (StringUtils.isNotBlank(blogger.getTwitter())) {
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

