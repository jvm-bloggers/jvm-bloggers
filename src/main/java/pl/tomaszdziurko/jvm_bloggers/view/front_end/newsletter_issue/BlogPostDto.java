package pl.tomaszdziurko.jvm_bloggers.view.front_end.newsletter_issue;

import lombok.Builder;

@Builder
public class BlogPostDto {

    public String url;
    public String title;
    public String authorName;
    public String authorTwitterHandle;
    public BlogTypeDto blogType;

}
