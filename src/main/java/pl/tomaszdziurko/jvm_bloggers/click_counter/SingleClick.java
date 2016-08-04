package pl.tomaszdziurko.jvm_bloggers.click_counter;

import lombok.Getter;
import lombok.NonNull;
import lombok.RequiredArgsConstructor;

import pl.tomaszdziurko.jvm_bloggers.blog_posts.domain.BlogPost;

@RequiredArgsConstructor
@Getter
public class SingleClick {

    @NonNull
    private final BlogPost blogPost;

}
