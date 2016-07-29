package pl.tomaszdziurko.jvm_bloggers.blog_posts.data_migration;


import lombok.RequiredArgsConstructor;

import org.apache.commons.lang3.RandomStringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.PageRequest;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

import pl.tomaszdziurko.jvm_bloggers.blog_posts.domain.BlogPost;
import pl.tomaszdziurko.jvm_bloggers.blog_posts.domain.BlogPostRepository;

import java.util.List;

@Component
@RequiredArgsConstructor(onConstructor = @__(@Autowired))
class BlogPostUidShortener {

    private final BlogPostRepository blogPostRepository;

    @Transactional
    int shortenUUids(int batchSize) {
        List<BlogPost> postsToShortenUid = blogPostRepository
                .findPostsWithUidLongerThan(BlogPost.UID_LENGTH, new PageRequest(0, batchSize));

        postsToShortenUid.stream().forEach(post -> {
            post.setUid(RandomStringUtils.randomAlphanumeric(BlogPost.UID_LENGTH));
        });
        return postsToShortenUid.size();
    }
}
