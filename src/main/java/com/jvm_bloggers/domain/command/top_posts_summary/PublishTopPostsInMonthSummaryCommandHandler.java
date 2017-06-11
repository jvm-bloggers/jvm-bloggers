package com.jvm_bloggers.domain.command.top_posts_summary;

import com.jvm_bloggers.domain.command.CommandHandler;
import com.jvm_bloggers.entities.blog_post.BlogPost;
import com.jvm_bloggers.entities.blog_post.BlogPostRepository;
import com.jvm_bloggers.entities.click.PostIdWithCount;
import com.jvm_bloggers.entities.top_posts_summary.PopularCompanyPost;
import com.jvm_bloggers.entities.top_posts_summary.PopularPersonalPost;
import com.jvm_bloggers.entities.top_posts_summary.TopPostsSummary;
import com.jvm_bloggers.entities.top_posts_summary.TopPostsSummaryRepository;
import com.jvm_bloggers.utils.NowProvider;
import javaslang.collection.List;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.event.EventListener;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
@Slf4j
@RequiredArgsConstructor(onConstructor = @__(@Autowired))
class PublishTopPostsInMonthSummaryCommandHandler
    implements CommandHandler<PublishTopPostsInMonthSummary> {

    private final TopPostsSummaryRepository topPostsSummaryRepository;
    private final BlogPostRepository blogPostRepository;
    private final NowProvider nowProvider;

    @Override
    @EventListener
    @Transactional
    public void handle(PublishTopPostsInMonthSummary command) {

        List<PopularPersonalPost> personalPosts = command
            .getBestPersonalPosts()
            .zipWithIndex()
            .map(postWithIndex -> {
                long position = postWithIndex._2 + 1;
                PostIdWithCount postIdWithCount = postWithIndex._1;
                BlogPost post = blogPostRepository.getOne(postIdWithCount.getBlogPostId());
                return new PopularPersonalPost(post, position, postIdWithCount.getCount());
            });

        List<PopularCompanyPost> companyPosts = command
            .getBestCompanyPosts()
            .zipWithIndex()
            .map(postWithIndex -> {
                long position = postWithIndex._2 + 1;
                PostIdWithCount postIdWithCount = postWithIndex._1;
                BlogPost post = blogPostRepository.getOne(postIdWithCount.getBlogPostId());
                return new PopularCompanyPost(post, position, postIdWithCount.getCount());
            });

        TopPostsSummary topPostsSummary = new TopPostsSummary(
            command.getYearMonth().getYear(),
            command.getYearMonth().getMonthValue(),
            nowProvider.now(),
            personalPosts.toJavaList(),
            companyPosts.toJavaList()
        );
        topPostsSummaryRepository.save(topPostsSummary);
    }

}
