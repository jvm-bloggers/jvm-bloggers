package com.jvm_bloggers.core.blogpost_redirect;

import akka.actor.ActorRef;
import akka.actor.ActorSystem;
import akka.routing.RoundRobinPool;
import com.google.common.base.Stopwatch;
import com.jvm_bloggers.core.blogpost_redirect.click_counter.ClicksStoringActor;
import com.jvm_bloggers.core.blogpost_redirect.click_counter.SingleClick;
import com.jvm_bloggers.core.utils.UriUtmComponentsBuilder;
import com.jvm_bloggers.entities.blog_post.BlogPost;
import com.jvm_bloggers.entities.blog_post.BlogPostRepository;
import com.jvm_bloggers.entities.click.ClickRepository;
import com.jvm_bloggers.utils.NowProvider;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RestController;

import java.io.IOException;
import java.util.Optional;
import java.util.concurrent.TimeUnit;

import javax.servlet.http.HttpServletResponse;

import static akka.actor.ActorRef.noSender;

@RestController
@RequestMapping(path = RedirectController.REDIRECT_URL_PATH)
@Slf4j
public class RedirectController {

    private static final String UTM_MEDIUM = "link";
    private static final int MAX_ALLOWED_EXECUTION_TIME_IN_MILLIS = 1000;
    public static final String REDIRECT_URL_PATH = "/r";

    private final BlogPostRepository blogPostRepository;
    private final ActorRef actorRef;

    @Autowired
    public RedirectController(BlogPostRepository blogPostRepository,
                              ClickRepository clickRepository,
                              ActorSystem actorSystem,
                              NowProvider nowProvider) {
        this.blogPostRepository = blogPostRepository;
        this.actorRef = actorSystem.actorOf(
            new RoundRobinPool(3).props(ClicksStoringActor.props(clickRepository, nowProvider)),
            "clicksStoringActor"
        );
    }

    @RequestMapping(value = "/{uid}", method = RequestMethod.GET)
    public void redirectToBlogPostWithUid(HttpServletResponse response, @PathVariable String uid) {
        Stopwatch stopwatch = Stopwatch.createStarted();
        Optional<BlogPost> blogPost = blogPostRepository.findByUid(uid);
        if (blogPost.isPresent()) {
            actorRef.tell(new SingleClick(blogPost.get()), noSender());
            redirectToBlogPost(response, blogPost.get());
            long executionTime = stopwatch.elapsed(TimeUnit.MILLISECONDS);
            if (executionTime > MAX_ALLOWED_EXECUTION_TIME_IN_MILLIS) {
                log.warn(
                    "Execution time {} ms longer than acceptable {} ms",
                    executionTime,
                    MAX_ALLOWED_EXECUTION_TIME_IN_MILLIS
                );
            }
        } else {
            response.setStatus(HttpServletResponse.SC_NOT_FOUND);
        }
    }

    private void redirectToBlogPost(HttpServletResponse response, BlogPost blogPost) {
        try {
            response.sendRedirect(UriUtmComponentsBuilder.fromHttpUrl(blogPost.getUrl())
                .withSource(UriUtmComponentsBuilder.DEFAULT_UTM_SOURCE)
                .withMedium(UTM_MEDIUM)
                .withCampaign(UriUtmComponentsBuilder.DEFAULT_UTM_CAMPAING)
                .build());
        } catch (IOException ex) {
            throw new RuntimeException("Error while sending redirect to " + blogPost.getUrl(), ex);
        }
    }
}
