package com.jvm_bloggers.core.blogpost_redirect;

import com.google.common.base.Stopwatch;
import com.jvm_bloggers.core.blogpost_redirect.click_counter.ClickStoringService;
import com.jvm_bloggers.core.blogpost_redirect.click_counter.SingleClick;
import com.jvm_bloggers.core.blogpost_redirect.click_counter.SingleClick.Referer;
import com.jvm_bloggers.core.blogpost_redirect.click_counter.SingleClick.UserAgent;
import com.jvm_bloggers.core.utils.UriUtmComponentsBuilder;
import com.jvm_bloggers.entities.blog_post.BlogPost;
import com.jvm_bloggers.entities.blog_post.BlogPostRepository;
import io.vavr.control.Option;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.io.IOException;
import java.util.concurrent.TimeUnit;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

@RestController
@RequestMapping(path = RedirectController.REDIRECT_URL_PATH)
@Slf4j
public class RedirectController {

    private static final String UTM_MEDIUM = "link";
    private static final int MAX_ALLOWED_EXECUTION_TIME_IN_MILLIS = 1000;
    public static final String REDIRECT_URL_PATH = "/r";

    private final BlogPostRepository blogPostRepository;
    private final String applicationBaseUrl;
    private final ClickStoringService clickStoringService;

    @Autowired
    public RedirectController(BlogPostRepository blogPostRepository,
                              ClickStoringService clickStoringService,
                              @Value("${application.baseUrl}") String applicationBaseUrl) {
        this.blogPostRepository = blogPostRepository;
        this.clickStoringService = clickStoringService;
        this.applicationBaseUrl = applicationBaseUrl;
    }

    @GetMapping(value = "/{uid}")
    public void redirectToBlogPostWithUid(
        HttpServletRequest request,
        HttpServletResponse response,
        @PathVariable String uid
    ) {
        Stopwatch stopwatch = Stopwatch.createStarted();
        Option<BlogPost> blogPost = blogPostRepository.findByUid(uid);
        if (blogPost.isDefined()) {
            clickStoringService.persistClick(buildSingleClick(blogPost.get(), request));
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
            try {
                response.sendRedirect(applicationBaseUrl);
            } catch (IOException ex) {
                throw new RuntimeException(
                    "Error while sending redirect to " + applicationBaseUrl,
                    ex
                );
            }
        }
    }

    private SingleClick buildSingleClick(BlogPost blogPost, HttpServletRequest request) {
        return new SingleClick(
            blogPost,
            new Referer(request.getHeader("referer")),
            new UserAgent(request.getHeader("user-agent")),
            new SingleClick.IpAddress(request.getRemoteAddr())
        );
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
