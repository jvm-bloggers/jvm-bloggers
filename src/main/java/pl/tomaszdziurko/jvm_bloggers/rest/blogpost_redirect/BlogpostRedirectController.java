package pl.tomaszdziurko.jvm_bloggers.rest.blogpost_redirect;

import lombok.AllArgsConstructor;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RestController;

import pl.tomaszdziurko.jvm_bloggers.blog_posts.domain.BlogPost;
import pl.tomaszdziurko.jvm_bloggers.blog_posts.domain.BlogPostRepository;

import java.io.IOException;
import java.util.Optional;

import javax.servlet.http.HttpServletResponse;

/**
 * @author Mateusz Urbański <matek2305@gmail.com>
 */
@RestController
@RequestMapping(path = "/blogpost/r")
@AllArgsConstructor(onConstructor = @__(@Autowired))
public class BlogpostRedirectController {

    private final BlogPostRepository blogPostRepository;

    @RequestMapping(value = "/{uid}", method = RequestMethod.GET)
    public void redirect(HttpServletResponse response, @PathVariable String uid) {
        Optional<BlogPost> blogPost = blogPostRepository.findByUid(uid);
        if (blogPost.isPresent()) {
            redirectToBlogPost(response, blogPost.get());
        } else {
            response.setStatus(HttpServletResponse.SC_NOT_FOUND);
        }
    }

    private void redirectToBlogPost(HttpServletResponse response, BlogPost blogPost) {
        try {
            response.sendRedirect(blogPost.getUrl());
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }
}
