package pl.tomaszdziurko.jvm_bloggers.blog_posts;

import com.rometools.rome.io.SyndFeedOutput;

import lombok.RequiredArgsConstructor;
import lombok.SneakyThrows;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.io.PrintWriter;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

@RestController
@RequiredArgsConstructor(onConstructor = @__(@Autowired))
public class BlogPostsController {

    private final AggregatedRssFeedProducer rssProducer;

    @SneakyThrows
    @RequestMapping("/pl/rss")
    public void getRss(HttpServletRequest request, HttpServletResponse response,
            PrintWriter writer) {
        response.setContentType(MediaType.APPLICATION_ATOM_XML_VALUE);
        new SyndFeedOutput().output(
            rssProducer.getRss(request.getRequestURL().toString()),
            writer
        );
    }

}
