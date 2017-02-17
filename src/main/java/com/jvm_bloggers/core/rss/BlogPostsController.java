package com.jvm_bloggers.core.rss;

import com.rometools.rome.io.SyndFeedOutput;
import lombok.RequiredArgsConstructor;
import lombok.SneakyThrows;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import java.io.PrintWriter;
import java.util.Set;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import static com.google.common.base.MoreObjects.firstNonNull;

@RestController
@RequiredArgsConstructor(onConstructor = @__(@Autowired))
public class BlogPostsController {

    private final AggregatedRssFeedProducer rssProducer;

    @SneakyThrows
    @RequestMapping("/pl/rss")
    public void getRss(HttpServletRequest request, HttpServletResponse response,
        PrintWriter writer, @RequestParam(required = false) Integer limit,
        @Value("${generated.rss.entries.limit}") int defaultLimit,
        @RequestParam(required = false) Set<String> excludedAuthors) {
        limit = firstNonNull(limit, defaultLimit);
        response.setContentType(MediaType.APPLICATION_ATOM_XML_VALUE);
        new SyndFeedOutput().output(
            rssProducer.getRss(request.getRequestURL().toString(), limit, excludedAuthors),
            writer
        );
    }

}
