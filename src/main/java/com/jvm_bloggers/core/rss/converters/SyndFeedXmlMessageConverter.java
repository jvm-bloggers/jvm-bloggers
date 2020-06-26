package com.jvm_bloggers.core.rss.converters;

import com.rometools.rome.feed.synd.SyndFeed;
import com.rometools.rome.io.SyndFeedOutput;

import lombok.SneakyThrows;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpInputMessage;
import org.springframework.http.HttpOutputMessage;
import org.springframework.http.converter.AbstractHttpMessageConverter;
import org.springframework.http.converter.HttpMessageNotReadableException;
import org.springframework.http.converter.HttpMessageNotWritableException;
import org.springframework.stereotype.Component;

import java.io.IOException;
import java.io.PrintWriter;
import java.io.Writer;

import static org.springframework.http.MediaType.APPLICATION_ATOM_XML;

@Component
public class SyndFeedXmlMessageConverter extends AbstractHttpMessageConverter<SyndFeed> {

    @Autowired
    public SyndFeedXmlMessageConverter() {
        super(APPLICATION_ATOM_XML);
    }

    @Override
    protected boolean supports(Class<?> clazz) {
        return SyndFeed.class.isAssignableFrom(clazz);
    }

    @Override
    protected SyndFeed readInternal(Class<? extends SyndFeed> clazz,
                                    HttpInputMessage inputMessage) {
        throw new UnsupportedOperationException(
            "Reading SyndFeed from a XML file is currently not supported!");
    }

    @Override
    @SneakyThrows
    protected void writeInternal(SyndFeed syndFeed, HttpOutputMessage outputMessage) {

        Writer writer = new PrintWriter(outputMessage.getBody());
        new SyndFeedOutput().output(syndFeed, writer);
    }
}
