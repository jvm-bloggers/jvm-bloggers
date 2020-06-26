package com.jvm_bloggers.core.rss.converters;

import com.github.openjson.JSONObject;
import com.rometools.rome.feed.synd.SyndFeed;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpInputMessage;
import org.springframework.http.HttpOutputMessage;
import org.springframework.http.converter.AbstractHttpMessageConverter;
import org.springframework.http.converter.HttpMessageNotReadableException;
import org.springframework.http.converter.HttpMessageNotWritableException;
import org.springframework.stereotype.Component;

import java.io.IOException;

import static org.springframework.http.MediaType.APPLICATION_JSON;
import static org.springframework.http.MediaType.APPLICATION_JSON_UTF8;

@Component
public class SyndFeedJsonMessageConverter extends AbstractHttpMessageConverter<SyndFeed> {

    private final SyndFeedToJsonConverter converter;

    @Autowired
    public SyndFeedJsonMessageConverter(SyndFeedToJsonConverter converter) {
        super(APPLICATION_JSON, APPLICATION_JSON_UTF8);

        this.converter = converter;
    }

    @Override
    protected boolean supports(Class<?> clazz) {
        return SyndFeed.class.isAssignableFrom(clazz);
    }

    @Override
    protected SyndFeed readInternal(Class<? extends SyndFeed> clazz, HttpInputMessage inputMessage) {
        throw new UnsupportedOperationException(
            "Reading SyndFeed from a JSON file is currently not supported!");
    }

    @Override
    protected void writeInternal(SyndFeed syndFeed, HttpOutputMessage outputMessage)
        throws IOException {

        JSONObject json = converter.convert(syndFeed);
        outputMessage.getBody().write(json.toString().getBytes());
    }
}
