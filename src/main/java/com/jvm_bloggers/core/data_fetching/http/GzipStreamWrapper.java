package com.jvm_bloggers.core.data_fetching.http;

import org.springframework.stereotype.Component;

import java.io.BufferedInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.zip.GZIPInputStream;
import java.util.zip.ZipException;

@Component
public class GzipStreamWrapper {

    public InputStream wrap(InputStream inputStream) throws IOException {
        if (!inputStream.markSupported()) {
            inputStream = new BufferedInputStream(inputStream);
        }
        inputStream.mark(1000);
        try {
            return new GZIPInputStream(inputStream);
        } catch (ZipException ex) {
            inputStream.reset();
            return inputStream;
        }
    }
}
