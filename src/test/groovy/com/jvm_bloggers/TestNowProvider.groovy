package com.jvm_bloggers

import com.jvm_bloggers.utils.NowProvider

import java.time.LocalDate
import java.time.LocalDateTime

class TestNowProvider extends NowProvider {

    private final LocalDateTime stubbedNow;

    TestNowProvider(LocalDateTime stubbedNow) {
        this.stubbedNow = stubbedNow
    }

    @Override
    LocalDateTime now() {
        return stubbedNow
    }

    @Override
    LocalDate today() {
        return stubbedNow.toLocalDate()
    }
}
