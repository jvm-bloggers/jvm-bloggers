package com.jvm_bloggers

import com.jvm_bloggers.utils.NowProvider

import java.time.LocalDate
import java.time.LocalDateTime

class TestTimeProvider implements NowProvider {

    private final LocalDateTime stubbedNow;

    TestTimeProvider(LocalDateTime stubbedNow) {
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
