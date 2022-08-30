package com.jvm_bloggers.core.utils;

import com.vdurmont.emoji.EmojiParser;
import io.vavr.control.Option;
import org.apache.commons.lang3.StringUtils;

import static org.apache.commons.lang3.StringUtils.EMPTY;

public class EmojiRemover {

    public String remove(final String input) {
        return Option.of(input)
                .filter(StringUtils::isNotBlank)
                .map(EmojiParser::removeAllEmojis)
                .getOrElse(EMPTY);
    }
}
