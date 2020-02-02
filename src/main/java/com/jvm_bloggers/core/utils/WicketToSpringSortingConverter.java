package com.jvm_bloggers.core.utils;

import io.vavr.control.Option;

import org.apache.wicket.extensions.markup.html.repeater.util.SortParam;
import org.springframework.data.domain.Sort;

public class WicketToSpringSortingConverter {
    private WicketToSpringSortingConverter() {
        //static utility
    }

    public static Option<Sort> convert(SortParam<String> wicketSortState) {
        if (wicketSortState == null) {
            return Option.none();
        }

        Sort.Direction direction = toSpringDirection(wicketSortState.isAscending());

        return Option.of(Sort.by(direction, wicketSortState.getProperty()));
    }

    private static Sort.Direction toSpringDirection(boolean ascending) {
        if (ascending) {
            return Sort.Direction.ASC;
        } else {
            return Sort.Direction.DESC;
        }
    }
}
