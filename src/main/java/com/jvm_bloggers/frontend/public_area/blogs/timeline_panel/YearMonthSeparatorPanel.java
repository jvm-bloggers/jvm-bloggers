package com.jvm_bloggers.frontend.public_area.blogs.timeline_panel;

import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.panel.Panel;
import org.apache.wicket.model.Model;

import java.time.YearMonth;
import java.util.Locale;

import static java.time.format.TextStyle.FULL_STANDALONE;

public class YearMonthSeparatorPanel extends Panel {

    private static final Locale POLISH_LOCALE = new Locale("PL");

    static String MONTH_YEAR_LABEL_ID = "monthYear";

    public YearMonthSeparatorPanel(String id, YearMonth yearMonth) {
        super(id);

        String monthName = yearMonth.getMonth().getDisplayName(FULL_STANDALONE, POLISH_LOCALE);
        add(new Label(MONTH_YEAR_LABEL_ID,
            Model.of(String.format("%s %d", monthName, yearMonth.getYear()))));
    }
}
