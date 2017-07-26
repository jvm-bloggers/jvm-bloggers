package com.jvm_bloggers.frontend.admin_area;

import com.jvm_bloggers.entities.top_posts_summary.TopPostsSummary;
import com.jvm_bloggers.entities.top_posts_summary.TopPostsSummaryRepository;
import io.vavr.control.Option;
import org.apache.wicket.injection.Injector;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.apache.wicket.validation.IValidatable;
import org.apache.wicket.validation.IValidator;
import org.apache.wicket.validation.ValidationError;

import java.time.YearMonth;

import static com.jvm_bloggers.utils.DateTimeUtilities.stringify;

public class TopPostsSummaryExistenceValidator implements IValidator<YearMonth> {

    @SpringBean
    private TopPostsSummaryRepository repository;

    public TopPostsSummaryExistenceValidator() {
        Injector.get().inject(this);
    }

    @Override
    public void validate(IValidatable<YearMonth> validatable) {
        YearMonth selectedPeriod = validatable.getValue();
        Option<TopPostsSummary> summary = repository
            .findOneByYearAndMonth(selectedPeriod.getYear(), selectedPeriod.getMonthValue());
        if (summary.isDefined()) {
            validatable.error(
                new ValidationError("Summary already exists for " + stringify(selectedPeriod))
            );
        }
    }

}
