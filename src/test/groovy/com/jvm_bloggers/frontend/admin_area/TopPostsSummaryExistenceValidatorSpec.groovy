package com.jvm_bloggers.frontend.admin_area

import com.jvm_bloggers.MockSpringContextAwareSpecification
import com.jvm_bloggers.entities.top_posts_summary.TopPostsSummary
import com.jvm_bloggers.entities.top_posts_summary.TopPostsSummaryRepository
import io.vavr.control.Option
import org.apache.wicket.validation.IValidatable
import spock.lang.Shared
import spock.lang.Subject

import java.time.YearMonth

class TopPostsSummaryExistenceValidatorSpec extends MockSpringContextAwareSpecification {

    @Shared
    YearMonth PERIOD = YearMonth.now()

    TopPostsSummaryRepository repository = Stub(TopPostsSummaryRepository)

    @Override
    protected void setupContext() {
        addBean(repository)
    }

    def "should raise an error when summary exists for a given period"() {
        given:
        @Subject TopPostsSummaryExistenceValidator validator = new TopPostsSummaryExistenceValidator()
        IValidatable<YearMonth> validatedValue = Mock(IValidatable) {
            getValue() >> PERIOD
        }

        and:
        repository.findOneByYearAndMonth(PERIOD.getYear(), PERIOD.getMonthValue()) >> Option.of(Stub(TopPostsSummary))

        when:
        validator.validate(validatedValue)

        then:
        1 * validatedValue.error(_)
    }

}
