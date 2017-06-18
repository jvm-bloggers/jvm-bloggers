package com.jvm_bloggers.frontend.admin_area;

import com.jvm_bloggers.frontend.admin_area.moderation.ModerationPage;
import com.jvm_bloggers.frontend.admin_area.panels.CustomFeedbackPanel;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.form.ChoiceRenderer;
import org.apache.wicket.markup.html.form.DropDownChoice;
import org.apache.wicket.markup.html.form.StatelessForm;
import org.apache.wicket.markup.html.link.BookmarkablePageLink;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.model.LoadableDetachableModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.wicketstuff.annotation.mount.MountPath;

import java.time.YearMonth;
import java.util.List;

import static com.jvm_bloggers.utils.DateTimeUtilities.stringify;

@MountPath("admin")
public class AdminDashboardPage extends AbstractAdminPage {

    public static final String GENERATE_TOP_POSTS_SUMMARY_FORM_ID = "generateTopPostsSummaryForm";
    public static final String YEAR_MONTH_SELECTOR_ID = "selectedPeriod";

    @SpringBean
    private AdminDashboardPageBackingBean backingBean;

    private YearMonth selectedPeriod;

    public AdminDashboardPage() {
        add(new CustomFeedbackPanel("feedback"));
        add(new Label("postsSinceLastPublication", new LoadableDetachableModel<Integer>() {
            @Override
            protected Integer load() {
                return backingBean.getNumberOfPostsSinceLastPublication();
            }
        }));
        add(new Label("postsWaitingForModeration", new LoadableDetachableModel<Integer>() {
            @Override
            protected Integer load() {
                return backingBean.getNumberOfPostsWaitingForModeration();
            }
        }));
        add(
            new BookmarkablePageLink<ModerationPage>("moderationDetailsLink", ModerationPage.class)
        );
        add(createTopPostsForm());
    }

    private StatelessForm createTopPostsForm() {
        StatelessForm<AdminDashboardPage> generateTopPostsSummaryForm =
            new StatelessForm<AdminDashboardPage>(
                GENERATE_TOP_POSTS_SUMMARY_FORM_ID,
                new CompoundPropertyModel<>(this)) {
            @Override
            protected void onSubmit() {
                backingBean.generateTopPostsSummary(selectedPeriod);
                getSession().info("Top posts summary generated for " + stringify(selectedPeriod));
                setResponsePage(AdminDashboardPage.class);
            }
        };
        generateTopPostsSummaryForm.add(createPeriodSelector());
        return generateTopPostsSummaryForm;
    }

    private DropDownChoice<YearMonth> createPeriodSelector() {
        List<YearMonth> availablePeriodChoices = prepareYearMonthChoices();
        DropDownChoice<YearMonth> yearMonthSelector = new DropDownChoice<>(
            YEAR_MONTH_SELECTOR_ID,
            availablePeriodChoices,
            createChoiceRenderer()
        );
        yearMonthSelector.setLabel(Model.of("Summary Period"));
        yearMonthSelector.setRequired(true);
        yearMonthSelector.add(new TopPostsSummaryExistenceValidator());
        return yearMonthSelector;
    }

    private ChoiceRenderer<YearMonth> createChoiceRenderer() {
        return new ChoiceRenderer<YearMonth>() {
            @Override
            public Object getDisplayValue(YearMonth object) {
                return stringify(object);
            }
        };
    }

    private List<YearMonth> prepareYearMonthChoices() {
        return backingBean.prepareYearMonthChoices();
    }

}
