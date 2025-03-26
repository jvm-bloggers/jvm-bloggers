package com.jvm_bloggers.frontend.admin_area.bot_clicks_detector;

import com.jvm_bloggers.entities.click.PostClicksCountByIpAddress;
import com.jvm_bloggers.entities.click.PostClicksCountByUserAgent;
import com.jvm_bloggers.frontend.admin_area.AbstractAdminPage;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.form.OnChangeAjaxBehavior;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.form.DropDownChoice;
import org.apache.wicket.markup.html.list.ListItem;
import org.apache.wicket.markup.html.list.ListView;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.wicketstuff.annotation.mount.MountPath;

import java.util.Arrays;

import static com.jvm_bloggers.frontend.admin_area.bot_clicks_detector.BotClicksDetectorPage.AggregatorType.IP_ADDRESS;
import static com.jvm_bloggers.frontend.admin_area.bot_clicks_detector.BotClicksDetectorPage.AggregatorType.USER_AGENT;
import static io.vavr.API.$;
import static io.vavr.API.Case;
import static io.vavr.API.Match;

@MountPath("bot-clicks-detector")
public class BotClicksDetectorPage extends AbstractAdminPage {

    public static final String AGGREGATOR_DROP_DOWN_ID = "aggregatorTypeDropDown";
    public static final String LIST_WRAPPER_ID = "listWrapper";
    public static final String TOP_CLICKS_LIST_VIEW_ID = "topClicksListView";

    @SpringBean
    private BotClicksDetectorPageBackingBean backingBean;

    private DropDownChoice<AggregatorType> aggregatorTypeDropDown;
    private IModel<AggregatorType> dropDownSelection = Model.of(IP_ADDRESS);
    private ListView topClicksListView;
    private WebMarkupContainer listWrapper;

    public BotClicksDetectorPage() {
        addAggregatorDropDown();
        topClicksListView = createTopClicksList();
        listWrapper = new WebMarkupContainer(LIST_WRAPPER_ID);
        listWrapper.setOutputMarkupId(true);
        listWrapper.add(topClicksListView);
        add(listWrapper);
    }

    enum AggregatorType {
        IP_ADDRESS,
        USER_AGENT
    }

    private void addAggregatorDropDown() {
        aggregatorTypeDropDown = new DropDownChoice<>(AGGREGATOR_DROP_DOWN_ID, dropDownSelection,
            Arrays.asList(AggregatorType.values()));
        aggregatorTypeDropDown.add(new OnChangeAjaxBehavior() {
            @Override
            protected void onUpdate(AjaxRequestTarget target) {
                topClicksListView = createTopClicksList();
                listWrapper.addOrReplace(topClicksListView);
                target.add(listWrapper);
            }
        });
        add(aggregatorTypeDropDown);
    }

    private ListView createTopClicksList() {
        ListView topClicksList = Match(aggregatorTypeDropDown.getModelObject()).of(
            Case($(IP_ADDRESS), () -> new ListView<PostClicksCountByIpAddress>(
                TOP_CLICKS_LIST_VIEW_ID,
                backingBean.getTop10PostsClicksFromSameIpForPreviousMonth().toJavaList()) {
                @Override
                protected void populateItem(ListItem<PostClicksCountByIpAddress> item) {
                    new PostClicksCountByIpAddressForListingPopulator().populateItem(item);
                }
            }),
            Case($(USER_AGENT), () -> new ListView<PostClicksCountByUserAgent>(
                TOP_CLICKS_LIST_VIEW_ID,
                backingBean.getTop10PostsClicksFromSameUserAgentForPreviousMonth().toJavaList()) {
                @Override
                protected void populateItem(ListItem<PostClicksCountByUserAgent> item) {
                    new PostClicksCountByUserAgentForListingPopulator().populateItem(item);
                }
            })
        );
        topClicksList.setOutputMarkupId(true);
        return topClicksList;
    }
}
