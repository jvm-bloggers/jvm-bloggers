package com.jvm_bloggers.frontend.public_area.contributors;

import com.jvm_bloggers.core.github.ContributorsService;
import com.jvm_bloggers.entities.github.Contributor;
import com.jvm_bloggers.frontend.public_area.AbstractFrontendPage;

import org.apache.wicket.markup.html.list.ListItem;
import org.apache.wicket.markup.html.list.ListView;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.wicketstuff.annotation.mount.MountPath;

import java.util.List;

@MountPath("contributors")
public class ContributorsPage extends AbstractFrontendPage {

    private static final String CONTRIBUTORS_LIST_ID = "contributorsList";

    @SpringBean
    private ContributorsService contributorsService;

    public ContributorsPage() {
        List<Contributor> contributors = contributorsService.fetchContributors();
        ListView<Contributor> listView =
            new ListView<Contributor>(CONTRIBUTORS_LIST_ID, contributors) {

                @Override
                protected void populateItem(ListItem<Contributor> item) {
                    item.add(new ContributorDetails("contributorDetails", item.getModel()));
                }
            };
        add(listView);
    }
}
