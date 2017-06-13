package com.jvm_bloggers.frontend.public_area.contributors;

import com.jvm_bloggers.core.github.ContributorsService;
import com.jvm_bloggers.entities.github.Contributor;
import com.jvm_bloggers.frontend.public_area.AbstractFrontendPage;

import io.vavr.collection.Seq;

import org.apache.wicket.markup.html.list.ListItem;
import org.apache.wicket.markup.html.list.ListView;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.wicketstuff.annotation.mount.MountPath;

@MountPath("contributors")
public class ContributorsPage extends AbstractFrontendPage {

    static final String FIRST_LEVEL_CONTRIBUTORS_LIST_ID = "firstLevelContributors";
    static final String SECOND_LEVEL_CONTRIBUTORS_LIST_ID = "secondLevelContributors";

    @SpringBean
    private ContributorsService contributorsService;

    public ContributorsPage() {
        Seq<Contributor> contributors = contributorsService.fetchContributors();
        add(new ListView<Contributor>(
            FIRST_LEVEL_CONTRIBUTORS_LIST_ID,
            contributors.slice(0, 6).toJavaList()
        ) {
            @Override
            protected void populateItem(ListItem<Contributor> item) {
                item.add(new ContributorDetails("contributorDetails", item.getModel()));
            }
        });

        add(new ListView<Contributor>(
            SECOND_LEVEL_CONTRIBUTORS_LIST_ID,
            contributors.drop(6).toJavaList()
        ) {
            @Override
            protected void populateItem(ListItem<Contributor> item) {
                item.add(new ContributorDetails("contributorDetails", item.getModel()));
            }
        });
    }

    @Override
    protected String getPageTitle() {
        return "Autorzy projektu";
    }

}
