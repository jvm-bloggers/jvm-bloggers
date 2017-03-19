package com.jvm_bloggers.frontend.public_area.contributors;

import com.jvm_bloggers.entities.github.Contributor;

import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.image.ExternalImage;
import org.apache.wicket.markup.html.link.ExternalLink;
import org.apache.wicket.markup.html.panel.Panel;
import org.apache.wicket.model.IModel;

public class ContributorDetails extends Panel {

    public ContributorDetails(String id, IModel<Contributor> model) {
        super(id);
        Contributor contributor = model.getObject();

        ExternalImage avatar = new ExternalImage("avatar", contributor.getAvatarUrl());
        ExternalLink avatarLink =
            new ExternalLink("avatarLink", contributor.getProfilePage());
        avatarLink.add(avatar);
        add(avatarLink);

        add(new ExternalLink("link", contributor.getProfilePage(), contributor.getLogin()));
        add(new Label("contributions", contributor.getContributions()));
    }
}
