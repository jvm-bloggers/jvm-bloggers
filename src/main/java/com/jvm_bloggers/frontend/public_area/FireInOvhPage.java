package com.jvm_bloggers.frontend.public_area;

import org.wicketstuff.annotation.mount.MountPath;

@MountPath("fire-in-ovh")
public class FireInOvhPage extends AbstractFrontendPage {

    public FireInOvhPage() {
    }

    @Override
    protected String getPageTitle() {
        return "Pożar w OVH i utrata danych";
    }

}
