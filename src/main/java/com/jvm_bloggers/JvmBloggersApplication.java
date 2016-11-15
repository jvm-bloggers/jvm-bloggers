package com.jvm_bloggers;

import com.giffing.wicket.spring.boot.starter.app.WicketBootSecuredWebApplication;
import com.jvm_bloggers.admin_panel.login.LoginPage;
import com.jvm_bloggers.admin_panel.session.UserSession;
import com.jvm_bloggers.frontend.HomePage;
import com.ulisesbocchio.jasyptspringboot.annotation.EnableEncryptableProperties;

import org.apache.wicket.Page;
import org.apache.wicket.authroles.authentication.AbstractAuthenticatedWebSession;
import org.apache.wicket.devutils.stateless.StatelessChecker;
import org.apache.wicket.markup.html.WebPage;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.boot.builder.SpringApplicationBuilder;
import org.springframework.cache.annotation.EnableCaching;
import org.springframework.scheduling.annotation.EnableAsync;
import org.springframework.scheduling.annotation.EnableScheduling;
import org.wicketstuff.annotation.scan.AnnotatedMountScanner;

@SpringBootApplication
@EnableScheduling
@EnableAsync
@EnableCaching
@EnableEncryptableProperties
public class JvmBloggersApplication extends WicketBootSecuredWebApplication {

    public static void main(String[] args) {
        new SpringApplicationBuilder()
            .sources(JvmBloggersApplication.class)
            .run(args);
    }

    @Override
    protected void init() {
        super.init();
        getComponentPostOnBeforeRenderListeners().add(new StatelessChecker());
        new AnnotatedMountScanner().scanPackage("com.jvm_bloggers").mount(this);
        getMarkupSettings().setStripWicketTags(true);
    }

    @Override
    protected Class<? extends WebPage> getSignInPageClass() {
        return LoginPage.class;
    }

    @Override
    public Class<? extends Page> getHomePage() {
        return HomePage.class;
    }

    @Override
    protected Class<? extends AbstractAuthenticatedWebSession> getWebSessionClass() {
        return UserSession.class;
    }
}
