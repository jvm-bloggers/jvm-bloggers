package com.jvm_bloggers

import com.jvm_bloggers.frontend.admin_area.session.UserSession
import org.apache.wicket.authroles.authorization.strategies.role.Roles
import com.jvm_bloggers.frontend.wicket.RenderJavaScriptToFooterHeaderResponseDecorator
import org.apache.wicket.bean.validation.BeanValidationConfiguration
import org.apache.wicket.mock.MockApplication
import org.apache.wicket.protocol.http.WebApplication
import org.apache.wicket.spring.injection.annot.SpringComponentInjector
import org.apache.wicket.spring.test.ApplicationContextMock
import org.apache.wicket.util.tester.WicketTester
import spock.lang.Specification

import static org.apache.wicket.authroles.authorization.strategies.role.Roles.ADMIN

abstract class MockSpringContextAwareSpecification extends Specification {

    private final ApplicationContextMock mockApplicationContext = new ApplicationContextMock()

    protected final WicketTester tester = new WicketTester([
        "newSession": {
            request, response -> new UserSession(request)
        }
    ] as MockApplication)

    def setup() {
        WebApplication webApp = tester.getApplication()
        webApp.setHeaderResponseDecorator(
                new RenderJavaScriptToFooterHeaderResponseDecorator("footer-container"))
        webApp.getComponentInstantiationListeners()
                .add(new SpringComponentInjector(webApp, mockApplicationContext))
        new BeanValidationConfiguration().configure(webApp);
        setupContext()
    }

    protected abstract void setupContext()

    protected void addBean(Object bean) {
        mockApplicationContext.putBean(bean)
    }

    protected void currentUserIsAdmin() {
        UserSession.get().loginAs("Admin", new Roles(ADMIN))
    }

}
