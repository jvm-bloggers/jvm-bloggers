package com.jvm_bloggers

import com.jvm_bloggers.frontend.admin_area.session.UserSession

import de.agilecoders.wicket.webjars.WicketWebjars
import de.agilecoders.wicket.webjars.settings.WebjarsSettings
import org.apache.wicket.authroles.authorization.strategies.role.Roles
import org.apache.wicket.bean.validation.BeanValidationConfiguration
import org.apache.wicket.markup.head.IHeaderResponse
import org.apache.wicket.markup.head.ResourceAggregator
import org.apache.wicket.markup.head.filter.JavaScriptFilteredIntoFooterHeaderResponse
import org.apache.wicket.markup.html.IHeaderResponseDecorator
import org.apache.wicket.mock.MockApplication
import org.apache.wicket.protocol.http.WebApplication
import org.apache.wicket.spring.injection.annot.SpringComponentInjector
import org.apache.wicket.spring.test.ApplicationContextMock
import org.apache.wicket.util.tester.WicketTester
import spock.lang.Specification

import static org.apache.wicket.authroles.authorization.strategies.role.Roles.ADMIN

abstract class MockSpringContextAwareSpecification extends Specification {

    private ApplicationContextMock mockApplicationContext = new ApplicationContextMock()

    protected WicketTester tester = new WicketTester([
        "newSession": {
            request, response -> new UserSession(request)
        }
    ] as MockApplication)

    def setup() {
        WebApplication webApp = tester.getApplication()
        webApp.setHeaderResponseDecorator(new IHeaderResponseDecorator() {
            @Override
            IHeaderResponse decorate(IHeaderResponse response) {
                return new ResourceAggregator(
                    new JavaScriptFilteredIntoFooterHeaderResponse(response, "footer-container")
                );
            }
        })
        webApp.getComponentInstantiationListeners()
                .add(new SpringComponentInjector(webApp, mockApplicationContext))
        WicketWebjars.install(webApp, new WebjarsSettings())
        new BeanValidationConfiguration().configure(webApp)
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
