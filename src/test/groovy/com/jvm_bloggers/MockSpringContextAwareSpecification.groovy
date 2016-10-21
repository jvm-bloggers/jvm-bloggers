package com.jvm_bloggers

import com.jvm_bloggers.admin_panel.session.UserSession
import org.apache.wicket.bean.validation.BeanValidationConfiguration
import org.apache.wicket.mock.MockApplication
import org.apache.wicket.protocol.http.WebApplication
import org.apache.wicket.spring.injection.annot.SpringComponentInjector
import org.apache.wicket.spring.test.ApplicationContextMock
import org.apache.wicket.util.tester.WicketTester


import spock.lang.Specification

public abstract class MockSpringContextAwareSpecification extends Specification {

    private final ApplicationContextMock mockApplicationContext = new ApplicationContextMock()

    protected final WicketTester tester = new WicketTester([
        "newSession": {
            request, response -> new UserSession(request)
        }
    ] as MockApplication)

    def setup() {
        WebApplication webApp = tester.getApplication()
        webApp.getComponentInstantiationListeners()
                .add(new SpringComponentInjector(webApp, mockApplicationContext))
        new BeanValidationConfiguration().configure(webApp);
        setupContext()
    }

    protected abstract void setupContext()

    protected void addBean(Object bean) {
        mockApplicationContext.putBean(bean)
    }

}
