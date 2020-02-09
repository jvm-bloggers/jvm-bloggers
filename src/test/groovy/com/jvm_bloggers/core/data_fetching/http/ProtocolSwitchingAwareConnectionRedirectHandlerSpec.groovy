package com.jvm_bloggers.core.data_fetching.http

import spock.lang.Specification
import spock.lang.Subject

import static ProtocolSwitchingAwareConnectionRedirectHandler.DEFAULT_TIMEOUT
import static ProtocolSwitchingAwareConnectionRedirectHandler.LOCATION_HEADER

@Subject(ProtocolSwitchingAwareConnectionRedirectHandler)
class ProtocolSwitchingAwareConnectionRedirectHandlerSpec extends Specification {

    static final REQUEST_HEADERS = ['header': 'value 1']
    static final String redirectLocation = 'https://redirect.location'

    HttpURLConnection httpConnection = Mock()

    def "Should proceed if no redirect"() {
        given:
        ProtocolSwitchingAwareConnectionRedirectHandler tested = new ProtocolSwitchingAwareConnectionRedirectHandler()

        when:
        HttpURLConnection conn = tested.handle(httpConnection, null)

        then:
        conn == httpConnection
    }

    def "Should handle redirect between protocols"() {
        given:
        ProtocolSwitchingAwareConnectionRedirectHandler tested = new ProtocolSwitchingAwareConnectionRedirectHandler()

        and:
        httpConnection.getURL() >> new URL('http://redirected.url')
        httpConnection.getResponseCode() >> HttpURLConnection.HTTP_MOVED_TEMP
        httpConnection.getHeaderField(LOCATION_HEADER) >> redirectLocation

        when:
        tested.handle(httpConnection, REQUEST_HEADERS)

        then:
        interaction { commonInteractions() }
        UnknownHostException e = thrown()
        redirectLocation.contains(e.message)
    }

    def "Should throw exception when redirect limit reached"() {
        given:
        ProtocolSwitchingAwareConnectionRedirectHandler tested = new ProtocolSwitchingAwareConnectionRedirectHandler(0)

        and:
        httpConnection.getURL() >> new URL('http://redirected.url')
        httpConnection.getResponseCode() >> HttpURLConnection.HTTP_MOVED_PERM
        httpConnection.getHeaderField(LOCATION_HEADER) >> redirectLocation

        when:
        tested.handle(httpConnection, REQUEST_HEADERS)

        then:
        interaction { commonInteractions() }
        thrown(ProtocolSwitchingAwareConnectionRedirectHandler.TooManyRedirectsException)
    }

    def "Should throw NPE on null connection parameter"() {
        given:
        ProtocolSwitchingAwareConnectionRedirectHandler tested = new ProtocolSwitchingAwareConnectionRedirectHandler(0)

        when:
        tested.handle(null, null)

        then:
        NullPointerException e = thrown()
        e.message.contains('urlConnection')
    }

    private def commonInteractions() {
        with(httpConnection) {
            1 * setRequestProperty('header', 'value 1')
            1 * setInstanceFollowRedirects(true)
            1 * setReadTimeout(DEFAULT_TIMEOUT)
            1 * setConnectTimeout(DEFAULT_TIMEOUT)
        }
    }
}
