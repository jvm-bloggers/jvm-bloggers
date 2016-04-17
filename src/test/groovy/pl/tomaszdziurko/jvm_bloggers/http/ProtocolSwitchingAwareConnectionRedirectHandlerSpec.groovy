package pl.tomaszdziurko.jvm_bloggers.http

import pl.tomaszdziurko.jvm_bloggers.http.ProtocolSwitchingAwareConnectionRedirectHandler.TooManyRedirectsException
import spock.lang.Specification
import spock.lang.Subject

import static pl.tomaszdziurko.jvm_bloggers.http.ProtocolSwitchingAwareConnectionRedirectHandler.DEFAULT_TIMEOUT
import static pl.tomaszdziurko.jvm_bloggers.http.ProtocolSwitchingAwareConnectionRedirectHandler.LOCATION_HEADER

class ProtocolSwitchingAwareConnectionRedirectHandlerSpec extends Specification {

    static final REQUEST_HEADERS = ["header": ["value 1", "value 2"]]

    final HttpURLConnection httpConnection = Mock();

    def "Should proceed if no redirect"() {
        given:
            @Subject tested = new ProtocolSwitchingAwareConnectionRedirectHandler();
        when:
            def conn = tested.handle(httpConnection, null)
        then:
            conn == httpConnection
    }

    def "Should handle redirect between protocols"() {
        given:
            @Subject tested = new ProtocolSwitchingAwareConnectionRedirectHandler()
        and:
            def redirectLocation = "https://redirect.location"
            httpConnection.getURL() >> new URL("http://redirected.url")
            httpConnection.getResponseCode() >> HttpURLConnection.HTTP_MOVED_TEMP
            httpConnection.getHeaderField(LOCATION_HEADER) >> redirectLocation
        when:
            HttpURLConnection redirected = tested.handle(httpConnection, REQUEST_HEADERS)
        then:
            interaction{
                commonInteractions(httpConnection)
                commonInteractions(redirected)
            }
            UnknownHostException e = thrown()
            redirectLocation.contains(e.message)
    }
    
    def "Should throw exception when redirect limit reached"() {
        given:
            @Subject tested = new ProtocolSwitchingAwareConnectionRedirectHandler(0);
        and:
            def redirectLocation = "https://redirect.location"
            httpConnection.getURL() >> new URL("http://redirected.url")
            httpConnection.getResponseCode() >> HttpURLConnection.HTTP_MOVED_PERM
            httpConnection.getHeaderField(LOCATION_HEADER) >> redirectLocation
        when:
            tested.handle(httpConnection, REQUEST_HEADERS)
        then:
            interaction{ commonInteractions(httpConnection) }
            thrown TooManyRedirectsException
    }

    def "Should throw NPE on null connection parameter"() {
        given:
            @Subject tested = new ProtocolSwitchingAwareConnectionRedirectHandler(0);
        when:
            tested.handle(null, null)
        then:
            NullPointerException e = thrown()
            e.message.contains("urlConnection")
    }
    
    private def commonInteractions(HttpURLConnection connection) {
        with(connection) {
            1 * setRequestProperty("header", "value 1")
            1 * setRequestProperty("header", "value 2")
            1 * setInstanceFollowRedirects(true)
            1 * setReadTimeout(DEFAULT_TIMEOUT)
            1 * setConnectTimeout(DEFAULT_TIMEOUT)
        }

    }
}
