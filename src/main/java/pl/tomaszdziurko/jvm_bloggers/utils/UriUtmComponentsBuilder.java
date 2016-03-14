package pl.tomaszdziurko.jvm_bloggers.utils;

import static pl.tomaszdziurko.jvm_bloggers.UTMParameters.UTM_CAMPAIGN_KEY;
import static pl.tomaszdziurko.jvm_bloggers.UTMParameters.UTM_MEDIUM_KEY;
import static pl.tomaszdziurko.jvm_bloggers.UTMParameters.UTM_SOURCE_KEY;

import org.springframework.web.util.UriComponentsBuilder;

public class UriUtmComponentsBuilder {

    public static final String DEFAULT_UTM_SOURCE = "jvm-bloggers.com";
    public static final String DEFAULT_UTM_CAMPAING = "jvm-bloggers";

    private final UriComponentsBuilder urlBuilder;

    private UriUtmComponentsBuilder(String url) {
        urlBuilder = UriComponentsBuilder.fromHttpUrl(url);
    }

    public static UriUtmComponentsBuilder fromHttpUrl(String url) {
        return new UriUtmComponentsBuilder(url);
    }

    public UriUtmComponentsBuilder withSource(String source) {
        urlBuilder.queryParam(UTM_SOURCE_KEY, source);
        return this;
    }

    public UriUtmComponentsBuilder withCampaign(String campaign) {
        urlBuilder.queryParam(UTM_CAMPAIGN_KEY, campaign);
        return this;
    }

    public UriUtmComponentsBuilder withMedium(String medium) {
        urlBuilder.queryParam(UTM_MEDIUM_KEY, medium);
        return this;
    }

    public String build() {
        return urlBuilder.build().toString();
    }

}
