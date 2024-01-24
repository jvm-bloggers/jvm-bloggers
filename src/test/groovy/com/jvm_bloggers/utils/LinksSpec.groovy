package com.jvm_bloggers.utils

import com.jvm_bloggers.core.utils.LinkUtils
import spock.lang.Specification


class LinksSpec extends Specification {

  def "should generate a full twitter url based on a twitter handle"() {
    expect:
    LinkUtils.getFullTwitterAccountUrl(twitterHandle) == fullUrl
    where:
    twitterHandle  | fullUrl
    "@tnurkiewicz" | "https://twitter.com/tnurkiewicz"
    "@jkubrynski"  | "https://twitter.com/jkubrynski"
  }

}