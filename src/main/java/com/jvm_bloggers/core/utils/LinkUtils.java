package com.jvm_bloggers.core.utils;

import lombok.experimental.UtilityClass;

@UtilityClass
public class LinkUtils {

  private static final String TWITTER_HOME_URL = "https://twitter.com/";

  public static String getFullTwitterAccountUrl(String twitterHandle) {
    if (twitterHandle == null) {
      return TWITTER_HOME_URL;
    }
    if (twitterHandle.startsWith("@")) {
      return TWITTER_HOME_URL + twitterHandle.substring(1);
    }
    return TWITTER_HOME_URL;
  }
}
