package com.jvm_bloggers.core.utils;

import lombok.experimental.UtilityClass;

@UtilityClass
public class LinkUtils {

  private static final String TWITTER_HOME_URL = "https://twitter.com/";

  public static String getFullTwitterAccountUrl(String twitterHandle) {
    return TWITTER_HOME_URL + twitterHandle;
  }

}
