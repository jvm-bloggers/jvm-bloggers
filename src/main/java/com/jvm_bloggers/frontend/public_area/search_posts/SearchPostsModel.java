package com.jvm_bloggers.frontend.public_area.search_posts;

import java.io.Serializable;
import lombok.Data;

@Data
public class SearchPostsModel implements Serializable {

  private String searchPhrase;
}
