package com.jvm_bloggers.core.social.twitter;

import io.vavr.collection.List;
import lombok.experimental.UtilityClass;

@UtilityClass
class TweetContentTemplates {

    private final String NEW_ISSUE_MESSAGE_TEMPLATE =
            "Nowy numer #<number> już online - <link> z postami między innymi <personal1>"
                    + "<if(company && personal2)>, <company> i <personal2>"
                    + "<elseif(company)> i <company>"
                    + "<elseif(personal2)> i <personal2><endif> #java #jvm";

    private final String NEW_BLOG_MESSAGE_TEMPLATE_1 =
            "Do linkowanych na @JVM_bloggers blogów zawitał blog "
                    + "<author>. Zapraszamy na <link> "
                    + "do zapoznania się z nowymi wpisami. #java #jvm";

    private final String NEW_BLOG_MESSAGE_TEMPLATE_2 =
            "<author> dodał bloga do listy, witamy na pokładzie i zapraszamy na <link>"
                    + " do najnowszego wydania newslettera. #java #jvm";

    public final String NEW_BLOG_MESSAGE_TEMPLATE_3 =
            "Witamy nowy blog autorstwa <author> i zapraszamy do najnowszego wydania "
                    + "newslettera na <link>. #java #jvm";

    String newIssueMessageTemplate() {
        return NEW_ISSUE_MESSAGE_TEMPLATE;
    }

    List<String> newBlogMessageTemplates() {
        return List.of(NEW_BLOG_MESSAGE_TEMPLATE_1, NEW_BLOG_MESSAGE_TEMPLATE_2, NEW_BLOG_MESSAGE_TEMPLATE_3);
    }
}
