package pl.tomaszdziurko.jvm_bloggers.view.panels;

import lombok.RequiredArgsConstructor;

import org.apache.wicket.markup.html.navigation.paging.IPageable;
import org.apache.wicket.model.AbstractReadOnlyModel;

@RequiredArgsConstructor
public class CustomPageLinkModel extends AbstractReadOnlyModel<String> {

    private final IPageable pageable;
    private final long pageNumber;
    private final String css;

    @Override
    public String getObject() {
        return isEnabled() ? css : "";
    }

    public boolean isEnabled() {
        return getPageNumber() == pageable.getCurrentPage();
    }

    private long getPageNumber() {
        long idx = pageNumber;

        if (idx < 0) {
            idx = pageable.getPageCount() + idx;
        }

        if (idx > (pageable.getPageCount() - 1)) {
            idx = pageable.getPageCount() - 1;
        }

        if (idx < 0) {
            idx = 0;
        }

        return idx;
    }
}
