package pl.tomaszdziurko.jvm_bloggers.view.panels;

import lombok.RequiredArgsConstructor;

import org.apache.wicket.markup.html.navigation.paging.IPageable;
import org.apache.wicket.model.IModel;

import java.io.Serializable;

@RequiredArgsConstructor
public class CustomPageLinkModel implements IModel<String>, Serializable {

    protected final IPageable pageable;
    protected final long pageNumber;
    private final String css;

    @Override
    public String getObject() {
        return isEnabled() ? css : "";
    }

    @Override
    public void setObject(String object) {
    }

    @Override
    public void detach() {
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
