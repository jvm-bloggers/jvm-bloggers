package com.jvm_bloggers.frontend.common_components;

import com.googlecode.wicket.jquery.ui.plugins.wysiwyg.WysiwygEditor;
import com.googlecode.wicket.jquery.ui.plugins.wysiwyg.toolbar.IWysiwygToolbar;

import org.apache.wicket.markup.html.form.HiddenField;
import org.apache.wicket.model.IModel;
import org.springframework.util.ReflectionUtils;

import java.lang.reflect.Field;

public class NonNullWysiwygEditor extends WysiwygEditor {
    public NonNullWysiwygEditor(String id, IModel<String> model, IWysiwygToolbar toolbar) {
        super(id, model, toolbar);
    }

    @Override
    protected void onInitialize() {
        super.onInitialize();
        Field field = ReflectionUtils.findField(WysiwygEditor.class, "textarea");
        field.setAccessible(true);
        HiddenField<String> textArea = (HiddenField<String>) ReflectionUtils.getField(field, this);
        textArea.setConvertEmptyInputStringToNull(false);
    }
}
