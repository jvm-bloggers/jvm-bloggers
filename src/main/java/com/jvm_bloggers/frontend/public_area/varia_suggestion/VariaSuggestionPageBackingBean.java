package com.jvm_bloggers.frontend.public_area.varia_suggestion;

import com.jvm_bloggers.domain.command.CommandPublisher;
import com.jvm_bloggers.domain.command.new_varia_suggestion.CreateNewVariaSuggestion;
import lombok.NoArgsConstructor;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service
@NoArgsConstructor
public class VariaSuggestionPageBackingBean {

    private CommandPublisher commandPublisher;

    @Autowired
    public VariaSuggestionPageBackingBean(CommandPublisher commandPublisher) {
        this.commandPublisher = commandPublisher;
    }

    public void createVariaSuggestion(VariaSuggestionModel variaSuggestionModel) {
        commandPublisher.publish(new CreateNewVariaSuggestion(
            variaSuggestionModel.getUrl(),
            variaSuggestionModel.getReason(),
            variaSuggestionModel.getAuthor()
        ));
    }
}
