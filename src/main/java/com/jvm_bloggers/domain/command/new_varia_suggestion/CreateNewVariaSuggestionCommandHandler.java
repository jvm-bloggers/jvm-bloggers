package com.jvm_bloggers.domain.command.new_varia_suggestion;

import com.jvm_bloggers.domain.command.CommandHandler;
import com.jvm_bloggers.entities.varia_suggestion.VariaSuggestion;
import com.jvm_bloggers.entities.varia_suggestion.VariaSuggestionRepository;

import lombok.RequiredArgsConstructor;

import org.springframework.context.event.EventListener;
import org.springframework.stereotype.Service;

@Service
@RequiredArgsConstructor
public class CreateNewVariaSuggestionCommandHandler implements
    CommandHandler<CreateNewVariaSuggestion> {

    private final VariaSuggestionRepository variaSuggestionRepository;

    @Override
    @EventListener
    public void handle(CreateNewVariaSuggestion command) {
        variaSuggestionRepository.save(
            new VariaSuggestion(
                command.getUrl(),
                command.getReason(),
                command.getAuthor()
            )
        );
    }
}
