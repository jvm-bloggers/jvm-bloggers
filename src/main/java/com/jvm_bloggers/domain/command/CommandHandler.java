package com.jvm_bloggers.domain.command;

public interface CommandHandler<T extends Command> {

    void handle(T command);

}
