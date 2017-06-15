package com.jvm_bloggers.domain.command;

public interface CommandHandler<T> {

    void handle(T command);

}
