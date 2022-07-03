package com.jvm_bloggers.validation

import org.gradle.api.DefaultTask
import org.gradle.api.tasks.TaskAction

import javax.inject.Inject
import java.nio.file.Files
import java.nio.file.Path

abstract class JsonValidationTask extends DefaultTask {

    private final JsonValidator validator

    @Inject
    JsonValidationTask(final Path schemaFile, final Path jsonFile) {
        this.validator = new JsonValidator(
                () -> Files.newInputStream(schemaFile),
                () -> Files.newInputStream(jsonFile))
    }

    @TaskAction
    def validate() {
        validator.validate()
    }
}
