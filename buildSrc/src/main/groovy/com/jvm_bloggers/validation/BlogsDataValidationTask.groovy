package com.jvm_bloggers.validation

import com.fasterxml.jackson.databind.ObjectMapper
import com.networknt.schema.JsonSchemaFactory
import com.networknt.schema.SpecVersion
import org.gradle.api.DefaultTask
import org.gradle.api.tasks.TaskAction

import javax.inject.Inject
import java.nio.file.Path

abstract class BlogsDataValidationTask extends DefaultTask {

    private static final ObjectMapper mapper = new ObjectMapper()
    private static final JsonSchemaFactory factory = JsonSchemaFactory.getInstance(SpecVersion.VersionFlag.V4)

    private final Path schemaFile
    private final Path jsonFile

    @Inject
    BlogsDataValidationTask(final Path schemaFile, final Path jsonFile) {
        this.schemaFile = schemaFile
        this.jsonFile = jsonFile
    }

    @TaskAction
    def validate() {
        try {
            def schema = factory.getSchema(schemaFile.toUri())
            def json = mapper.readTree(jsonFile.toFile())

            def errors = schema.validate(json)

            if (!errors.isEmpty()) {
                throw new ValidationException("Unable to validate '${jsonFile.toString()}' due to '${errors}'!")
            }
        } catch (Exception e) {
            throw new ValidationException(e)
        }
    }

    static class ValidationException extends RuntimeException {

        ValidationException(final String message) {
            super(message)
        }

        ValidationException(final Throwable cause) {
            super(cause)
        }
    }
}
