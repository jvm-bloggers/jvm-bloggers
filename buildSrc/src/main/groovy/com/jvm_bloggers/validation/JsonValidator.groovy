package com.jvm_bloggers.validation

import com.fasterxml.jackson.core.JacksonException
import com.fasterxml.jackson.databind.JsonNode
import com.fasterxml.jackson.databind.ObjectMapper
import com.networknt.schema.JsonSchema
import com.networknt.schema.JsonSchemaException
import com.networknt.schema.JsonSchemaFactory
import com.networknt.schema.SpecVersion

import java.util.function.Supplier

class JsonValidator {
    private static final ObjectMapper mapper = new ObjectMapper()
    private static final JsonSchemaFactory factory = JsonSchemaFactory.getInstance(SpecVersion.VersionFlag.V4)

    private final Supplier<InputStream> schemaSupplier
    private final Supplier<InputStream> jsonSupplier

    JsonValidator(final Supplier<InputStream> schemaSupplier, final Supplier<InputStream> jsonSupplier) {
        this.schemaSupplier = schemaSupplier
        this.jsonSupplier = jsonSupplier
    }

    def validate() {
        try (def schemaStream = schemaSupplier.get()
             def jsonStream = jsonSupplier.get()) {

            def schema = factory.getSchema(schemaStream)
            def json = mapper.readTree(jsonStream)

            validateData(schema, json)
        } catch (JacksonException | JsonSchemaException e) {
            throw new JsonValidationException("Unable to setup validation process!", e)
        }
    }

    private static void validateData(final JsonSchema schema, final JsonNode json) {
        def errors = schema.validate(json)

        if (!errors.isEmpty()) {
            throw new JsonValidationException("Unable to validate '${json.toString()}' due to '${errors}'!")
        }
    }

    static class JsonValidationException extends RuntimeException {

        JsonValidationException(final String message) {
            super(message)
        }

        JsonValidationException(final String message, final Throwable cause) {
            super(message, cause)
        }
    }
}
