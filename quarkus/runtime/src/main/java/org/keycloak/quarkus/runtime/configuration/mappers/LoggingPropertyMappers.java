package org.keycloak.quarkus.runtime.configuration.mappers;

import org.jboss.logmanager.LogManager;

import java.util.Arrays;

final class LoggingPropertyMappers {
    private LoggingPropertyMappers(){}

    public static PropertyMapper[] getLoggingPropertyMappers() {
        return new PropertyMapper[] {
                builder()
                        .from("log.level")
                        .to("quarkus.log.level")
                        .description("Set the root logging Level.")
                        .expectedValues(Arrays.asList("OFF","FATAL","ERROR","WARN","INFO","DEBUG","TRACE","ALL"))
                        .defaultValue("INFO")
                        .paramLabel("level")
                        .build(),
                builder()
                        .from("log.")
                        .to("quarkus.log.")
                        .description("Placeholder for extension via properties file...")
                        .build(),
                builder()
                        .from("log.json")
                        .to("quarkus.log.console.json")
                        .description("Enable / Disable JSON Logging to console.")
                        .paramLabel(Boolean.TRUE + "|" + Boolean.FALSE)
                        .expectedValues(Arrays.asList(Boolean.TRUE.toString(), Boolean.FALSE.toString()))
                        .defaultValue(Boolean.TRUE.toString())
                        .build(),
                builder()
                        .from("log.db.level")
                        .to("quarkus.log.category.\"org.hibernate\".level")
                        .description("Set the logging Level for db related logs.")
                        .expectedValues(Arrays.asList("OFF","FATAL","ERROR","WARN","INFO","DEBUG","TRACE","ALL"))
                        .defaultValue("INFO")
                        .paramLabel("level")
                        .build(),
        };
    }

    private static PropertyMapper.Builder builder() {
        return PropertyMapper.builder(ConfigCategory.LOGGING);
    }

}
