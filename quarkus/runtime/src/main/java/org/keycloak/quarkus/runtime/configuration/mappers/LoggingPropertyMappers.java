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
                        .description("Set the root logging Level of the application.")
                        .expectedValues(Arrays.asList("OFF","FATAL","ERROR","WARN","INFO","DEBUG","TRACE","ALL"))
                        .defaultValue("INFO")
                        .paramLabel("level")
                        .build(),
                builder()
                        .from("log.")
                        .to("quarkus.log.")
                        .description("Placeholder for extension via properties file...")
                        .build()
        };
    }

    private static PropertyMapper.Builder builder() {
        return PropertyMapper.builder(ConfigCategory.LOGGING);
    }

}
