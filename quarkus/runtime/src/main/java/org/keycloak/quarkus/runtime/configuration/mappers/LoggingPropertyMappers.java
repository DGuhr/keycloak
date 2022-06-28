package org.keycloak.quarkus.runtime.configuration.mappers;

import static org.keycloak.quarkus.runtime.configuration.mappers.PropertyMapper.fromOption;
import static org.keycloak.quarkus.runtime.integration.QuarkusPlatform.addInitializationException;

import java.io.File;
import java.util.Arrays;
import java.util.List;
import java.util.Locale;
import java.util.function.BiFunction;
import java.util.logging.Level;
import java.util.stream.Collectors;

import org.jboss.logmanager.LogContext;
import org.keycloak.config.LoggingOptions;
import org.keycloak.quarkus.runtime.Messages;

import io.smallrye.config.ConfigSourceInterceptorContext;

public final class LoggingPropertyMappers {

    private LoggingPropertyMappers(){}

    public static PropertyMapper[] getMappers() {
        return new PropertyMapper[] {
                fromOption(LoggingOptions.LOG)
                        .paramLabel("<handler>")
                        .build(),
                fromOption(LoggingOptions.LOG_CONSOLE_OUTPUT)
                        .to("quarkus.log.console.json")
                        .paramLabel("default|json")
                        .transformer((value, context) -> {
                            if(value.equals(LoggingOptions.DEFAULT_CONSOLE_OUTPUT.name().toLowerCase(Locale.ROOT))) {
                                return Boolean.FALSE.toString();
                            }
                            return Boolean.TRUE.toString();
                        })
                        .build(),
                fromOption(LoggingOptions.LOG_CONSOLE_FORMAT)
                        .to("quarkus.log.console.format")
                        .paramLabel("format")
                        .build(),
                fromOption(LoggingOptions.LOG_CONSOLE_COLOR)
                        .to("quarkus.log.console.color")
                        .paramLabel(Boolean.TRUE + "|" + Boolean.FALSE)
                        .build(),
                fromOption(LoggingOptions.LOG_CONSOLE_ENABLED)
                        .mapFrom("log")
                        .to("quarkus.log.console.enable")
                        .transformer(LoggingPropertyMappers.resolveLogHandler(LoggingOptions.DEFAULT_LOG_HANDLER.name()))
                        .build(),
                fromOption(LoggingOptions.LOG_FILE_ENABLED)
                        .mapFrom("log")
                        .to("quarkus.log.file.enable")
                        .transformer(LoggingPropertyMappers.resolveLogHandler("file"))
                        .build(),
                fromOption(LoggingOptions.LOG_FILE)
                        .to("quarkus.log.file.path")
                        .paramLabel("<path>/<file-name>.log")
                        .transformer(LoggingPropertyMappers::resolveFileLogLocation)
                        .build(),
                fromOption(LoggingOptions.LOG_FILE_FORMAT)
                        .to("quarkus.log.file.format")
                        .paramLabel("<format>")
                        .build(),
                fromOption(LoggingOptions.LOG_LEVEL)
                        .to("quarkus.log.level")
                        .transformer(LoggingPropertyMappers::resolveLogLevel)
                        .paramLabel("category:level")
                        .build(),
                fromOption(LoggingOptions.LOG_GELF_ENABLED)
                        .mapFrom("log")
                        .to("quarkus.log.handler.gelf.enabled")
                        .paramLabel(Boolean.TRUE + "|" + Boolean.FALSE)
                        .transformer(LoggingPropertyMappers.resolveLogHandler("gelf"))
                        .build(),
                fromOption(LoggingOptions.LOG_GELF_HOST)
                        .to("quarkus.log.handler.gelf.host")
                        .paramLabel("hostname")
                        .build(),
                fromOption(LoggingOptions.LOG_GELF_PORT)
                        .to("quarkus.log.handler.gelf.port")
                        .paramLabel("PortNr")
                        .build(),
                fromOption(LoggingOptions.LOG_GELF_VERSION)
                        .to("quarkus.log.handler.gelf.version")
                        .paramLabel("version")
                        .build(),
                fromOption(LoggingOptions.LOG_GELF_INCLUDE_STACK_TRACE)
                        .to("quarkus.log.handler.gelf.extract-stack-trace")
                        .paramLabel(Boolean.TRUE + "|" + Boolean.FALSE)
                        .build(),
                fromOption(LoggingOptions.LOG_GELF_TIMESTAMP_FORMAT)
                        .to("quarkus.log.handler.gelf.timestamp-pattern")
                        .paramLabel("pattern")
                        .build(),
                fromOption(LoggingOptions.LOG_GELF_FACILITY)
                        .to("quarkus.log.handler.gelf.facility")
                        .paramLabel("name")
                        .build(),
                fromOption(LoggingOptions.LOG_GELF_MAX_MSG_SIZE)
                        .to("quarkus.log.handler.gelf.maximum-message-size")
                        .paramLabel("size")
                        .build(),
                fromOption(LoggingOptions.LOG_GELF_INCLUDE_LOG_MSG_PARAMS)
                        .to("quarkus.log.handler.gelf.include-log-message-parameters")
                        .paramLabel(Boolean.TRUE + "|" + Boolean.FALSE)
                        .build(),
                fromOption(LoggingOptions.LOG_GELF_INCLUDE_LOCATION)
                        .to("quarkus.log.handler.gelf.include-location")
                        .paramLabel(Boolean.TRUE + "|" + Boolean.FALSE)
                        .build()
        };
    }

    private static BiFunction<String, ConfigSourceInterceptorContext, String> resolveLogHandler(String handler) {
        return (parentValue, context) -> {

            //we want to fall back to console to not have nothing shown up when wrong values are set.
            String consoleDependantErrorResult = handler.equals(LoggingOptions.DEFAULT_LOG_HANDLER.name()) ? Boolean.TRUE.toString() : Boolean.FALSE.toString();

            if(parentValue.isBlank()) {
                addInitializationException(Messages.emptyValueForKey("log"));
                return consoleDependantErrorResult;
            }

            String[] logHandlerValues = parentValue.split(",");
            List<String> availableLogHandlers = Arrays.stream(LoggingOptions.Handler.values()).map(Enum::name).collect(Collectors.toList());

            if (!availableLogHandlers.containsAll(List.of(logHandlerValues))) {
                addInitializationException(Messages.notRecognizedValueInList("log", parentValue, String.join(",", availableLogHandlers)));
                return consoleDependantErrorResult;
            }

            for (String handlerInput : logHandlerValues) {
                if (handlerInput.equals(handler)) {
                    return Boolean.TRUE.toString();
                }
            }

            return Boolean.FALSE.toString();
        };
    }

    private static String resolveFileLogLocation(String value, ConfigSourceInterceptorContext configSourceInterceptorContext) {
        if (value.endsWith(File.separator)) {
            return value + LoggingOptions.DEFAULT_LOG_FILENAME;
        }

        return value;
    }

    private static Level toLevel(String categoryLevel) throws IllegalArgumentException {
        return LogContext.getLogContext().getLevelForName(categoryLevel.toUpperCase(Locale.ROOT));
    }

    private static void setCategoryLevel(String category, String level) {
        LogContext.getLogContext().getLogger(category).setLevel(toLevel(level));
    }

    private static String resolveLogLevel(String value, ConfigSourceInterceptorContext configSourceInterceptorContext) {
        String rootLevel = LoggingOptions.DEFAULT_LOG_LEVEL.name();

        for (String level : value.split(",")) {
            String[] parts = level.split(":");
            String category = null;
            String categoryLevel;

            if (parts.length == 1) {
                categoryLevel = parts[0];
            } else if (parts.length == 2) {
                category = parts[0];
                categoryLevel = parts[1];
            } else {
                addInitializationException(Messages.invalidLogCategoryFormat(level));
                return rootLevel;
            }

            Level levelType;

            try {
                levelType = toLevel(categoryLevel);
            } catch (IllegalArgumentException iae) {
                addInitializationException(Messages.invalidLogLevel(categoryLevel));
                return rootLevel;
            }

            if (category == null) {
                rootLevel = levelType.getName();
            } else {
                setCategoryLevel(category, levelType.getName());
            }
        }

        return rootLevel;
    }
}
