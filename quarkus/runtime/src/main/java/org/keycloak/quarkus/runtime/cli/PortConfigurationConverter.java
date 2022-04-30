package org.keycloak.quarkus.runtime.cli;

import org.keycloak.quarkus.runtime.configuration.model.PortConfiguration;
import picocli.CommandLine;
import picocli.CommandLine.ITypeConverter;
import picocli.CommandLine.TypeConversionException;

public class PortConfigurationConverter implements ITypeConverter<PortConfiguration> {

    @Override
    public PortConfiguration convert(String input) throws CommandLine.TypeConversionException {
        try {
            return new PortConfiguration(input);
        } catch (IllegalArgumentException ex) {
           throw new TypeConversionException(ex.getMessage());
        }
    }
}
