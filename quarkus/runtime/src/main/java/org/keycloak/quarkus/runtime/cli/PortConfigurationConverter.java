package org.keycloak.quarkus.runtime.cli;

import org.keycloak.quarkus.runtime.configuration.model.PortConfiguration;
import picocli.CommandLine;
import picocli.CommandLine.ITypeConverter;

public class PortConfigurationConverter implements ITypeConverter<PortConfiguration> {
    @Override
    public PortConfiguration convert(String input) throws CommandLine.TypeConversionException {

        if (input.isEmpty()) {
            throw new CommandLine.TypeConversionException("Port cannot be empty.");
        }

        int intValue;

        try {
            intValue = Integer.parseInt(input);
        }
        catch (NumberFormatException e) {
            throw new CommandLine.TypeConversionException(
                    "Invalid Input: Must be a port number between 0 and 65535");
        }

        if( intValue < 0 || intValue > 65535 ) {
            throw new CommandLine.TypeConversionException(
                    "Invalid Port: Must be a port number between 0 and 65535. Number: " + intValue);
        }

        return new PortConfiguration(intValue);
    }
}
