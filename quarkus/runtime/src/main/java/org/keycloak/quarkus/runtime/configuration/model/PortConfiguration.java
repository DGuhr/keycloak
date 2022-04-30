package org.keycloak.quarkus.runtime.configuration.model;

public class PortConfiguration {
    private int port;

    public PortConfiguration(String input) {
        this.port = validate(input);;
    }

    public int getPort() {
        return port;
    }

    public static int validate(String input) {

        if (input.isEmpty()) {
            throw new IllegalArgumentException("Port cannot be empty.");
        }

        int portValue;

        try {
            portValue = Integer.parseInt(input);
        }
        catch (NumberFormatException e) {
            throw new IllegalArgumentException(
                    "Invalid Input: Must be a port number between 0 and 65535");
        }

        if( portValue < 0 || portValue > 65535 ) {
            throw new IllegalArgumentException(
                    "Invalid Port: Must be a port number between 0 and 65535. Port: " + portValue);
        }

        return portValue;
    }
}
