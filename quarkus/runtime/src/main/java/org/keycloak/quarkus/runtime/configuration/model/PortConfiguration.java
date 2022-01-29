package org.keycloak.quarkus.runtime.configuration.model;

public class PortConfiguration {
    private int port;

    public PortConfiguration(int input) {
        this.port = input;
    }

    public int getPort() {
        return port;
    }
}
