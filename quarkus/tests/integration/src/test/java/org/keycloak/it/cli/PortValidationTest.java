package org.keycloak.it.cli;

import io.quarkus.test.junit.main.Launch;
import io.quarkus.test.junit.main.LaunchResult;
import org.junit.jupiter.api.Test;
import org.keycloak.it.junit5.extension.CLIResult;
import org.keycloak.it.junit5.extension.CLITest;

import static org.keycloak.quarkus.runtime.cli.command.Main.CONFIG_FILE_LONG_NAME;

@CLITest
public class PortValidationTest {

    @Test
    @Launch({ "start-dev", "--http-port=foo" })
    void testInvalidMessageWhenPortNotNumber(LaunchResult result) {
        CLIResult cliResult = (CLIResult) result;
        cliResult.assertInvalidPort();
    }

    @Test
    @Launch({ "start-dev", "--http-port=" })
    void testEmptyPortFails(LaunchResult result) {
        CLIResult cliResult = (CLIResult) result;
        cliResult.assertError("Port cannot be empty.");
    }

    @Test
    @Launch({ "start-dev", "--http-port=999999" })
    void testInvalidMessageWhenPortNotInRange(LaunchResult result) {
        CLIResult cliResult = (CLIResult) result;
        cliResult.assertInvalidPort();
    }

    @Test
    @Launch({ "start-dev", "--http-port=8088" })
    void testValidPortGetsParsed(LaunchResult result) {
        CLIResult cliResult = (CLIResult) result;
        cliResult.assertStartedDevMode();
    }

    @Test
    @Launch({ CONFIG_FILE_LONG_NAME+"=src/test/resources/PortValidationTest/keycloak-invalid-port.conf","start-dev" })
    void testInvalidPortFromNonCliSource(LaunchResult result) {
        CLIResult cliResult = (CLIResult) result;
        cliResult.assertInvalidPortAtStartUp();
    }

    @Test
    @Launch({ CONFIG_FILE_LONG_NAME+"=src/test/resources/PortValidationTest/keycloak-empty-port.conf","start-dev" })
    void testEmptyPortFromNonCliSource(LaunchResult result) {
        CLIResult cliResult = (CLIResult) result;
        cliResult.assertError("The config property quarkus.http.port is defined as the empty String");
    }
}
