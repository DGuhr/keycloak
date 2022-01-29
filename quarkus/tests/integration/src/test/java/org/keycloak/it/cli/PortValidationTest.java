package org.keycloak.it.cli;

import io.quarkus.test.junit.main.Launch;
import io.quarkus.test.junit.main.LaunchResult;
import org.junit.jupiter.api.Test;
import org.keycloak.it.junit5.extension.CLIResult;
import org.keycloak.it.junit5.extension.CLITest;

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
}
