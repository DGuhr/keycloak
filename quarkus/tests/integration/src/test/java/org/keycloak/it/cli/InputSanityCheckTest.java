package org.keycloak.it.cli;

import io.quarkus.test.junit.main.Launch;
import io.quarkus.test.junit.main.LaunchResult;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.keycloak.it.junit5.extension.CLIResult;
import org.keycloak.it.junit5.extension.CLITest;
import org.keycloak.it.junit5.extension.DistributionTest;
import org.keycloak.quarkus.runtime.cli.command.Start;
import org.keycloak.quarkus.runtime.cli.command.StartDev;
import org.keycloak.quarkus.runtime.configuration.mappers.PropertyMappers;

import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Tests in this class should check that "parseArgs" used in picocli works
 * as intended, e.g. when using whitespaces, tabs or other input.
 */
@CLITest
public class InputSanityCheckTest {

    @Test
    @Launch({ Start.NAME, "--auto-build", "--db=mariadb", "--db-password=secret", "--https-key-store-password  secret"})
    void testMultipleSpacesReturnCliValidationError(LaunchResult result) {

        String expectedOutput = "Unknown Option(s) detected: [--https-key-store-password]." + System.lineSeparator() +
                "Please check your CLI Input, it has to be in the format --<option>=<value> or --<option> <value>." + System.lineSeparator() +
                "Multiple Spaces and Tabs are not allowed." + System.lineSeparator() +
                "Your Input: [--db=mariadb, --db-password=*******, --https-key-store-password=*******]";

        CLIResult cliResult = (CLIResult) result;
        Assertions.assertEquals(expectedOutput, cliResult.getErrorOutput());
    }

    @Test
    @Launch({ StartDev.NAME, "--db=mariadb", "--db-password=secret", "--https-key-store-password "})
    void testSpaceSeparatorWithoutValueShowsRightErrorMessage(LaunchResult result) {
        CLIResult cliResult = (CLIResult) result;
        cliResult.assertError("Unknown option: '--https-key-store-password '");
    }

    @Test
    @Launch({StartDev.NAME, "--db=mariadb", "--db-password=secret", "--https-key-store-password="})
    void testEqualsSeparatorWithoutValueShowsRightErrorMessage(LaunchResult result) {
        CLIResult cliResult = (CLIResult) result;
        cliResult.assertError("Missing required value for option '--https-key-store-password' (password).");
    }

    @Test
    @Launch({StartDev.NAME, "--db=mariadb", "--db-password=secret", "--https-key-store-password=  "})
    void testEqualsSeparatorWithWhiteSpaceOnlyValueShowsRightErrorMessage(LaunchResult result) {
        CLIResult cliResult = (CLIResult) result;
        cliResult.assertError("Missing required value for option '--https-key-store-password' (password).");
    }
}
