package org.keycloak.it.cli.dist;

import io.quarkus.test.junit.main.Launch;
import io.quarkus.test.junit.main.LaunchResult;

import org.junit.jupiter.api.Test;
import org.keycloak.it.junit5.extension.CLIResult;
import org.keycloak.it.junit5.extension.DistributionTest;
import static org.keycloak.it.junit5.extension.DistributionTest.DbContainer;

// useDbContainer adds the cliArgs in the testextension, atm only --db=... and --db-url=...
// bc. there we can change actual List of arguments provided to the dist.
// we can't set the jdbcUrl which is a dynamic string w/ random port from container.getJdbcUrl() in @Launch, bc. of static context.
// also this is not possible for @CLITest, bc. there we can only access launch.value() reading, but not writing to it w/o reflection.
//Maybe TODO: Think about providing pw/username/database., e.g. useDbContainer = { DbContainer.Postgres, "username", "password", "database", ...}
@DistributionTest(useDbContainer = DbContainer.POSTGRES)
class PostgresStartDevDistTest {

    @Test
    //atm pw/username have to match the one in extension. Should be set as mentioned in TODO above.
    @Launch({ "start-dev", "--db-username=testuser", "--db-password=testPassword" })
    void testDevModeWarning(LaunchResult result) {
        CLIResult cliResult = (CLIResult) result;
        cliResult.assertStartedDevMode();
    }
}
