package org.keycloak.it.cli.dist;

import io.quarkus.test.junit.main.Launch;
import io.restassured.RestAssured;
import io.restassured.http.ContentType;
import io.restassured.parsing.Parser;
import io.restassured.response.Response;
import org.apache.http.HttpStatus;
import org.junit.jupiter.api.Test;
import org.keycloak.it.junit5.extension.DistributionTest;
import org.keycloak.it.junit5.extension.RawDistOnly;

import static io.restassured.RestAssured.given;
import static io.restassured.RestAssured.when;
import static org.hamcrest.Matchers.equalTo;

@DistributionTest(keepAlive = true)
@RawDistOnly(reason = "no docker https cert support yet.")
public class HostnameTest {

    @Test
    @Launch({ "start-dev", "--hostname=kc-quarkus.127.0.0.1.nip.io", "--https-port=8765", "--https-key-store-file=keycloak.jks" })
    void testHttpsWithoutProxySetsPort() {
        RestAssured.useRelaxedHTTPSValidation();
        when().get("https://kc-quarkus.127.0.0.1.nip.io:8765/realms/master/.well-known/openid-configuration").then().statusCode(HttpStatus.SC_OK);

        RestAssured.defaultParser = Parser.JSON;

        Response externalDiscoveryEndpointResponse = given().headers("Content-Type", ContentType.JSON, "Accept", ContentType.JSON).
                when().get("https://kc-quarkus.127.0.0.1.nip.io:8765/realms/master/.well-known/openid-configuration").
                then().contentType(ContentType.JSON).extract().response();
        externalDiscoveryEndpointResponse.then().body("issuer", equalTo("https://kc-quarkus.127.0.0.1.nip.io:8765/realms/master"));
        externalDiscoveryEndpointResponse.then().body("introspection_endpoint", equalTo("https://kc-quarkus.127.0.0.1.nip.io:8765/realms/master/protocol/openid-connect/token/introspect"));

        Response internalDiscoveryEndpointResponse = given().headers("Content-Type", ContentType.JSON, "Accept", ContentType.JSON).
                when().get("https://localhost:8765/realms/master/.well-known/openid-configuration").
                then().contentType(ContentType.JSON).extract().response();

        internalDiscoveryEndpointResponse.then().body("issuer", equalTo("https://kc-quarkus.127.0.0.1.nip.io:8765/realms/master"));
        internalDiscoveryEndpointResponse.then().body("introspection_endpoint", equalTo("https://localhost:8765/realms/master/protocol/openid-connect/token/introspect"));
    }

    @Test
    @Launch({ "start-dev", "--hostname=kc-quarkus.127.0.0.1.nip.io", "--https-port=8765", "--https-key-store-file=keycloak.jks", "--proxy=passthrough" })
    void testHttpsWithProxyUsesStandardFrontEndPort() {
        RestAssured.useRelaxedHTTPSValidation();
        when().get("https://kc-quarkus.127.0.0.1.nip.io:8765/realms/master/.well-known/openid-configuration").then().statusCode(HttpStatus.SC_OK);

        RestAssured.defaultParser = Parser.JSON;

        Response externalDiscoveryEndpointResponse = given().headers("Content-Type", ContentType.JSON, "Accept", ContentType.JSON).
                when().get("https://kc-quarkus.127.0.0.1.nip.io:8765/realms/master/.well-known/openid-configuration").
                then().contentType(ContentType.JSON).extract().response();
        externalDiscoveryEndpointResponse.then().body("issuer", equalTo("https://kc-quarkus.127.0.0.1.nip.io/realms/master"));
        externalDiscoveryEndpointResponse.then().body("introspection_endpoint", equalTo("https://kc-quarkus.127.0.0.1.nip.io/realms/master/protocol/openid-connect/token/introspect"));

        Response internalDiscoveryEndpointResponse = given().headers("Content-Type", ContentType.JSON, "Accept", ContentType.JSON).
                when().get("https://localhost:8765/realms/master/.well-known/openid-configuration").
                then().contentType(ContentType.JSON).extract().response();

        internalDiscoveryEndpointResponse.then().body("issuer", equalTo("https://kc-quarkus.127.0.0.1.nip.io/realms/master"));
        internalDiscoveryEndpointResponse.then().body("introspection_endpoint", equalTo("https://localhost:8765/realms/master/protocol/openid-connect/token/introspect"));
    }
}
