package org.keycloak.testsuite.url;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.impl.client.HttpClientBuilder;
import org.jboss.arquillian.container.test.api.ContainerController;
import org.jboss.arquillian.test.api.ArquillianResource;
import org.junit.Test;
import org.keycloak.admin.client.Keycloak;
import org.keycloak.broker.provider.util.SimpleHttp;
import org.keycloak.client.registration.Auth;
import org.keycloak.client.registration.ClientRegistration;
import org.keycloak.client.registration.ClientRegistrationException;
import org.keycloak.common.util.UriUtils;
import org.keycloak.jose.jws.JWSInput;
import org.keycloak.jose.jws.JWSInputException;
import org.keycloak.models.BrowserSecurityHeaders;
import org.keycloak.protocol.oidc.representations.OIDCConfigurationRepresentation;
import org.keycloak.representations.AccessToken;
import org.keycloak.representations.JsonWebToken;
import org.keycloak.representations.idm.ClientInitialAccessCreatePresentation;
import org.keycloak.representations.idm.ClientInitialAccessPresentation;
import org.keycloak.representations.idm.ClientRepresentation;
import org.keycloak.representations.idm.RealmRepresentation;
import org.keycloak.testsuite.arquillian.annotation.AuthServerContainerExclude;
import org.keycloak.testsuite.util.AdminClientUtil;
import org.keycloak.testsuite.util.ClientBuilder;
import org.keycloak.testsuite.util.OAuthClient;
import org.keycloak.testsuite.util.RealmBuilder;
import org.keycloak.testsuite.util.UserBuilder;

import javax.ws.rs.core.UriBuilder;
import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.keycloak.testsuite.arquillian.annotation.AuthServerContainerExclude.AuthServer.REMOTE;
import static org.keycloak.testsuite.util.OAuthClient.AUTH_SERVER_ROOT;
import static org.keycloak.testsuite.util.ServerURLs.getAuthServerContextRoot;

@AuthServerContainerExclude(REMOTE)
public class NonStandardHttpsPortHostnameTest extends AbstractHostnameTest{

    @ArquillianResource
    protected ContainerController controller;

    private String expectedBackendUrl;
    private final int expectedNonStandardPort = 8543;

    private final String globalFrontEndUrl = "https://keycloak.127.0.0.1.nip.io:" + String.valueOf(expectedNonStandardPort) + "/custom";

    private final String realmFrontEndUrl = "https://my-realm.127.0.0.1.nip.io:" + String.valueOf(expectedNonStandardPort);

    @Override
    public void addTestRealms(List<RealmRepresentation> testRealms) {
        RealmRepresentation test = RealmBuilder.create().name("test")
                .client(ClientBuilder.create().name("direct-grant").clientId("direct-grant").enabled(true).secret("password").directAccessGrants())
                .user(UserBuilder.create().username("test-user@localhost").password("password"))
                .build();
        testRealms.add(test);

        RealmRepresentation customHostname = RealmBuilder.create().name("frontendUrl")
                .client(ClientBuilder.create().name("direct-grant").clientId("direct-grant").enabled(true).secret("password").directAccessGrants())
                .user(UserBuilder.create().username("test-user@localhost").password("password"))
                .attribute("frontendUrl", realmFrontEndUrl)
                .build();
        testRealms.add(customHostname);
    }

    @Test
    public void fixedFrontendUrl() throws Exception {
        expectedBackendUrl = transformUrlIfQuarkusServer(AUTH_SERVER_ROOT, expectedNonStandardPort);
        System.out.println("----- Expected backend URL: " + expectedBackendUrl);

        oauth.clientId("direct-grant");

        try (Keycloak testAdminClient = AdminClientUtil.createAdminClient(suiteContext.isAdapterCompatTesting(), getAuthServerContextRoot())) {
            assertWellKnown("test", expectedBackendUrl);

            System.out.println("----- Global Frontend URL: " + globalFrontEndUrl);
            configureDefault(transformUrlIfQuarkusServer(globalFrontEndUrl, expectedNonStandardPort,true), false, null, false);

            assertWellKnown("test", transformUrlIfQuarkusServer(globalFrontEndUrl, expectedNonStandardPort,true));
            assertTokenIssuer("test", transformUrlIfQuarkusServer(globalFrontEndUrl, expectedNonStandardPort,true));
            assertInitialAccessTokenFromMasterRealm(testAdminClient,"test", transformUrlIfQuarkusServer(globalFrontEndUrl, expectedNonStandardPort,true));
            assertBackendForcedToFrontendWithMatchingHostname("test", transformUrlIfQuarkusServer(globalFrontEndUrl, expectedNonStandardPort,true));

            assertAdminPage("master", globalFrontEndUrl, transformUrlIfQuarkusServer(globalFrontEndUrl, expectedNonStandardPort,true));

            assertWellKnown("frontendUrl", realmFrontEndUrl);
            assertTokenIssuer("frontendUrl", realmFrontEndUrl);
            assertInitialAccessTokenFromMasterRealm(testAdminClient,"frontendUrl", realmFrontEndUrl);
            assertBackendForcedToFrontendWithMatchingHostname("frontendUrl", realmFrontEndUrl);

            assertAdminPage("frontendUrl", realmFrontEndUrl, transformUrlIfQuarkusServer(realmFrontEndUrl, expectedNonStandardPort,true));
        } finally {
            System.out.println("----- CALLING RESET");
            reset(false);
        }
    }


    private void assertInitialAccessTokenFromMasterRealm(Keycloak testAdminClient, String realm, String expectedBaseUrl) throws JWSInputException, ClientRegistrationException {
        ClientInitialAccessCreatePresentation rep = new ClientInitialAccessCreatePresentation();
        rep.setCount(1);
        rep.setExpiration(10000);

        ClientInitialAccessPresentation initialAccess = testAdminClient.realm(realm).clientInitialAccess().create(rep);
        JsonWebToken token = new JWSInput(initialAccess.getToken()).readJsonContent(JsonWebToken.class);
        assertEquals(expectedBaseUrl + "/realms/" + realm, token.getIssuer());

        ClientRegistration clientReg = ClientRegistration.create().url(AUTH_SERVER_ROOT, realm).build();
        clientReg.auth(Auth.token(initialAccess.getToken()));

        ClientRepresentation client = new ClientRepresentation();
        client.setEnabled(true);
        ClientRepresentation response = clientReg.create(client);

        String registrationAccessToken = response.getRegistrationAccessToken();
        JsonWebToken registrationToken = new JWSInput(registrationAccessToken).readJsonContent(JsonWebToken.class);
        assertEquals(expectedBaseUrl + "/realms/" + realm, registrationToken.getIssuer());
    }

    private void assertTokenIssuer(String realm, String expectedBaseUrl) throws Exception {
        oauth.realm(realm);

        OAuthClient.AccessTokenResponse tokenResponse = oauth.doGrantAccessTokenRequest("password", "test-user@localhost", "password");

        AccessToken token = new JWSInput(tokenResponse.getAccessToken()).readJsonContent(AccessToken.class);
        assertEquals(expectedBaseUrl + "/realms/" + realm, token.getIssuer());

        String introspection = oauth.introspectAccessTokenWithClientCredential(oauth.getClientId(), "password", tokenResponse.getAccessToken());
        ObjectMapper objectMapper = new ObjectMapper();
        JsonNode introspectionNode = objectMapper.readTree(introspection);
        assertTrue(introspectionNode.get("active").asBoolean());
        assertEquals(expectedBaseUrl + "/realms/" + realm, introspectionNode.get("iss").asText());
    }

    private void assertWellKnown(String realm, String expectedFrontendUrl) {
        OIDCConfigurationRepresentation config = oauth.doWellKnownRequest(realm);
        System.out.println("----- Well Known Issuer: " + config.getIssuer());

        assertEquals(expectedFrontendUrl + "/realms/" + realm, config.getIssuer());
        System.out.println("----- Well Known Authz Endpoint: " + config.getAuthorizationEndpoint());

        assertEquals(expectedFrontendUrl + "/realms/" + realm + "/protocol/openid-connect/auth", config.getAuthorizationEndpoint());
        System.out.println("----- Well Known Token Endpoint: " + config.getTokenEndpoint());

        System.out.println("----- SLEEPY KITTY GOES TO SLEEP: " + config.getTokenEndpoint());

        try {
            System.out.println("Going to sleep for 10 seconds");
            Thread.sleep(10000); //try to login manually and see... sigh.
        } catch (InterruptedException e) {
            e.printStackTrace();
        }

        assertEquals(expectedBackendUrl + "/realms/" + realm + "/protocol/openid-connect/token", config.getTokenEndpoint());
        assertEquals(expectedBackendUrl + "/realms/" + realm + "/protocol/openid-connect/userinfo", config.getUserinfoEndpoint());
        assertEquals(expectedFrontendUrl + "/realms/" + realm + "/protocol/openid-connect/logout", config.getLogoutEndpoint());
        assertEquals(expectedBackendUrl + "/realms/" + realm + "/protocol/openid-connect/certs", config.getJwksUri());
        assertEquals(expectedFrontendUrl + "/realms/" + realm + "/protocol/openid-connect/login-status-iframe.html", config.getCheckSessionIframe());
        assertEquals(expectedBackendUrl + "/realms/" + realm + "/clients-registrations/openid-connect", config.getRegistrationEndpoint());
    }

    // Test backend is forced to frontend if the request hostname matches the frontend
    private void assertBackendForcedToFrontendWithMatchingHostname(String realm, String expectedFrontendUrl) throws URISyntaxException {
        String host = new URI(expectedFrontendUrl).getHost();

        // Scheme and port doesn't matter as we force based on hostname only, so using http and bind port as we can't make requests on configured frontend URL since reverse proxy is not available
        oauth.baseUrl("http://" + host + ":" + System.getProperty("auth.server.http.port") + "/auth");

        OIDCConfigurationRepresentation config = oauth.doWellKnownRequest(realm);

        assertEquals(expectedFrontendUrl + "/realms/" + realm, config.getIssuer());
        assertEquals(expectedFrontendUrl + "/realms/" + realm + "/protocol/openid-connect/auth", config.getAuthorizationEndpoint());
        assertEquals(expectedFrontendUrl + "/realms/" + realm + "/protocol/openid-connect/token", config.getTokenEndpoint());
        assertEquals(expectedFrontendUrl + "/realms/" + realm + "/protocol/openid-connect/userinfo", config.getUserinfoEndpoint());
        assertEquals(expectedFrontendUrl + "/realms/" + realm + "/protocol/openid-connect/logout", config.getLogoutEndpoint());
        assertEquals(expectedFrontendUrl + "/realms/" + realm + "/protocol/openid-connect/certs", config.getJwksUri());
        assertEquals(expectedFrontendUrl + "/realms/" + realm + "/protocol/openid-connect/login-status-iframe.html", config.getCheckSessionIframe());
        assertEquals(expectedFrontendUrl + "/realms/" + realm + "/clients-registrations/openid-connect", config.getRegistrationEndpoint());

        oauth.baseUrl(AUTH_SERVER_ROOT);
    }

    private void assertWelcomePage(String expectedAdminUrl) throws IOException {
        try (CloseableHttpClient client = HttpClientBuilder.create().build()) {
            SimpleHttp get = SimpleHttp.doGet(AUTH_SERVER_ROOT + "/", client);

            String welcomePage = get.asString();
            assertTrue(welcomePage.contains("<a href=\"" + expectedAdminUrl + "/admin/\">"));
        }
    }

    private void assertAdminPage(String realm, String expectedFrontendUrl, String expectedAdminUrl) throws IOException, URISyntaxException {
        try (CloseableHttpClient client = HttpClientBuilder.create().build()) {
            SimpleHttp get = SimpleHttp.doGet(AUTH_SERVER_ROOT + "/admin/" + realm + "/console/", client);

            SimpleHttp.Response response = get.asResponse();
            String indexPage = response.asString();

            assertTrue(indexPage.contains("authServerUrl = '" + expectedFrontendUrl +"'"));
            assertTrue(indexPage.contains("authUrl = '" + expectedAdminUrl +"'"));
            assertTrue(indexPage.contains("consoleBaseUrl = '" + new URI(expectedAdminUrl).getPath() +"/admin/" + realm + "/console/'"));
            assertTrue(indexPage.contains("resourceUrl = '" + new URI(expectedAdminUrl).getPath() +"/resources/"));

            String cspHeader = response.getFirstHeader(BrowserSecurityHeaders.CONTENT_SECURITY_POLICY.getHeaderName());

            if (expectedFrontendUrl.equalsIgnoreCase(expectedAdminUrl)) {
                assertEquals("frame-src 'self'; frame-ancestors 'self'; object-src 'none';", cspHeader);
            } else {
                assertEquals("frame-src " + UriUtils.getOrigin(expectedFrontendUrl) + "; frame-ancestors 'self'; object-src 'none';", cspHeader);
            }
        }
    }

    public String transformUrlIfQuarkusServer(String expectedUrl, int expectedPort) {
        return transformUrlIfQuarkusServer(expectedUrl, expectedPort,false);
    }

    public String transformUrlIfQuarkusServer(String expectedUrl, int expectedPort, boolean adminUrl) {
        if (suiteContext.getAuthServerInfo().isQuarkus()) {
            UriBuilder uriBuilder = UriBuilder.fromUri(expectedUrl).port(expectedPort);

            if (adminUrl) {
                // for quarkus, the path is set from the request. As we are not running behind a proxy, that means defaults to /auth.
                uriBuilder.replacePath("/auth");
            }

            return uriBuilder.build().toString();
        }

        return expectedUrl;
    }
}
