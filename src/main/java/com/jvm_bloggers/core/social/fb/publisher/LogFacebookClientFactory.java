package com.jvm_bloggers.core.social.fb.publisher;

import com.restfb.BinaryAttachment;
import com.restfb.Connection;
import com.restfb.DefaultJsonMapper;
import com.restfb.FacebookClient;
import com.restfb.JsonMapper;
import com.restfb.Parameter;
import com.restfb.WebRequestor;
import com.restfb.batch.BatchRequest;
import com.restfb.batch.BatchResponse;
import com.restfb.exception.devicetoken.FacebookDeviceTokenCodeExpiredException;
import com.restfb.exception.devicetoken.FacebookDeviceTokenDeclinedException;
import com.restfb.exception.devicetoken.FacebookDeviceTokenPendingException;
import com.restfb.exception.devicetoken.FacebookDeviceTokenSlowdownException;
import com.restfb.scope.ScopeBuilder;
import com.restfb.types.DeviceCode;
import lombok.extern.slf4j.Slf4j;
import org.springframework.context.annotation.Profile;
import org.springframework.stereotype.Component;

import java.util.List;
import java.util.Map;

import static com.jvm_bloggers.ApplicationProfiles.DEV;
import static com.jvm_bloggers.ApplicationProfiles.TEST;

@Slf4j
@Component
@Profile({DEV, TEST})
class LogFacebookClientFactory implements FacebookClientFactory {

    @Override
    public FacebookClient createFacebookClient() {
        return new FacebookClient() {
            private final JsonMapper jsonMapper = new DefaultJsonMapper();

            @Override
            public <T> T fetchObject(String object, Class<T> objectType, Parameter... parameters) {
                return null;
            }

            @Override
            public <T> T fetchObjects(List<String> ids, Class<T> objectType, Parameter... parameters) {
                return null;
            }

            @Override
            public <T> Connection<T> fetchConnection(String connection, Class<T> connectionType, Parameter... parameters) {
                return null;
            }

            @Override
            public <T> Connection<T> fetchConnectionPage(String connectionPageUrl, Class<T> connectionType) {
                return null;
            }

            @Override
            public <T> List<T> executeQuery(String query, Class<T> objectType, Parameter... parameters) {
                return null;
            }

            @Override
            public <T> T executeMultiquery(Map<String, String> queries, Class<T> objectType, Parameter... parameters) {
                return null;
            }

            @Override
            public <T> List<T> executeFqlQuery(String query, Class<T> objectType, Parameter... parameters) {
                return null;
            }

            @Override
            public <T> T executeFqlMultiquery(Map<String, String> queries, Class<T> objectType, Parameter... parameters) {
                return null;
            }

            @Override
            public List<BatchResponse> executeBatch(BatchRequest... batchRequests) {
                return null;
            }

            @Override
            public List<BatchResponse> executeBatch(List<BatchRequest> batchRequests) {
                return null;
            }

            @Override
            public List<BatchResponse> executeBatch(List<BatchRequest> batchRequests, List<BinaryAttachment> binaryAttachments) {
                return null;
            }

            @Override
            public <T> T publish(String connection, Class<T> objectType, Parameter... parameters) {
                return jsonMapper.toJavaObject("published", objectType);
            }

            @Override
            public <T> T publish(String connection, Class<T> objectType, List<BinaryAttachment> binaryAttachments, Parameter... parameters) {
                return jsonMapper.toJavaObject("published", objectType);
            }

            @Override
            public <T> T publish(String connection, Class<T> objectType, BinaryAttachment binaryAttachment, Parameter... parameters) {
                return jsonMapper.toJavaObject("published", objectType);
            }

            @Override
            public boolean deleteObject(String object, Parameter... parameters) {
                return false;
            }

            @Override
            public List<AccessToken> convertSessionKeysToAccessTokens(String appId, String secretKey, String... sessionKeys) {
                return null;
            }

            @Override
            public AccessToken obtainUserAccessToken(String appId, String appSecret, String redirectUri, String verificationCode) {
                return null;
            }

            @Override
            public AccessToken obtainAppAccessToken(String appId, String appSecret) {
                return null;
            }

            @Override
            public AccessToken obtainExtendedAccessToken(String appId, String appSecret, String accessToken) {
                return null;
            }

            @Override
            public String obtainAppSecretProof(String accessToken, String appSecret) {
                return null;
            }

            @Override
            public AccessToken obtainExtendedAccessToken(String appId, String appSecret) {
                return null;
            }

            @Override
            public <T> T parseSignedRequest(String signedRequest, String appSecret, Class<T> objectType) {
                return null;
            }

            @Override
            public DeviceCode fetchDeviceCode(String appId, ScopeBuilder scope) {
                return null;
            }

            @Override
            public AccessToken obtainDeviceAccessToken(String appId, String code) throws FacebookDeviceTokenCodeExpiredException, FacebookDeviceTokenPendingException, FacebookDeviceTokenDeclinedException, FacebookDeviceTokenSlowdownException {
                return null;
            }

            @Override
            public DebugTokenInfo debugToken(String inputToken) {
                return null;
            }

            @Override
            public JsonMapper getJsonMapper() {
                return null;
            }

            @Override
            public WebRequestor getWebRequestor() {
                return null;
            }

            @Override
            public String getLogoutUrl(String next) {
                return null;
            }

            @Override
            public String getLoginDialogUrl(String appId, String redirectUri, ScopeBuilder scope, Parameter... additionalParameters) {
                return null;
            }
        };
    }
}
