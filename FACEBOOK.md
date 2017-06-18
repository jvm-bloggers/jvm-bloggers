# Posting on Facebook

The application requires four parameters: identifier of the Facebook page, user access token and application's id and secret.

### Page id
A unique identifier of the Facebook page you would like to post on. Can be found with [findmyfbid.com](http://findmyfbid.com).

### Application id
The application id is required during generation of a user access token only.

It can be found on the application dashboard on [developers.facebook.com](https://developers.facebook.com/apps).

### Application secret
To get the application secret you have to be in admin role of a page on which you would like to publish posts.

Go to the application's dashboard. The secret is placed on the main page of it.

### Access token
To grab a user access token two calls have to be made to the Facebook Graph API:
* the first one for an authentication code:
`GET https://www.facebook.com/dialog/oauth?client_id={app-id}&redirect_uri={redirect-uri}&scope=manage_pages,publish_pages`
* the second one to change the code to access user token:
`GET https://graph.facebook.com/oauth/access_token?client_id={app-id}&redirect_uri={redirect-uri}&client_secret={app-secret}&code={code}`

`{redirect-uri}` can be anything based on *App Domain* defined on the application settings page.

Based on a user access token, a page access token is generated during startup of JVM Bloggers core.

### Secured requests
Keep in mind that call for a Page Access Token uses appsecret_proof to protect the request. For more information about securing request look [here](https://developers.facebook.com/docs/graph-api/securing-requests).
